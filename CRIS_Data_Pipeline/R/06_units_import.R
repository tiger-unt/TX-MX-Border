
# Tag CF/PCF columns for whether they match a keyword group defined in CONTRIB_FACTR_ID.csv
# The CSV is expected to have columns: CONTRIB_FACTR_ID, Factor_Group (and optionally labels)


# Memoized function to read the lookup file
read_contrib_lookup <- memoise(function(contrib_lookup_path) {
  fread(contrib_lookup_path) %>%
    as_tibble()
})

is_contrib_related <- function(df, keyword, contrib_lookup_path) {
  lookup_df <- read_contrib_lookup(contrib_lookup_path)  # Cached read
  relevant_ids <- lookup_df %>% filter(str_detect(Factor_Group, keyword)) %>% pull(CONTRIB_FACTR_ID)

  cf_cols  <- c('Contrib_Factr_1_ID','Contrib_Factr_2_ID','Contrib_Factr_3_ID')
  pcf_cols <- c('Contrib_Factr_P1_ID','Contrib_Factr_P2_ID')

  cf_match <- df %>% mutate(match = if_any(all_of(cf_cols), ~ .x %in% relevant_ids)) %>% pull(match)
  pcf_match <- df %>% mutate(match = if_any(all_of(pcf_cols), ~ .x %in% relevant_ids)) %>% pull(match)

  df[[paste0(keyword, '_CF')]] <- cf_match
  df[[paste0(keyword, '_PCF')]] <- pcf_match
  df[[paste0(keyword, '_CF_or_PCF')]] <- cf_match | pcf_match
  df
}

units_import <- function(unit_path, lookup_path, contrib_lookup_path, crash_id_keep, all_persons) {
  units <- fread(unit_path) %>% as_tibble() %>%
    filter(Crash_ID %in% crash_id_keep) %>%
    unite(Unit_ID, Crash_ID:Unit_Nbr, sep = '_', remove = FALSE, na.rm = FALSE) %>%
    
    # Replacing NA in contributing factor and commercial motor vehicle event ID columns with -1
    # Contrib_Factr (1,2 & 3) = The factor for the vehicle  which the officer felt contributed to the crash
    # Contrib_Factr (P1 & P2) = The factor for a given vehicle  which the officer felt possibly contributed to the crash
    # Cmv_Evnt = CMV Sequence of Events
    mutate(
      Contrib_Factr_1_ID = replace_na(Contrib_Factr_1_ID, -1),
      Contrib_Factr_2_ID = replace_na(Contrib_Factr_2_ID, -1),
      Contrib_Factr_3_ID = replace_na(Contrib_Factr_3_ID, -1),
      Contrib_Factr_P1_ID = replace_na(Contrib_Factr_P1_ID, -1),
      Contrib_Factr_P2_ID = replace_na(Contrib_Factr_P2_ID, -1),
      Cmv_Evnt1_ID = replace_na(Cmv_Evnt1_ID, -1),
      Cmv_Evnt2_ID = replace_na(Cmv_Evnt2_ID, -1),
      Cmv_Evnt3_ID = replace_na(Cmv_Evnt3_ID, -1),
      Cmv_Evnt4_ID = replace_na(Cmv_Evnt4_ID, -1)
    ) %>%
    
    # Creating flags for Vehicle, Pedestrian and Pedalcyclist
    mutate(
      Is_Vehicle = Unit_Desc_ID %in% c(1,5,6), # 1 = MOTOR VEHICLE; 5 = MOTORIZED CONVEYANCE ; 6 = TOWED/TRAILER
      Is_Pedestrian = (Unit_Desc_ID == 4),
      Is_Pedalcyclist = (Unit_Desc_ID == 3),
      
      # Flagging Passenger Vehicles
      # 30 = PICKUP, 69 = SPORT UTILITY VEHICLE,  100 = PASSENGER CAR(2-DOOR),  103 = VAN,  104 = PASSENGER CAR(4-DOOR)
      POV_Flag = Veh_Body_Styl_ID %in% c(30,69,100,103,104)
    )

  # CF/PCF taggers -  Contribution Factor
  for (kw in c('Cell_Phone','Speeding','Intoxicated','PedFailedToYieldROWToVehicle','VehicleFailedToYieldROWToPed','Distracted_Driving','Animal')) {
    units <- is_contrib_related(units, kw, contrib_lookup_path)
  }
  # Inattention composed from Distracted + Cell Phone
  units <- units %>% mutate(
    Inattention_CF = Distracted_Driving_CF | Cell_Phone_CF,
    Inattention_PCF = Distracted_Driving_PCF | Cell_Phone_PCF,
    Inattention_CF_or_PCF = Inattention_CF | Inattention_PCF
  )

  # Intoxication flags at unit level
  units <- units %>% mutate(
    Pedestrian_Intoxicated = (Is_Pedestrian & (Intoxicated_CF | Intoxicated_PCF)),
    Vehicle_Intoxicated = (Is_Vehicle & (Intoxicated_CF | Intoxicated_PCF))
  )

  # Animal involved via CMV
  units <- units %>% mutate(Animal_CMV = if_any(c(Cmv_Evnt1_ID,Cmv_Evnt2_ID,Cmv_Evnt3_ID,Cmv_Evnt4_ID), ~ .x == 17)) # 17 = COLLISION INVOLVING ANIMAL

  # People counts by unit
  units <- units %>% mutate(Num_People_from_Units = (Death_Cnt + Sus_Serious_Injry_Cnt + Nonincap_Injry_Cnt + Poss_Injry_Cnt + Non_Injry_Cnt + Unkn_Injry_Cnt))

  # Pedestrian & Pedalcyclist counts by inj-sev
  add_counts <- function(df, flag_col, prefix) {
    df %>% mutate(
      !!paste0('Total_Number_of_', prefix) := as.integer(.data[[flag_col]]) * Num_People_from_Units,
      !!paste0(prefix, '_InjSev_Killed') := as.integer(.data[[flag_col]]) * Death_Cnt,
      !!paste0(prefix, '_InjSev_Incap') := as.integer(.data[[flag_col]]) * Sus_Serious_Injry_Cnt,
      !!paste0(prefix, '_InjSev_NonIncap') := as.integer(.data[[flag_col]]) * Nonincap_Injry_Cnt,
      !!paste0(prefix, '_InjSev_PossInj') := as.integer(.data[[flag_col]]) * Poss_Injry_Cnt,
      !!paste0(prefix, '_InjSev_NonInj') := as.integer(.data[[flag_col]]) * Non_Injry_Cnt,
      !!paste0(prefix, '_InjSev_Unknown') := as.integer(.data[[flag_col]]) * Unkn_Injry_Cnt
    )
  }
  units <- units %>% add_counts('Is_Pedestrian','Pedestrians') %>% add_counts('Is_Pedalcyclist','Pedalcyclists')

  # PBCAT & Action lookups (via master lookup)
  units <- units %>%
    mutate(PBCAT_Pedestrian_ID = replace_na(PBCAT_Pedestrian_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'PBCAT_PEDESTRIAN_ID','PBCAT_Pedestrian_ID','PBCAT_Pedestrian_ID_str'), by='PBCAT_Pedestrian_ID') %>%
    mutate(PBCAT_Pedestrian_ID_str = coalesce(PBCAT_Pedestrian_ID_str,'NA/UNKNOWN')) %>%
    mutate(Pedestrian_Action_ID = replace_na(Pedestrian_Action_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'PEDESTRIAN_ACTION_ID','Pedestrian_Action_ID','Pedestrian_Action_ID_str'), by='Pedestrian_Action_ID') %>%
    mutate(Pedestrian_Action_ID_str = coalesce(Pedestrian_Action_ID_str,'NA/UNKNOWN')) %>%
    mutate(PBCAT_Pedalcyclist_ID = replace_na(PBCAT_Pedalcyclist_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'PBCAT_PEDALCYCLIST_ID','PBCAT_Pedalcyclist_ID','PBCAT_Pedalcyclist_ID_str'), by='PBCAT_Pedalcyclist_ID') %>%
    mutate(PBCAT_Pedalcyclist_ID_str = coalesce(PBCAT_Pedalcyclist_ID_str,'NA/UNKNOWN')) %>%
    mutate(Pedalcyclist_Action_ID = replace_na(Pedalcyclist_Action_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'PEDALCYCLIST_ACTION_ID','Pedalcyclist_Action_ID','Pedalcyclist_Action_ID_str'), by='Pedalcyclist_Action_ID') %>%
    mutate(Pedalcyclist_Action_ID_str = coalesce(Pedalcyclist_Action_ID_str,'NA/UNKNOWN'))

  # Unintended pedestrian flag
  # Flagging pedestrians related to "PUSHING OR WORKING ON VEHICLE IN ROADWAY OR SHOULDER" or "DISABLED VEHICLE-RELATED"
  units <- units %>% mutate(Unintended_Pedestrian = ((Pedestrian_Action_ID == 11) | (PBCAT_Pedestrian_ID == 12)))

  # Augment with motorcyclist flags using persons (by unit)
  units <- units %>% left_join(
    all_persons %>% group_by(Unit_ID) %>% summarise(
      Num_People_from_Persons = n(),
      Number_Motorcyclists = sum(as.integer(Is_Motorcyclist), na.rm = TRUE),
      Is_Motorcyclist = Number_Motorcyclists > 0,
      .groups='drop'
    ), by='Unit_ID') %>%
    mutate(
      Num_People_from_Persons = coalesce(Num_People_from_Persons, 0L),
      Is_Motorcyclist = coalesce(Is_Motorcyclist, FALSE),
      Total_Number_of_Motorcyclists = as.integer(Is_Motorcyclist) * Num_People_from_Units,
      Motorcyclist_InjSev_Killed = as.integer(Is_Motorcyclist) * Death_Cnt,
      Motorcyclist_InjSev_Incap = as.integer(Is_Motorcyclist) * Sus_Serious_Injry_Cnt,
      Motorcyclist_InjSev_NonIncap = as.integer(Is_Motorcyclist) * Nonincap_Injry_Cnt,
      Motorcyclist_InjSev_PossInj = as.integer(Is_Motorcyclist) * Poss_Injry_Cnt,
      Motorcyclist_InjSev_NonInj = as.integer(Is_Motorcyclist) * Non_Injry_Cnt,
      Motorcyclist_InjSev_Unknown = as.integer(Is_Motorcyclist) * Unkn_Injry_Cnt
    )

  units
}
