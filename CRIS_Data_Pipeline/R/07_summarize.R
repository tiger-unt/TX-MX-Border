
# This function aggregates crash-level indicators for each Crash_ID.
# For each keyword (e.g., Cell_Phone, Speeding), it checks whether any unit involved
# in the crash had a matching Contributing Factor (CF) or Possible Contributing Factor (PCF).
# It creates three flags per keyword: CF, PCF, and CF_or_PCF.
# It also includes vehicle counts and intoxication flags.
summarize_contrib_flags <- function(df, keywords) {
  summary_df <- df %>% group_by(Crash_ID) %>% summarise(
    Num_Vehicles = sum(as.integer(Is_Vehicle), na.rm = TRUE),
    Pedestrian_Intoxicated = sum(as.integer(Pedestrian_Intoxicated), na.rm = TRUE) > 0,
    Vehicle_Intoxicated = sum(as.integer(Vehicle_Intoxicated), na.rm = TRUE) > 0,
    .groups = 'drop'
  )
  
  # Loop through each keyword to calculate CF, PCF, and CF_or_PCF flags
  for (keyword in keywords) {
    cf_col <- paste0(keyword, '_CF')
    pcf_col <- paste0(keyword, '_PCF')
    cf_or_pcf_col <- paste0(keyword, '_CF_or_PCF')
    
    # Calculate CF flag: TRUE if any unit had this CF
    cf_flag <- df %>% group_by(Crash_ID) %>% summarise(!!cf_col := sum(as.integer(.data[[cf_col]]), na.rm = TRUE) > 0, .groups='drop')
    
    # Calculate PCF flag: TRUE if any unit had this PCF
    pcf_flag <- df %>% group_by(Crash_ID) %>% summarise(!!pcf_col := sum(as.integer(.data[[pcf_col]]), na.rm = TRUE) > 0, .groups='drop')
    
    summary_df  %<>% left_join(cf_flag, by='Crash_ID') %>% left_join(pcf_flag, by='Crash_ID') %>%
      mutate(!!cf_or_pcf_col := .data[[cf_col]] | .data[[pcf_col]])
  }
  summary_df
}

build_summaries <- function(crashes, units, all_persons , lookup_path) {
  keywords <- c('Cell_Phone','Speeding','Intoxicated','PedFailedToYieldROWToVehicle','VehicleFailedToYieldROWToPed','Distracted_Driving','Inattention','Animal')
  
  # crash-level CF flags + unique factor list per crash
  contrib_factors_by_crash <- summarize_contrib_flags(units, keywords) %>%
    full_join(
      units %>% as.data.table() %>% select(Crash_ID, Contrib_Factr_1_ID, Contrib_Factr_2_ID, Contrib_Factr_3_ID, Contrib_Factr_P1_ID, Contrib_Factr_P2_ID) %>%
        melt(id='Crash_ID') %>% rename(Contrib_Factor = value) %>% select(Crash_ID, Contrib_Factor) %>%
        filter(Contrib_Factor != -1 & Contrib_Factor != 0) %>% distinct() %>% group_by(Crash_ID) %>%
        summarise(Contrib_Factor = paste(Contrib_Factor, collapse=', '), .groups='drop'),
      by = 'Crash_ID')
  
  # all contributing factors (CF and PCF) with strings
  all_contrib_facts <- units %>% as.data.table() %>%
    select(Crash_ID, Contrib_Factr_1_ID, Contrib_Factr_2_ID, Contrib_Factr_3_ID, Contrib_Factr_P1_ID, Contrib_Factr_P2_ID) %>%
    
    # Reshape from wide to long format: each row is one factor per crash
    melt(id='Crash_ID') %>% rename(Contrib_Factor = value) %>%
    mutate(Possible_Contrib_Factor = str_detect(variable, '_P'), Contrib_Factor_Type = ifelse(Possible_Contrib_Factor,'Possible','Normal')) %>%
    select(Crash_ID, Contrib_Factor, Contrib_Factor_Type) %>% distinct(Crash_ID, Contrib_Factor, Contrib_Factor_Type) %>%
    left_join(make_lookup_tb(lookup_path, 'CONTRIB_FACTR_ID', 'Contrib_Factor', 'Contrib_Factor_str'), by='Contrib_Factor') %>%
    mutate(Contrib_Factor_str = coalesce(Contrib_Factor_str, 'NA/UNKNOWN'))
  
  
  # Combine all contributing factors (CF and PCF) for each crash into a unified list.
  # It groups by Crash_ID and factor ID/description, removes duplicates, and labels all factors
  # as "Normal_or_Possible" to indicate they came from either CF or PCF columns.
  all_contrib_facts_comb <- all_contrib_facts %>% distinct(Crash_ID, Contrib_Factor, Contrib_Factor_str) %>% mutate(Contrib_Factor_Type = 'Normal_or_Possible')
  

  # seatbelt summary from persons
  # For each crash_id, calculating:
  # - Total number of people involved
  # - how many people DID NOT wear seatbelts and died
  # - how many people DID wear seatbelts and died
  # - how many people DID NOT wear seatbelts and survived
  # - how many people DID wear seatbelts and survived
  seatbelt_summary <- all_persons %>%
    mutate(
      Dead_Without_Seatbelt = as.integer((Prsn_Rest_ID == 8) & (Death_Cnt == 1)),
      Dead_With_Seatbelt = as.integer((Prsn_Rest_ID %in% c(1,2,3)) & (Death_Cnt == 1)),
      Alive_Without_Seatbelt = as.integer((Prsn_Rest_ID == 8) & (Death_Cnt == 0)),
      Alive_With_Seatbelt = as.integer((Prsn_Rest_ID %in% c(1,2,3)) & (Death_Cnt == 0))
    ) %>%
    group_by(Crash_ID) %>%
    summarise(
      Num_People = sum(!is.na(Prsn_Nbr)),
      Deaths_Without_Seatbelts = sum(Dead_Without_Seatbelt, na.rm = TRUE),
      Deaths_With_Seatbelts = sum(Dead_With_Seatbelt, na.rm = TRUE),
      Alives_Without_Seatbelts = sum(Alive_Without_Seatbelt, na.rm = TRUE),
      Alives_With_Seatbelts = sum(Alive_With_Seatbelt, na.rm = TRUE),
      Deaths_Alives_With_Without_Seatbelts = Deaths_Without_Seatbelts + Deaths_With_Seatbelts + Alives_Without_Seatbelts + Alives_With_Seatbelts,
      .groups = 'drop'
    )
  
  # final crashes join & enrichments
  # ------------------------------------------------------------------------------
  # This pipeline enriches the `crashes` dataset by joining multiple related tables
  # and computing aggregated metrics for analysis. The steps include:
  #
  # 1. Join contributing factors data (`contrib_factors_by_crash`) and replace all
  #    missing values in factor-related columns with 0.
  #
  # 2. Add seatbelt usage summary (`seatbelt_summary`) and fill missing counts with 0.
  #
  # 3. Aggregate person-level details from `all_persons`:
  #    - Count total people, seniors, children, and intoxicated individuals.
  #    - Create a concatenated list of person IDs for each crash.
  #    - Replace missing values with 0 for numeric fields.
  #
  # 4. Add a severity indicator for crashes involving injuries (levels 1 to 4).
  #
  # 5. Aggregate unit-level details from `units`:
  #    - Compute totals for pedestrians, pedalcyclists, and motorcyclists by injury severity.
  #    - Replace missing values with 0 and create flags for crash types (pedestrian, cyclist, motorcyclist).
  #
  # Overall, this code prepares a comprehensive crash-level dataset by merging
  # multiple sources and ensuring all numeric fields are clean (no NAs).
  # ------------------------------------------------------------------------------
  crashes2 <- crashes %>%
    
    # Join contributing factors
    left_join(contrib_factors_by_crash, by='Crash_ID') %>%
    mutate(across(c(Cell_Phone_CF,Cell_Phone_PCF,Cell_Phone_CF_or_PCF,Speeding_CF,Speeding_PCF,Speeding_CF_or_PCF,
                                  Intoxicated_CF,Intoxicated_PCF,Intoxicated_CF_or_PCF,PedFailedToYieldROWToVehicle_CF,PedFailedToYieldROWToVehicle_PCF,PedFailedToYieldROWToVehicle_CF_or_PCF,
                                  VehicleFailedToYieldROWToPed_CF,VehicleFailedToYieldROWToPed_PCF,VehicleFailedToYieldROWToPed_CF_or_PCF,
                                  Distracted_Driving_CF,Distracted_Driving_PCF,Distracted_Driving_CF_or_PCF,Inattention_CF,Inattention_PCF,Inattention_CF_or_PCF,
                                  Animal_CF,Animal_PCF,Animal_CF_or_PCF,Num_Vehicles,Pedestrian_Intoxicated,Vehicle_Intoxicated), ~ replace_na(., 0))) %>%
    
    # Pulling in the seatbelt data
    left_join(seatbelt_summary, by='Crash_ID') %>%
    mutate(across(c(Deaths_Without_Seatbelts,Deaths_With_Seatbelts,Alives_Without_Seatbelts,Alives_With_Seatbelts,Deaths_Alives_With_Without_Seatbelts), ~ replace_na(., 0))) %>%
    
    # Pulling in persons in each crash
    left_join(
      all_persons %>% select(Crash_ID, Prsn_ID, Age_Determined, Is_Senior, Is_Child, Is_Senior_Pedestrian, Is_Child_Pedestrian,
                                    Intoxicated_Person, Intoxicated_Pedestrian, Intoxicated_Pedalcyclist, Intoxicated_Motorcyclist, Intoxicated_Other) %>%
        group_by(Crash_ID) %>% summarise(
          Person_IDs = paste(Prsn_ID, collapse=', '),
          Num_People_from_Persons = n(),
          Num_People_from_Persons_Age_Determined = sum(as.integer(Age_Determined), na.rm = TRUE),
          Num_Senior = sum(coalesce(Is_Senior, FALSE)),
          Num_Child = sum(coalesce(Is_Child, FALSE)),
          Num_Senior_Pedestrian = sum(coalesce(Is_Senior_Pedestrian, FALSE)),
          Num_Child_Pedestrian = sum(coalesce(Is_Child_Pedestrian, FALSE)),
          Num_Intoxicated_Persons = sum(coalesce(Intoxicated_Person, FALSE)),
          Num_Intoxicated_Pedestrian = sum(coalesce(Intoxicated_Pedestrian, FALSE)),
          Num_Intoxicated_Pedalcyclist = sum(coalesce(Intoxicated_Pedalcyclist, FALSE)),
          Num_Intoxicated_Motorcyclist = sum(coalesce(Intoxicated_Motorcyclist, FALSE)),
          Num_Intoxicated_Other = sum(coalesce(Intoxicated_Other, FALSE)),
          .groups='drop'
        ), by='Crash_ID') %>%
    mutate(across(c(Num_People_from_Persons,Num_People_from_Persons_Age_Determined,Num_Senior,Num_Child,Num_Senior_Pedestrian,Num_Child_Pedestrian,
                                  Num_Intoxicated_Persons,Num_Intoxicated_Pedestrian,Num_Intoxicated_Pedalcyclist,Num_Intoxicated_Motorcyclist,Num_Intoxicated_Other), ~ replace_na(., 0)),
                  In_Person_Files = !is.na(Person_IDs)) %>%
    
    # Pulling in the "Severity Selector" from the person level
    left_join(
      all_persons %>% select(Crash_ID, Person_InjurySeverity_Selector) %>% filter(Person_InjurySeverity_Selector == TRUE) %>%
        group_by(Crash_ID) %>% summarise(Person_InjurySeverity_Selector = 1, .groups='drop'), by='Crash_ID') %>%
    mutate(Person_InjurySeverity_Selector = replace_na(Person_InjurySeverity_Selector, 0)) %>%
    
    # Pulling in the number of pedestrians, pedalcyclists and motorcyclists by injury severity level
    left_join(units %>% group_by(Crash_ID) %>%
                       summarise(across(c(Num_People_from_Units, Total_Number_of_Pedestrians, Pedestrians_InjSev_Killed = Pedestrians_InjSev_Killed, Pedestrians_InjSev_Incap = Pedestrians_InjSev_Incap,
                                                        Pedestrians_InjSev_NonIncap = Pedestrians_InjSev_NonIncap, Pedestrians_InjSev_PossInj = Pedestrians_InjSev_PossInj, Pedestrians_InjSev_NonInj = Pedestrians_InjSev_NonInj, Pedestrians_InjSev_Unknown = Pedestrians_InjSev_Unknown,
                                                        Total_Number_of_Pedalcyclists, Pedalcyclists_InjSev_Killed = Pedalcyclists_InjSev_Killed, Pedalcyclists_InjSev_Incap = Pedalcyclists_InjSev_Incap, Pedalcyclists_InjSev_NonIncap = Pedalcyclists_InjSev_NonIncap,
                                                        Pedalcyclists_InjSev_PossInj = Pedalcyclists_InjSev_PossInj, Pedalcyclists_InjSev_NonInj = Pedalcyclists_InjSev_NonInj, Pedalcyclists_InjSev_Unknown = Pedalcyclists_InjSev_Unknown,
                                                        Total_Number_of_Motorcyclists, Motorcyclist_InjSev_Killed, Motorcyclist_InjSev_Incap, Motorcyclist_InjSev_NonIncap, Motorcyclist_InjSev_PossInj, Motorcyclist_InjSev_NonInj, Motorcyclist_InjSev_Unknown), ~ sum(replace_na(.,0))), .groups='drop'), by='Crash_ID') %>%
    mutate(
      across(c(Num_People_from_Units, starts_with('Total_Number_of_'), ends_with('_InjSev_Killed'), ends_with('_InjSev_Incap'), ends_with('_InjSev_NonIncap'), ends_with('_InjSev_PossInj'), ends_with('_InjSev_NonInj'), ends_with('_InjSev_Unknown')),
                    ~ replace_na(., 0)),
      Pedestrian_Crash = Total_Number_of_Pedestrians > 0,
      Pedalcyclist_Crash = Total_Number_of_Pedalcyclists > 0,
      Motorcyclist_Crash = Total_Number_of_Motorcyclists > 0
    ) %>%
    left_join(units %>% group_by(Crash_ID) %>% summarise(Num_Units = n(), POV_Flag = sum(as.integer(POV_Flag), na.rm=TRUE) > 0,
                                                                              Animal_CMV = sum(as.integer(Animal_CMV), na.rm=TRUE) > 0, Unintended_Pedestrian = sum(as.integer(Unintended_Pedestrian), na.rm=TRUE) > 0, .groups='drop'), by='Crash_ID') %>%
    mutate(Animal_All = (Animal_CMV | Animal_CF | Animal_PCF | (Harm_Evnt_ID == 6) | (Othr_Factr_ID %in% c(29,40))),
                  Intoxicated_Any = (Intoxicated_CF | Intoxicated_PCF | Num_Intoxicated_Persons > 0))
  

  # Getting statistics for each contributing factor
  # ------------------------------------------------------------------------------
  # This code block creates a dataset (`contrib_factors_with_extra_data`) that links
  # each crash to its contributing factors and enriches it with crash-level details.
  # The result is a comprehensive table for analyzing contributing factors alongside
  # crash characteristics and severity metrics.
  # ------------------------------------------------------------------------------
  contrib_factors_with_extra_data <- units %>% as.data.table() %>% select(Crash_ID, Contrib_Factr_1_ID, Contrib_Factr_2_ID, Contrib_Factr_3_ID, Contrib_Factr_P1_ID, Contrib_Factr_P2_ID) %>%
    melt(id.vars='Crash_ID') %>% mutate(Possible_Contrib_Factor = str_detect(variable,'_P')) %>% select(-variable) %>% rename(Contrib_Factor = value) %>%
    distinct(Crash_ID, Contrib_Factor) %>%
    left_join(crashes2 %>% select(Crash_ID, Crash_Year, Pedestrian_Crash, Pedalcyclist_Crash, Motorcyclist_Crash, Num_Units, Num_People_from_Crash, Death_Cnt, Sus_Serious_Injry_Cnt, Nonincap_Injry_Cnt,
                                                Pedestrians_InjSev_Killed, Pedestrians_InjSev_Incap, Pedestrians_InjSev_NonIncap, Pedestrians_InjSev_PossInj, Pedestrians_InjSev_NonInj, Pedestrians_InjSev_Unknown,
                                                Pedalcyclists_InjSev_Killed, Pedalcyclists_InjSev_Incap, Pedalcyclists_InjSev_NonIncap, Pedalcyclists_InjSev_PossInj, Pedalcyclists_InjSev_NonInj, Pedalcyclists_InjSev_Unknown,
                                                Motorcyclist_InjSev_Killed, Motorcyclist_InjSev_Incap, Motorcyclist_InjSev_NonIncap, Motorcyclist_InjSev_PossInj, Motorcyclist_InjSev_NonInj, Motorcyclist_InjSev_Unknown,
                                                Txdot_Rptable_Fl, Onsys_Fl, Located_Fl, Rural_Urban_simp, CNTY_NAME, TXDOT_CNTY_NBR, TXDOT_DIST_NBR, TXDOT_DIST_ABRVN, TXDOT_DIST_NAME), by='Crash_ID') %>%
    mutate(Num_Fatalities = Death_Cnt, Num_Serious_Injuries = Sus_Serious_Injry_Cnt, Num_NonIncap_Injuries = Nonincap_Injry_Cnt) %>%
    left_join(all_contrib_facts %>% distinct(Contrib_Factor, Contrib_Factor_str), by='Contrib_Factor')
  
  
  # ------------------------------------------------------------------------------
  # This block aggregates crash statistics by contributing factor and year,
  # calculates totals for crashes, units, people, injuries, and fatalities,
  # and computes the share of fatal crashes for each factor.
  # ------------------------------------------------------------------------------
  totals_by_contrib_factor_per_year <- contrib_factors_with_extra_data %>% group_by(Contrib_Factor, Crash_Year) %>% summarise(
    Num_Crashes = length(unique(Crash_ID)), Num_Units = sum(Num_Units, na.rm=TRUE), Num_People = sum(Num_People_from_Crash, na.rm=TRUE),
    Num_Fatalities = sum(Num_Fatalities, na.rm=TRUE), Num_Serious_Injuries = sum(Num_Serious_Injuries, na.rm=TRUE), Num_NonIncap_Injuries = sum(Num_NonIncap_Injuries, na.rm=TRUE),
    Num_KAB_Inj = Num_Fatalities + Num_Serious_Injuries + Num_NonIncap_Injuries, .groups='drop') %>%
    left_join(contrib_factors_with_extra_data %>% filter(Num_Fatalities > 0) %>% group_by(Contrib_Factor, Crash_Year) %>% summarise(Num_Fatal_Crashes = length(unique(Crash_ID)), .groups='drop'), by=c('Contrib_Factor','Crash_Year')) %>%
    mutate(Num_Fatal_Crashes = if_else(is.na(Num_Fatal_Crashes), 0L, Num_Fatal_Crashes), Share_of_Fatal_Crashes = round(Num_Fatal_Crashes/Num_Crashes, 3)) %>%
    left_join(all_contrib_facts %>% distinct(Contrib_Factor, Contrib_Factor_str), by='Contrib_Factor')
  
  list(
    crashes = crashes2,
    all_contrib_facts = all_contrib_facts,
    all_contrib_facts_comb = all_contrib_facts_comb,
    contrib_factors_by_crash = contrib_factors_by_crash,
    contrib_factors_with_extra_data = contrib_factors_with_extra_data,
    totals_by_contrib_factor_per_year = totals_by_contrib_factor_per_year
  )
}