all_persons_import <- function(person_path, primary_person_path, lookup_path, crash_id_keep) {
  # persons
  person_data <- fread(person_path) %>% as_tibble() %>%
    filter(Crash_ID %in% crash_id_keep) %>%
    unite(Prsn_ID, Crash_ID:Prsn_Nbr, sep = '_', remove = FALSE, na.rm = FALSE) %>%
    unite(Unit_ID, Crash_ID:Unit_Nbr, sep = '_', remove = FALSE, na.rm = FALSE) %>%
    mutate(In_Person_File = 1)

  # primary persons
  primary_person_data <- fread(primary_person_path) %>% as_tibble() %>%
    filter(Crash_ID %in% crash_id_keep) %>%
    unite(Prsn_ID, Crash_ID:Prsn_Nbr, sep = '_', remove = FALSE, na.rm = FALSE) %>%
    unite(Unit_ID, Crash_ID:Unit_Nbr, sep = '_', remove = FALSE, na.rm = FALSE) %>%
    mutate(In_Primary_Person_File = 1)

  ### Combining "Primary Person" and "Person" files. No need to worry about deduplicating people across the files. They are all unique.
  all_persons <- bind_rows(person_data, primary_person_data)

  ### Lookup #1
  # Adds injury severity labels to person-level data using CRASH_SEV_ID lookup and maps them to KABCO codes.
  # Prsn_Injry_Sev_ID = Severity of injury to the occupant
  all_persons <- all_persons %>%
    left_join(make_lookup_tb(lookup_path, 'CRASH_SEV_ID', 'Prsn_Injry_Sev_ID', 'Prsn_Injry_Sev_ID_str'), by='Prsn_Injry_Sev_ID') %>%
    mutate(
      Prsn_Injry_Sev_KABCO = recode(as.integer(Prsn_Injry_Sev_ID),
        `4`='K',`1`='A',`2`='B',`3`='C',`5`='O',`0`='U', .default='U'),
      Person_InjurySeverity_Selector = Prsn_Injry_Sev_ID %in% c(1,2,4),
      InjSev_Incap = as.integer(Prsn_Injry_Sev_ID == 1), # Incapacitated
      InjSev_NonIncap = as.integer(Prsn_Injry_Sev_ID == 2), # Not Incapacitated
      InjSev_PossInj = as.integer(Prsn_Injry_Sev_ID == 3), # Possible Injury
      InjSev_Killed = as.integer(Prsn_Injry_Sev_ID == 4), # Killed
      InjSev_NonInj = as.integer(Prsn_Injry_Sev_ID == 5), # No Injury
      InjSev_Unknown = as.integer(Prsn_Injry_Sev_ID == 0) # Injury Details Unknown
    )

  ### Lookup #2
  # Adds drug category descriptions to person-level data using DRUG_CATEGORY_ID lookup
  # Drvr_Drg_Cat_1_ID = First category of drugs related to the driver
  all_persons <- all_persons %>%
    mutate(Drvr_Drg_Cat_1_ID = replace_na(Drvr_Drg_Cat_1_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'DRUG_CATEGORY_ID','Drvr_Drg_Cat_1_ID','Drvr_Drg_Cat_1_ID_str'), by='Drvr_Drg_Cat_1_ID') %>%
    mutate(Drvr_Drg_Cat_1_ID_str = coalesce(Drvr_Drg_Cat_1_ID_str,'NA/UNKNOWN'))

  ### Lookup #3
  # Adds Alcohol & Drug Test Result descriptions to person-level data using TST_RESULT_ID lookup
  all_persons <- all_persons %>%
    mutate(Prsn_Alc_Rslt_ID = replace_na(Prsn_Alc_Rslt_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'TST_RESULT_ID','Prsn_Alc_Rslt_ID','Prsn_Alc_Rslt_ID_str'), by='Prsn_Alc_Rslt_ID') %>%
    mutate(Prsn_Alc_Rslt_ID_str = coalesce(Prsn_Alc_Rslt_ID_str,'NA/UNKNOWN')) %>%
    
    # Prsn_Drg_Rslt_ID = Drug Test Result
    mutate(Prsn_Drg_Rslt_ID = replace_na(Prsn_Drg_Rslt_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path,'TST_RESULT_ID','Prsn_Drg_Rslt_ID','Prsn_Drg_Rslt_ID_str'), by='Prsn_Drg_Rslt_ID') %>%
    mutate(Prsn_Drg_Rslt_ID_str = coalesce(Prsn_Drg_Rslt_ID_str,'NA/UNKNOWN')) %>%
    
    # Identify Intoxicated Person involved in crashes
    # Prsn_Bac_Test_Rslt = Numeric blood alcohol content test result for a primary person involved in the crash, using standardized alcohol breath results (i.e. .08 or .129)
    mutate(Prsn_Bac_Test_Rslt = replace_na(Prsn_Bac_Test_Rslt, -1),
                  Intoxicated_Person = ((Drvr_Drg_Cat_1_ID %in% c(2,3,4,6,7,8,10,11,12)) |
                                        (Prsn_Alc_Rslt_ID == 1) |
                                        (Prsn_Bac_Test_Rslt > 0) |
                                        (Prsn_Drg_Rslt_ID == 1)))

  ### Lookup #4
  # Joins person-level data with lookup descriptions for PRSN_TYPE_ID to add readable labels.
  # PRSN_TYPE_ID = Person Type
  all_persons <- all_persons %>%
    left_join(make_lookup_tb(lookup_path,'PRSN_TYPE_ID','Prsn_Type_ID','Prsn_Type_ID_str'), by='Prsn_Type_ID') %>%
    
    # Create binary flags for key person types: pedestrian, pedalcyclist, and motorcyclist
    mutate(Is_Pedestrian = (Prsn_Type_ID == 4),
                  Is_Pedalcyclist = (Prsn_Type_ID == 3),
                  Is_Motorcyclist = (Prsn_Type_ID %in% c(5,6))) %>%
    
    # Identify Intoxicated pedestrian, pedalcyclist, and motorcyclist
    mutate(Intoxicated_Pedestrian = Intoxicated_Person & Is_Pedestrian,
                  Intoxicated_Pedalcyclist = Intoxicated_Person & Is_Pedalcyclist,
                  Intoxicated_Motorcyclist = Intoxicated_Person & Is_Motorcyclist,
                  Intoxicated_Other = Intoxicated_Person & !(Is_Pedestrian | Is_Pedalcyclist | Is_Motorcyclist)) %>%
    
    # Adds age-based flags to person-level data to identify seniors, children, and their pedestrian status
    # Prsn_Age = Age of person involved in the crash
    mutate(Is_Senior = Prsn_Age >= 65,
                  Is_Child = Prsn_Age <= 16,
                  Is_Senior_Pedestrian = Is_Senior & Is_Pedestrian,
                  Is_Child_Pedestrian = Is_Child & Is_Pedestrian,
                  Age_Determined = !is.na(Prsn_Age))

  all_persons
}
