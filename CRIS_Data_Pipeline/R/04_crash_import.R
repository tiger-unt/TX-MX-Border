
crash_import <- function(crash_path, lookup_path, cnty_data, crash_id_keep) {
  # helper to create HWY strings
  make_hwy <- function(sys, nbr, sfx) {
    x <- str_c(coalesce(sys, ''), coalesce(nbr, ''), coalesce(sfx, ''), sep = '')
    na_if(x, '')
  }

  crashes <- fread(crash_path) %>%
    as_tibble() %>%
    filter(Crash_ID %in% crash_id_keep) %>%
    mutate(
      HWY = make_hwy(Hwy_Sys, Hwy_Nbr, Hwy_Sfx), # Construct primary highway identifier
      HWY_2 = make_hwy(Hwy_Sys_2, Hwy_Nbr_2, Hwy_Sfx_2), # Construct secondary highway identifier
      
      # Convert crash date to Date format and extract year
      Crash_Date_dt = mdy(Crash_Date),
      Crash_Year = year(Crash_Date_dt),
      Crash_Year_int = as.integer(Crash_Year),
      
      # Replace empty Hwy_Sys with placeholder 'XX'
      Hwy_Sys_fx = if_else(Hwy_Sys == '', 'XX', Hwy_Sys), 
      
      # Map highway system codes to descriptive names
      Hwy_Sys_str = recode(Hwy_Sys_fx,
        'IH'='Interstate','SL'='State Loop','SS'='State Spur','SH'='State Highway','US'='US Highway',
        'FM'='Farm to Market','RM'='Ranch to Market','BU'='Business US','PA'='Principal Arterial','BS'='Business State',
        'BI'='Business IH','PR'='Park Road','UA'='US Alternate','RE'='Rec Road','UP'='US Spur','BF'='Business FM',
        'FS'='FM Spur','RR'='Ranch Road','RS'='RM Spur','RU'='RR Spur','SA'='State Alternate','CR'='County Road',
        'FD'='Federal Road','LS'='(Local) City Street','TL'='Off-System Toll Road','FC'='Func. Classified City Street',
        'XX'='NA/Unknown'
      ),
      Is_Interstate = Hwy_Sys %in% c('IH'),
      
      # Evaluating the number of people invloved the crash using the Crash file
      Num_People_from_Crash = (Death_Cnt + Sus_Serious_Injry_Cnt + Nonincap_Injry_Cnt + Poss_Injry_Cnt + Non_Injry_Cnt + Unkn_Injry_Cnt),
      
      # Crash_Speed_Limit = Speed Limit
      # Round speed limit to nearest 5 mph bucket, with special handling for -1 (unknown)
      Speed_Limit_round = case_when(
        Crash_Speed_Limit == -1 ~ -1,
        Crash_Speed_Limit > 0 & Crash_Speed_Limit <= 25 ~ 25,
        Crash_Speed_Limit > 25 & Crash_Speed_Limit <= 85 ~ round(Crash_Speed_Limit/5)*5,
        TRUE ~ as.numeric(NA)
      ),
      
      # Simplified speed limit category for analysis
      Speed_Limit_simp = case_when(
        Speed_Limit_round == -1 ~ 'Unknown',
        Speed_Limit_round <= 45 ~ '<= 45mph',
        Speed_Limit_round > 45 ~ '>= 50mph',
        TRUE ~ NA_character_
      ),
      
      # 1Adt_Curnt_Amt = Average daily traffic amount for a given road segment and year for crashes located on the state highway system
      Volume_simp = case_when(
        is.na(Adt_Curnt_Amt) ~ 'Missing',
        Adt_Curnt_Amt >= 0 & Adt_Curnt_Amt <= 1999 ~ 'Below 2,000',
        Adt_Curnt_Amt >= 2000 & Adt_Curnt_Amt <= 4999 ~ '2,000 to 4,999',
        Adt_Curnt_Amt >= 5000 & Adt_Curnt_Amt <= 9999 ~ '5,000 to 9,999',
        Adt_Curnt_Amt >= 10000 & Adt_Curnt_Amt <= 19999 ~ '10,000 to 19,999',
        Adt_Curnt_Amt >= 20000 & Adt_Curnt_Amt <= 49999 ~ '20,000 to 49,999',
        Adt_Curnt_Amt >= 50000 & Adt_Curnt_Amt <= 999999999 ~ '50,000 and above'
      ),
      
      Longitude_fx = ifelse(is.na(Longitude), 0, Longitude),
      Latitude_fx  = ifelse(is.na(Latitude), 0, Latitude)
    ) %>%
    
    #Joining Crash data with County data.
    rename(CRIS_CNTY_NBR = Cnty_ID) %>%
    left_join(cnty_data %>% select(CRIS_CNTY_NBR, TXDOT_CNTY_NBR, CNTY_NAME, TXDOT_DIST_NBR, TXDOT_DIST_NAME, TXDOT_DIST_ABRVN),
                     by='CRIS_CNTY_NBR')

  # Decode via lookups
  # 1 - Rpt_Rdwy_Sys_ID = Roadway System (road on which crash occurred)
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'RWY_SYS_ID', 'Rpt_Rdwy_Sys_ID', 'Rpt_Rdwy_Sys_ID_str'), by='Rpt_Rdwy_Sys_ID')

  # 2 - Road_Cls_ID = The functional classification group of the priority road
  # the motor vehicle(s) was traveling on before the First Harmful Event (FHE) occurred
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'ROAD_CLS_ID', 'Road_Cls_ID', 'Road_Cls_ID_str'), by='Road_Cls_ID')

  # 3 - Road_Type_ID = Roadway Type
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'RPT_ROAD_TYPE_ID', 'Road_Type_ID', 'Road_Type_ID_str'), by='Road_Type_ID') %>%
    mutate(Road_Type_ID_simp = recode(as.integer(Road_Type_ID),
      `1`='Undivided', `2`='Divided', `3`='Divided', `4`='Divided', `98`='NA/Unknown', .default='NA/Unknown'))

  # 4 Rural/Urban - Description of whether the crash location was rural,
  #   small urban, large urban, or urbanized, for crashes located on the state highway system
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'RURAL_URBAN_ID', 'Rural_Urban_Type_ID', 'Rural_Urban_Type_ID_str'), by='Rural_Urban_Type_ID') %>%
    mutate(Rural_Urban_simp = recode(as.integer(Rural_Urban_Type_ID),
      `1`='Rural', `2`='Urban', `3`='Urban', `4`='Urban', .default='NA/Unknown'))

  # 5 Population Group - Population group of the location where the crash was located
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'POP_GROUP_ID', 'Pop_Group_ID', 'Pop_Group_ID_str'), by='Pop_Group_ID') %>%
    mutate(Pop_Group_simp = recode(as.integer(Pop_Group_ID),
      `0`='Rural',`1`='Rural',`3`='Rural',`4`='Small Urban',`5`='Small Urban',`6`='Small Urban',`7`='Large Urban',`8`='Large Urban',`9`='Urbanized',`10`='Not Applicable'))

  # 6 Median Type
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'MEDIAN_TYPE_ID', 'Median_Type_ID', 'Median_Type_ID_str'), by='Median_Type_ID') %>%
    mutate(Median_Type_simp = recode(as.integer(Median_Type_ID),
      `0`='Undivided', `2`='Divided', `3`='Divided', `4`='Divided', `5`='Divided', `6`='Divided', `7`='Divided', `99`='NA/Unknown', .default='NA/Unknown')) %>%
    # Nbr_Of_Lane - Number of lanes, not including turning and climbing lanes, 
    # for crashes located on the state highway system
    mutate(
      Nbr_Of_Lane_simp = case_when(is.na(Nbr_Of_Lane) ~ 'NA/Unknown', Nbr_Of_Lane <= 3 ~ '2 or fewer', Nbr_Of_Lane >= 4 ~ '4 or more'),
      XS_simp = case_when(
        Nbr_Of_Lane_simp == '2 or fewer' ~ '2L',
        Nbr_Of_Lane_simp == '4 or more' & Median_Type_simp == 'Undivided' ~ '4U+',
        Nbr_Of_Lane_simp == '4 or more' & Median_Type_simp == 'Divided' ~ '4D+',
        TRUE ~ 'NA/Unknown'
      ),
      XS_simpler = case_when(XS_simp %in% c('2L','4U+') ~ '2L/4U', XS_simp == '4D+' ~ '4D', TRUE ~ 'NA/Unknown')
    )

  # 7 Crash Severity
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'CRASH_SEV_ID', 'Crash_Sev_ID', 'Crash_Sev_ID_str'), by='Crash_Sev_ID') %>%
    mutate(Crash_Sev_KABCO = recode(as.integer(Crash_Sev_ID),
      `4`='K',`1`='A',`2`='B',`3`='C',`5`='O',`0`='U'),
      Crash_Sev_ID_factor = factor(recode(as.integer(Crash_Sev_ID),
        `4`='Fatal',`1`='Serious',`2`='Non-incapacitating',`3`='Possible injury',`5`='Not injured',`0`='Unknown'),
        levels=c('Fatal','Serious','Non-incapacitating','Possible injury','Not injured','Unknown')),
      Crash_InjurySeverity_Selector = Crash_Sev_ID %in% c(1,2,4)
    )

  # 8 Road Part
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'ROAD_PART_ID', 'Rpt_Road_Part_ID', 'Rpt_Road_Part_ID_str'), by='Rpt_Road_Part_ID') %>%
    mutate(Road_Part_simp = recode(as.integer(Rpt_Road_Part_ID),
      `1`='MAIN/PROPER LANE', `2`='SERVICE/FRONTAGE ROAD', `3`='ON/OFF RAMPS', `4`='ON/OFF RAMPS', `5`='OTHER', `7`='MAIN/PROPER LANE'))

  # 9 Functional System
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'FUNC_SYS_ID', 'Func_Sys_ID', 'Func_Sys_ID_str'), by='Func_Sys_ID') %>%
    mutate(Func_Sys_ID_simp = recode(as.integer(Func_Sys_ID),
      `1`='Interstates, Freeways and Expressways', `2`='Interstates, Freeways and Expressways', `3`='Arterials', `4`='Arterials', `5`='Collectors', `6`='Collectors', `7`='Local', .default='NA/Unknown'))

  # 10 Intersection Relation
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'INTRSCT_RELAT_ID', 'Intrsct_Relat_ID', 'Intrsct_Relat_ID_str'), by='Intrsct_Relat_ID') %>%
    mutate(Intrsct_Simp = recode(as.character(Intrsct_Relat_ID_str),
      'NON INTERSECTION'='Non-Intersection', 'DRIVEWAY ACCESS'='Non-Intersection', 'INTERSECTION'='Intersection', 'INTERSECTION RELATED'='Intersection'))

  # 11 MPO
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'MPO_ID', 'MPO_ID', 'MPO_ID_str'), by='MPO_ID')

  # 12 Manner of Collision
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'COLLSN_ID', 'FHE_Collsn_ID', 'FHE_Collsn_ID_str'), by='FHE_Collsn_ID') %>%
    mutate(
      FHE_Collsn_simp = case_when(
        FHE_Collsn_ID %in% c(1,2,3,4,5) ~ 'ONE MOTOR VEHICLE',
        FHE_Collsn_ID %in% c(10,11,12,13,14,15,16,17,18,19) ~ 'ANGLE',
        FHE_Collsn_ID %in% c(20,21,22,23,24,25,26,27,28,29) ~ 'SAME DIRECTION',
        FHE_Collsn_ID %in% c(30,31,32,33,34,35,36,37,38,39) ~ 'OPPOSITE DIRECTION',
        FHE_Collsn_ID %in% c(40,41,42,43,44,45,46) ~ 'OTHER')
    ) %>%
    mutate(Has_Vision_Zero = MPO_ID %in% c(15,205,90,28))

  # 13 Light condition
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'LIGHT_COND_ID', 'Light_Cond_ID', 'Light_Cond_ID_str'), by='Light_Cond_ID') %>%
    mutate(Dark_Unlit_Crash = Light_Cond_ID %in% c(2,3,4,5,6))

  # 14 First harmful event
  crashes <- crashes %>% left_join(
    make_lookup_tb(lookup_path, 'HARM_EVNT_ID', 'Harm_Evnt_ID', 'Harm_Evnt_ID_str'), by='Harm_Evnt_ID')

  # 15 Other Factor
  crashes <- crashes %>% mutate(Othr_Factr_ID = replace_na(Othr_Factr_ID, -1)) %>%
    left_join(make_lookup_tb(lookup_path, 'OTHR_FACTR_ID', 'Othr_Factr_ID', 'Othr_Factr_ID_str'), by='Othr_Factr_ID') %>%
    mutate(Othr_Factr_ID_str = coalesce(Othr_Factr_ID_str, 'NA/UNKNOWN'))

  # 16 Object Struck
  crashes <- crashes %>% left_join(make_lookup_tb(lookup_path, 'OBJ_STRUCK_ID', 'Obj_Struck_ID', 'Obj_Struck_ID_str'), by='Obj_Struck_ID') %>%
    # Identifying bridge strikes using Obj_Struck_ID column
    # `40`='HIT END OF BRIDGE (ABUTMENT OR RAIL END)',
    # `41`='HIT SIDE OF BRIDGE (BRIDGE RAIL)',
    # `42`='HIT PIER OR SUPPORT AT UNDERPASS, TUNNEL OR OVERHEAD SIGN BRIDGE',
    # `43`='HIT TOP OF UNDERPASS OR TUNNEL',
    mutate(Bridge_Strike = Obj_Struck_ID %in% c(40,41,42,43))

  crashes
}
