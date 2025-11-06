
export_outputs <- function(crashes, units, all_persons, all_contrib_facts, all_contrib_facts_comb, contrib_factors_by_crash, contrib_factors_with_extra_data, totals_by_contrib_factor_per_year, lookup_df, cnty_data, dist_data, out_dir, extra_name_tag, write_excel = TRUE, write_gdb = FALSE, write_gpkg = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  todays_date <- as.character(Sys.Date())

  if (isTRUE(write_excel)) {
    out_xl <- file.path(out_dir, paste0('Crash_Data_', extra_name_tag, '_', todays_date, '.xlsx'))
    output_list <- list(
      crashes = crashes,
      units = units,
      all_persons = all_persons,
      all_contrib_facts = all_contrib_facts,
      all_contrib_facts_comb = all_contrib_facts_comb,
      contrib_factors_by_crash = contrib_factors_by_crash,
      contrib_factors_with_extra_data = contrib_factors_with_extra_data,
      totals_by_contrib_fact_per_year = totals_by_contrib_factor_per_year,
      lookup_df = lookup_df,
      cnty_data = select(cnty_data, -any_of('geometry')),
      dist_data = select(dist_data, -any_of('geometry'))
    )
    write_xlsx(output_list, out_xl)
  }

  write_spatial <- function(df, dsn, layer, driver) {
    st_as_sf(df, coords = c('Longitude_fx','Latitude_fx'), crs = 4326, agr = 'constant') %>%
      st_write(dsn = dsn, layer = layer, driver = driver, append = FALSE, quiet = TRUE)
  }

  if (isTRUE(write_gdb)) {
    out_gdb <- file.path(out_dir, paste0('Crash_Data_', extra_name_tag, '_', todays_date, '.gdb'))
    write_spatial(crashes, out_gdb, 'all_crashes', 'OpenFileGDB')
    write_spatial(filter(crashes, Pedestrian_Crash), out_gdb, 'pedestrian_crashes', 'OpenFileGDB')
    write_spatial(filter(crashes, Pedalcyclist_Crash), out_gdb, 'pedalcyclist_crashes', 'OpenFileGDB')
    write_spatial(filter(crashes, Pedestrian_Crash | Pedalcyclist_Crash), out_gdb, 'pedestrian_and_pedalcyclist_crashes', 'OpenFileGDB')
  }

  if (isTRUE(write_gpkg)) {
    out_gpkg <- file.path(out_dir, paste0('Crash_Data_', extra_name_tag, '_', todays_date, '.gpkg'))
    write_spatial(crashes, out_gpkg, 'all_crashes', 'GPKG')
    write_spatial(filter(crashes, Pedestrian_Crash), out_gpkg, 'pedestrian_crashes', 'GPKG')
    write_spatial(filter(crashes, Pedalcyclist_Crash), out_gpkg, 'pedalcyclist_crashes', 'GPKG')
    write_spatial(filter(crashes, Pedestrian_Crash | Pedalcyclist_Crash), out_gpkg, 'pedestrian_and_pedalcyclist_crashes', 'GPKG')
  }
}
