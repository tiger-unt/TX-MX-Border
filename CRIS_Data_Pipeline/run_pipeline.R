# -------------------------------------------------------------
# CRIS Pipeline Orchestration
# Edit the CONFIG section below and just run this file.
# -------------------------------------------------------------

setwd("J:/TPP - SRC Task Force - General/40 Tasks/01_KC_Factsheets/Data/2025 Files/TxDOT-SRC---Crash-Data-Analysis/Querying_Cris_Database")

suppressPackageStartupMessages({
  source(file.path('R','01_initial_setup.R')); load_packages()
  source(file.path('R','02_lookup.R'))
  source(file.path('R','03_geo.R'))
  source(file.path('R','04_crash_import.R'))
  source(file.path('R','05_persons_import.R'))
  source(file.path('R','06_units_import.R'))
  source(file.path('R','07_summarize.R'))
  source(file.path('R','08_export.R'))
})



# -------------------------------------------------------------
# CONFIG ??? Edit these values and run the script
# -------------------------------------------------------------
CONFIG <- list(
  base_input_dir = "J:/Austin Transportation Planning - Documents/General/Data/CRIS_Crash_Data",
  data_subdir    = "03_Extracted_files_Concatenated_2025-08-12",
  out_dir        = "C:/Temp",
  max_crashes    = 100000,      # set to NA to read all
  write_excel    = TRUE,
  write_gdb      = FALSE,       
  write_gpkg     = TRUE,
  extra_name_tag = "ExtraCrashDetails"
)

# -------------------------------------------------------------
# PIPELINE
# -------------------------------------------------------------
# Resolve file paths
paths <- build_file_paths(CONFIG$base_input_dir, CONFIG$data_subdir)

# Read Crash_IDs (optionally truncated for dev speed)
crash_ids <- get_crash_ids(paths$crash, max_rows = CONFIG$max_crashes)

# Fetch District and County geodata
geo <- fetch_geographies()

# Imports
crashes <- crash_import(crash_path = paths$crash,
                        lookup_path = paths$lookup,
                        cnty_data = geo$cnty_data,
                        crash_id_keep = crash_ids)

all_persons <- all_persons_import(person_path = paths$person,
                                  primary_person_path = paths$primary_person,
                                  lookup_path = paths$lookup,
                                  crash_id_keep = crash_ids)

units <- units_import(unit_path = paths$unit,
                      lookup_path = paths$lookup,
                      contrib_lookup_path = paths$contrib_lookup,
                      crash_id_keep = crash_ids,
                      all_persons = all_persons)

# Summaries & joins
summ <- build_summaries(crashes = crashes,
                        units = units,
                        all_persons = all_persons,
                        lookup_path = paths$lookup)

# Exports
export_outputs(crashes = summ$crashes,
               units = units,
               all_persons = all_persons,
               all_contrib_facts = summ$all_contrib_facts,
               all_contrib_facts_comb = summ$all_contrib_facts_comb,
               contrib_factors_by_crash = summ$contrib_factors_by_crash,
               contrib_factors_with_extra_data = summ$contrib_factors_with_extra_data,
               totals_by_contrib_factor_per_year = summ$totals_by_contrib_factor_per_year,
               lookup_df = read_lookup(paths$lookup),
               cnty_data = geo$cnty_data,
               dist_data = geo$dist_data,
               out_dir = CONFIG$out_dir,
               extra_name_tag = CONFIG$extra_name_tag,
               write_excel = CONFIG$write_excel,
               write_gdb = CONFIG$write_gdb,
               write_gpkg = CONFIG$write_gpkg)
