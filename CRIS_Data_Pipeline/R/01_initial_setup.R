
load_packages <- function() {
  pkgs <- c('pacman','memoise', 'data.table','tidyverse','magrittr','sf','writexl','httr')
  if (!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman', repos='https://cloud.r-project.org')
  pacman::p_load(char = pkgs)
  invisible(TRUE)
}

# Helper: choose first existing path from a vector (e.g., Sys.glob result)
choose_first <- function(paths) {
  paths <- sort(paths)
  if (length(paths) == 0) stop('No matching files found.')
  paths[[1]]
}

# Resolve all required file paths from a base directory and subfolder
build_file_paths <- function(base_input_dir, data_subdir) {
  root <- file.path(base_input_dir, data_subdir)
  list(
    person = choose_first(Sys.glob(file.path(root, '*!!!person_*.csv'))),
    primary_person = choose_first(Sys.glob(file.path(root, '*!!!primaryperson_*.csv'))),
    unit = choose_first(Sys.glob(file.path(root, '*!!!unit_*.csv'))),
    crash = choose_first(Sys.glob(file.path(root, '*!!!crash_*.csv'))),
    lookup = choose_first(Sys.glob(file.path(root, '*!!!lookup_*.csv'))),
    contrib_lookup = choose_first(Sys.glob(file.path(base_input_dir, 'CONTRIB_FACTR_ID.csv')))
  )
}

# Small helper to read Crash_IDs with optional limit
get_crash_ids <- function(crash_path, max_rows = NA_integer_) {
  dt <- if (is.na(max_rows)) data.table::fread(crash_path) else data.table::fread(crash_path, nrows = max_rows)
  as_tibble(dt) %>% select(Crash_ID) %>% pull(Crash_ID)
}

