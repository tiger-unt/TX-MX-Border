# memoise() wraps the function and caches its output based on the input arguments.
# The first time read_lookup("path/to/file.csv") is called, it reads the file and stores the result.
# Subsequent calls with the same lookup_path will return the cached data without re-reading the file.
read_lookup <- memoise(function(lookup_path) {
  data.table::fread(lookup_path)
})

lookup_df <- fread(lookup_path)

# Creates a lookup tibble for a given ColumnName
make_lookup_tb <- function(lookup_path,
                           lookup_colname_str = '',
                           new_id_colname = '',
                           new_desc_colname = '',
                           add_na_unknown_row = FALSE,
                           na_unknown_value = -1,
                           na_unknown_str = 'NA/UNKNOWN') {
  lookup_df <- read_lookup(lookup_path)  # Cached read
  out_df <- lookup_df %>%
    filter(ColumnName == lookup_colname_str) %>%
    select(-ColumnName)
  
  if (add_na_unknown_row) {
    out_df <- out_df %>%
      add_row('ID' = na_unknown_value, 'Description' = na_unknown_str)
  }
  
  out_df %>%
    rename(!!new_id_colname := ID, !!new_desc_colname := Description)
}