
# Fetch TxDOT Districts and County Boundaries via ArcGIS REST (GeoJSON)
# Returns a list(dist_data=..., cnty_data=...)
fetch_geographies <- function() {
  my_url_builder <- function(url_main) {
    parsed <- parse_url(url_main)
    parsed$query <- list(where = 'OBJECTID is not NULL', outFields='*', returnGeometry='true', f='geojson')
    build_url(parsed)
  }
  dist_url <- paste0('https://services.arcgis.com/KTcxiTD9dsQw4r7Z/arcgis/rest/services/TxDOT_Districts/FeatureServer/0/query')
  cnty_url <- paste0('https://services.arcgis.com/KTcxiTD9dsQw4r7Z/arcgis/rest/services/Texas_County_Boundaries/FeatureServer/0/query')

  dist_data <- st_read(my_url_builder(dist_url), quiet = TRUE) %>%
    as.data.frame() %>% as_tibble() %>%
    mutate(
      TXDOT_DIST_NAME = DIST_NM,
      TXDOT_DIST_ABRVN = TXDOT_DIST_ABRVN_NM
    )

  cnty_data <- st_read(my_url_builder(cnty_url), quiet = TRUE) %>%
    as.data.frame() %>% as_tibble() %>%
    left_join(dist_data %>% select(TXDOT_DIST_NBR, TXDOT_DIST_ABRVN, TXDOT_DIST_NAME), by='TXDOT_DIST_NBR') %>%
    mutate(CNTY_NAME = CNTY_NM, CRIS_CNTY_NBR = CMPTRL_CNTY_NBR)

  list(dist_data = dist_data, cnty_data = cnty_data)
}
