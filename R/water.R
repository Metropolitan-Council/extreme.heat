library(tidyverse)
library(sf)
library(osmdata)
`%not_in%` <- Negate(`%in%`)

# metc <- councilR::fetch_county_geo(core = T) %>%
#   group_by(STATEFP) %>%
#   summarise()


#######
# pull osm data
#######

# using purrr here so I can force sys.sleep between API calls
packageVersion("osmdata")
curl::curl_version()

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
osm_data_natural <- purrr::map(.x = c(1:nrow(summarise(metc_plus))), .f = function(x) {
  Sys.sleep(10)

  bbox <- summarise(metc_plus) %>%
    st_transform(4326) %>%
    st_bbox()
  
  natural <- try(opq(bbox, timeout = 100) %>%
               add_osm_feature(key = "natural", value = "water") %>% # ,value = c("pedestrian", "cycleway", "footway", "bridleway", "path")) %>%
               osmdata_sf())
  natural
})

osm_data_manmade <- purrr::map(.x = c(1:nrow(summarise(metc_plus))), .f = function(x) {
  Sys.sleep(10)
  
  bbox <- summarise(metc_plus) %>%
    st_transform(4326) %>%
    st_bbox()
  
  manmade <- try(opq(bbox, timeout = 100) %>%
                   add_osm_feature(key = "leisure", value = "swimming_pool") %>% # ,value = c("pedestrian", "cycleway", "footway", "bridleway", "path")) %>%
                   osmdata_sf())
  
  manmade
})

# beepr::beep(sound = 3)

######
# get lines from osm data
# An easy step: just extract the `osm_lines` objects from the osm data downloaded above.
######
# mapview::mapview(osm_data_natural[[1]]$osm_polygons)
# mapview::mapview(osm_data_natural[[1]]$osm_multipolygons)

water_features <- osm_data_natural[[1]]$osm_multipolygons %>% 
  select(osm_id, name, type, natural, geometry) %>%
  filter(natural == "water") %>%
  mutate(flag = "nat multi") %>%
  bind_rows(osm_data_natural[[1]]$osm_polygons %>% select(osm_id, name, natural, place, geometry) %>% 
              filter(natural == "water") %>%
              filter(place %not_in% c("islet", "island"),) %>%
              filter(osm_id %not_in% c("41419612", "41428362", "693748759")) %>%
              mutate(flag = "nat poly")) %>%
  bind_rows(osm_data_manmade[[1]]$osm_polygons %>% 
              select(osm_id, name, leisure, geometry) %>% 
              filter(leisure == "swimming_pool") %>% 
              mutate(flag = "man poly")) %>%
  bind_rows(osm_data_manmade[[1]]$osm_multipolygons %>% 
              select(osm_id, name, leisure, geometry) %>%
              filter(leisure == "swimming_pool") %>% 
              mutate(flag = "man multi")) #%>%
  # filter(name %not_in% c("Coney Island"))
  # st_cast("MULTIPOLYGON")

# water_features
# mapview::mapview(list(water_features, metc), alpha.regions = list(.6, 0), lwd = c(1, 4), color = c("black", "white"))

######
# save
######
write_sf_shp_zip <- function(.capacity, overwrite = FALSE) {
  
  zipfile <- paste0(.capacity, ".zip")

  if (file.exists(zipfile) && !overwrite) {
    stop(paste0("File already exists: ", zipfile,
                "\nUse 'overwrite = TRUE' to overwrite the existing file."))
  }
  
  tmp_zip <- basename(zipfile)
  shp_name <- paste0(tools::file_path_sans_ext(tmp_zip), ".shp")
  
  ## Temporary directory to write .shp file
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE))
  
  shpfile <- water_features
  
  sf::write_sf(shpfile, file.path(tmp, shp_name), delete_layer = TRUE, layer_options="ENCODING=UTF-8")
  withr::with_dir(tmp, zip(tmp_zip, list.files()))
  
  file.copy(file.path(tmp, tmp_zip), zipfile, overwrite = overwrite)
}


write_sf_shp_zip("water_features", overwrite = TRUE)

