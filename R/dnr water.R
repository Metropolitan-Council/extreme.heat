library(tidyverse)
library(sf)
library(councilR)
library(mapview)


#dnr water layer

hydrography_url <- "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_dnr_hydrography/gpkg_water_dnr_hydrography.zip"
download.file(hydrography_url, file.path(tempdir(), basename(hydrography_url)))
list.files(tempdir())
unzip(zipfile = file.path(tempdir(), basename(hydrography_url)), exdir = tempdir()) 
list.files(tempdir())
sf::st_layers(file.path(tempdir(), "water_dnr_hydrography.gpkg"))
hydro <- read_sf(file.path(tempdir(), "water_dnr_hydrography.gpkg"), layer = "dnr_hydro_features_all")
st_write(hydro, file.path(here::here(), "data-raw/hydrography/hydrolayer.shp"), append = FALSE)

publicwater_url <- "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_mn_public_waters/gpkg_water_mn_public_waters.zip"
download.file(publicwater_url, file.path(tempdir(), basename(publicwater_url)))
list.files(tempdir())
unzip(zipfile = file.path(tempdir(), basename(publicwater_url)), exdir = tempdir()) 
list.files(tempdir())
sf::st_layers(file.path(tempdir(), "water_mn_public_waters.gpkg"))
water <- read_sf(file.path(tempdir(), "water_mn_public_waters.gpkg"), layer = "public_waters_basin_delineations")

# this is a HUGE file; probably not worth it to download. 
# wetland_url <- "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_nat_wetlands_inv_2009_2014/fgdb_water_nat_wetlands_inv_2009_2014.zip"
# options(timeout=500)
# download.file(wetland_url, file.path(tempdir(), basename(wetland_url)))
# list.files(tempdir())
# unzip(zipfile = file.path(tempdir(), basename(wetland_url)), exdir = tempdir()) 
# list.files(tempdir())
# sf::st_layers(file.path(tempdir(), "water_nat_wetlands_inv_2009-2014.gdb"))
# water <- read_sf(file.path(tempdir(), "water_mn_public_waters.gpkg"), layer = "public_waters_basin_delineations")

# water <- st_read(tempdir(), query = "SELECT * FROM CTUs") %>%
#   sf::st_transform(26915)

metc <- councilR::fetch_county_geo() %>%
  st_transform(26915)

wash_water <- water %>%
  st_join(filter(metc, NAME == "Hennepin"), join = st_intersects) %>%
  filter(!is.na(NAME))

wash_hydro <- hydro %>%
  st_join(filter(metc, NAME == "Hennepin"), join = st_intersects) %>%
  filter(!is.na(NAME))

mapview::mapview(filter(wash_water, !is.na(NAME)))
mapview::mapview(filter(wash_hydro, !is.na(NAME)))

