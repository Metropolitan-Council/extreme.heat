# script to process regional (well, MSP airport) weather data



#######
# setup -------
#######
library('rnoaa')
library('tidyverse')
library("sf")
library(readxl)
library(lubridate)
library(tigris)

# #need to use an api key to connect to noaa data
# #request api key here: https://www.ncdc.noaa.gov/cdo-web/token
# #add the key to your `.Reviron` file through the console:
# #- Enter in the console: `usethis::edit_r_environ()`
# #- When the `.Renviron` file comes up in the editor, type: `NOAA_KEY="KEY GOES HERE, INSIDE QUOTES"`
# #- Save and close the `.Renviron` file.
# #- Hit Ctrl+Shift+F10 to restart R.
# 
# 
######
#get weather stations
#####
## there are several weather stations around the metro area which we could consider using
## explore: https://gis.ncdc.noaa.gov/maps/ncei/summaries/daily

tictoc::tic(msg = "Get stations")
station_data <- ghcnd_stations(refresh = F) # Takes a while to run
tictoc::toc() # 134



metro_stations <- station_data %>%
  filter(str_detect(state, "MN|Minn"),
         element == "TMAX",
         first_year <= 2013, last_year >= 2022,
         latitude >= 44.4, latitude <= 45.5,
         longitude <= -92, longitude >= -94)
# # Get all stations within 50 kilometers
# metc <- tigris::counties(state = "MN") %>%
#   filter(COUNTYFP %in% c("003", "019", "037", "053", "123", "139", "163")) %>%
#   mutate(region = "metc") %>%
#   group_by(region) %>%
#   summarise(geometry = sf::st_union(geometry))
# 
# st_centroid(metc)
# 
# msp <- data.frame(id = "msp", latitude = 44.91781, longitude = -93.30398)
# metro_stations <- meteo_nearby_stations(lat_lon_df = msp, station_data = station_data,
#                       radius = 70, var = c("TMAX"),
#                       year_min = 1983, year_max = 2022) %>% 
#   .[[1]] %>% #get the msp dataframe from the list
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
# 
metro_stations %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mapview::mapview()
# 
# 
# # pull weather data from nearest stations -- takes a few minutes
# nearest_weather <- meteo_pull_monitors(monitors = metro_stations,
#                                        var = c("TMAX"),
#                                        date_min = '2021-01-01',
#                                        date_max = '2021-12-31') %>%
#   mutate(year = lubridate::year(date)) 
# 
# station_temps <- nearest_weather %>%
#   mutate(tmax = (tmax/10 * 9/5) + 32) %>% #farenheight
#   group_by(id, year) %>%
#   mutate(max = max(tmax, na.rm = T)) %>%
#   filter(max - tmax < 2) 
# 
# 
# #which year was the hottest temp recorded? 
# station_temps %>%
#   group_by(id) %>%
#   mutate(global_max = max(tmax)) %>%
#   filter(tmax == global_max) %>%
#   left_join(metro_stations) %>% 
#   st_as_sf() %>%
#   mapview::mapview(zcol = c(#"global_max", 
#                             "year"), legend = TRUE)
# 
# 
# #what is the hottest temp of all time
# station_temps %>%
#   group_by(id) %>%
#   mutate(global_max = max(tmax)) %>%
#   filter(tmax == global_max) %>%
#   left_join(metro_stations) %>% 
#   st_as_sf() %>%
#   mapview::mapview(zcol = c(#"global_max", 
#     "year"), legend = TRUE)
# 
# #what is the hottest temp in 2021
# station_temps %>% filter(year == 2021) %>%
#   group_by(id) %>%
#   mutate(temp_year = max(tmax)) %>%
#   filter(tmax == temp_year) %>%
#   left_join(metro_stations) %>% 
#   st_as_sf() %>%
#   mapview::mapview(zcol = c("temp_year"), legend = TRUE)


# ###########
# # can explore stations a bit, we will use MSP airport data
# #station info
ncdc_stations(stationid=c('GHCND:USW00014922'))
# umn campus might also be good; 'GHCND:USC00218450'
#st paul downtown airport: GHCND:USW00014927
# vadnais lake: GHCND:USC00218477
# farmington: GHCND:USC00212737
#rosemount: GHCND:USC00217107
# hastings: GHCND:USC00213567
#jordan: GHCND:USC00214176

#datasets available at this station from 2017 and later
ncdc_datatypes(stationid=c(paste0("GHCND:", metro_stations$id)), limit=150, offset = 0, startdate = '2017-01-01')

ncdc_datasets(stationid='GHCND:USW00014922', limit=150, offset = 0, startdate = '2017-01-01')
#GHCND = daily summaries
#GSOM = monthly summaries
#GSOY = yearly summaries

# #######
# # create a function to download daily data -------
# #######
dl_daily <- function(year, .station) {
  temp <- ncdc(stationid = .station, #'GHCND:USW00014922',
               datasetid='GHCND', datatypeid = c("TMAX", "TMIN", "TAVG"
                                                 #, "TAVG"
                                                 ), #3x 365 > 1000 (which is the max, so need to break this up)
               startdate = paste0(year, '-05-01'), enddate = paste0(year, '-10-01'),
               add_units = TRUE,
               limit = 1000)

  # precip <- ncdc(stationid='GHCND:USW00014922',
  #                datasetid='GHCND', datatypeid = c("PRCP"), #3x 365 > 1000 (which is the max, so need to break this up)
  #                startdate = paste0(year, '-01-01'), enddate = paste0(year, '-12-31'),
  #                add_units = TRUE,
  #                limit = 1000)

  df2 <- temp$data %>% #bind_rows(temp$data, precip$data) %>%
    mutate(date = lubridate::ymd_hms(date),
           value = case_when(datatype == 'TMAX' & units == "celcius_tenths" ~ (value/10 * 9/5) + 32,
                             datatype == 'TAVG' & units == "celcius_tenths" ~ (value/10 * 9/5) + 32,
                             datatype == 'TMIN' & units == "celcius_tenths" ~ (value/10 * 9/5) + 32,
                             datatype == "PRCP" & units == "mm_tenths" ~ value / 10 / 25.4),
           units = case_when(units == "celcius_tenths" ~ "farenheit",
                             units == "mm_tenths" ~ "inches"))
  return(df2)
}


#######
# dl weather data for years of interest -------
#######
metro_stations_daily <- map_dfr(.x = 2013:2022, ~dl_daily(year = .x, .station = c(paste0("GHCND:", metro_stations$id)))) %>%
  mutate(year = lubridate::year(date))
save(metro_stations_daily, file = "./data/metro_stations_daily.rda")

msp_daily <- map_dfr(.x = 1938:2022, ~dl_daily(year = .x, .station = 'GHCND:USW00014922')) %>% 
  mutate(year = lubridate::year(date))
save(msp_daily, file = "./data/msp_daily.rda")

summeravg_71_00 <- msp_daily %>%
  filter(year >= 1971, year <= 2000) %>%
  group_by(datatype) %>%
  summarise(lta = mean(value, na.rm = T))

msp_temps <- msp_daily %>%
  group_by(year, datatype) %>%
  mutate(max = max(value, na.rm = T),
         mean = mean(value, na.rm = T)) %>%
  filter(value == max) %>%
  mutate(doy = lubridate::yday(date))

save(msp_temps, file = "./data/msp_temps.rda")


msp_daily %>% filter(datatype == "TMAX") %>%
  mutate(lta = summeravg_71_00[[2,2]],
         diff = value - lta,
         doy = yday(date)) %>%
  group_by(year) %>%
  ggplot(aes(x = doy, y = diff, col = year, group = year)) +
  geom_line() +
  facet_wrap(~datatype)

msp_temps %>%
  filter(datatype == "TMAX") %>%
  mutate(lta = summeravg_71_00[[2,2]],
         diff = mean - lta,
         doy = yday(date)) %>%
  group_by(year) %>%
  ggplot(aes(x = year, y = diff)) +
  geom_bar(stat = "identity")

# takeaways
# 2016 had a heat wave, but the year itself wasn't super hot
# 100 degree days are pretty rare ->https://www.dnr.state.mn.us/climate/journal/100degreesmsp.html
# 2011 was pretty darn steamy https://www.mprnews.org/story/2011/06/07/heat-storm-103-is-hottest-day-in-23-years-cool-front-ahead


# so what to do. if heat isn't increasing each year
# instead, show all temps relative to airport?? for each year? to show if vulnerable area has increased or decreased??