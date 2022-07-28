# list.files("/Volumes/shared/CommDev/Research/ZTRAX/originals/current_transaction_assessor/20211018/27/ZAsmt/")

library(repmis) # for package citations
# library(citr) # for traditional references. citr also interfaces with Zotero
library(details) # reproducibility
library(councilR)
library(tidyverse)
library(readxl)
library(data.table)
library(feather)
library(tidygeocoder)
library(sf)

#####
# fetch fst zillow
######

fst_bldg <- fst::read.fst("/Users/escheh/Local work docs/ztrax/20211018/fst_data/metro_Building.fst", 
                          columns = c("RowID", "FIPS", "AirConditioningTypeorSystemStndCode"))
fst_main<- fst::read.fst("/Users/escheh/Local work docs/ztrax/20211018/fst_data/metro_Main.fst", 
                          columns = c("RowID", "FIPS", "PropertyAddressCensusTractAndBlock",
                                      "PropertyAddressLatitude", "PropertyAddressLongitude",
                                      "PropertyFullStreetAddress", "PropertyCity", "PropertyZip",
                                      "OriginalPropertyFullStreetAddress", "OriginalPropertyAddressLastline"))

fst_ztrax <- inner_join(fst_bldg, fst_main) %>%
  filter((AirConditioningTypeorSystemStndCode != ""))


library(tidyverse)
fst_bldg <- fst::read.fst("/Users/escheh/Local work docs/ztrax/20211018/fst_data/metro_Building.fst", 
                          columns = c("RowID", "FIPS", "AirConditioningTypeorSystemStndCode"))
fst_main<- fst::read.fst("/Users/escheh/Local work docs/ztrax/20211018/fst_data/metro_Main.fst", 
                         columns = c("RowID", "FIPS", "PropertyAddressCensusTractAndBlock",
                                     "PropertyAddressLatitude", "PropertyAddressLongitude",
                                     "PropertyFullStreetAddress", "PropertyCity", "PropertyZip",
                                     "OriginalPropertyFullStreetAddress", "OriginalPropertyAddressLastline"))
fst_ztrax <- inner_join(fst_bldg, fst_main) 
nrow(fst_ztrax) #plenty of ztrax data
fst_ztrax %>% filter(PropertyCity == "Minneapolis",  #yet no data from Minneapolis? 
                     PropertyAddressLatitude > 44.92, PropertyAddressLatitude < 44.93, 
                     PropertyAddressLongitude > -93.2, PropertyAddressLongitude < -93.1) 

fst_fix_geocode <- fst_ztrax %>% filter((is.na(PropertyAddressLatitude) | is.na(PropertyAddressLongitude)), #or nas with lat/long
                                !is.na(OriginalPropertyFullStreetAddress),
                                OriginalPropertyFullStreetAddress != "") %>% #get the ones we can actually fix
  mutate(OriginalPropertyAddressLastline = str_replace(OriginalPropertyAddressLastline, ",MN", ", MN"),
         OriginalPropertyFullStreetAddress = str_replace(OriginalPropertyFullStreetAddress, "HGLDS", "Highlands"),
         address = paste(PropertyFullStreetAddress, PropertyCity, "MN", PropertyZip, sep = ", ")) %>%
  as_tibble()

fix1 <- fst_fix_geocode[1:1000, ]
fix2 <- fst_fix_geocode[1001:2000, ]
fix3 <- fst_fix_geocode[2001:3000, ]
fix4 <- fst_fix_geocode[3001:4000, ]
fix5 <- fst_fix_geocode[4001:5000, ]
fix6 <- fst_fix_geocode[5001:6000, ]
fix7 <- fst_fix_geocode[6001:7000, ]
fix8 <- fst_fix_geocode[7001:8000, ]
fix9 <- fst_fix_geocode[8001:9000, ]
fix10 <- fst_fix_geocode[9001:10000, ]
fix11 <- fst_fix_geocode[10001:nrow(fst_fix_geocode), ]
nrow(fst_fix_geocode)

lat_long_fxn <- function(x){
  x %>% 
    geocode_combine(
      queries = list(
        list(method = "census",  full_results = TRUE, api_options = list(census_return_type = 'geographies')),
        list(method = 'arcgis')),
      global_params = list(address = 'address'))
}

fixed1 <- lat_long_fxn(fix1)
fixed2 <- lat_long_fxn(fix2)
fixed3 <- lat_long_fxn(fix3)
fixed4 <- lat_long_fxn(fix4)
fixed5 <- lat_long_fxn(fix5)
fixed6 <- lat_long_fxn(fix6)
fixed7 <- lat_long_fxn(fix7)
fixed8 <- lat_long_fxn(fix8)
fixed9 <- lat_long_fxn(fix9)
fixed10 <- lat_long_fxn(fix10)
fixed11 <- lat_long_fxn(fix11)


intermed_ztrax <- bind_rows(fixed1, fixed2) %>%
  bind_rows(fixed3) %>%
  bind_rows(fixed4) %>%
  bind_rows(fixed5) %>%
  bind_rows(fixed6) %>%
  bind_rows(fixed7) %>%
  bind_rows(fixed8) %>%
  bind_rows(fixed9) %>%
  bind_rows(fixed10) %>%
  bind_rows(fixed11) %>%
  right_join(fst_ztrax) %>%
  select(RowID, lat, long, PropertyAddressLatitude, PropertyAddressLongitude) %>%
  mutate(latitude = if_else(!is.na(lat), lat, PropertyAddressLatitude),
         longitude = if_else(!is.na(long), long, PropertyAddressLongitude)) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(geocode_lat = latitude,
         geocode_long = longitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  select(RowID, geocode_lat, geocode_long)

save(intermed_ztrax, file = "./data/intermed_ztrax.rda")
load("./data/intermed_ztrax.rda")
mn_bg2020 <- tigris::block_groups(state = "MN", 
                                  county = c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington",
                                             "Wright"), #having some issue joining so get all collar counties as viable matches with geometry 
                                  year = 2021) %>%
  transmute(bg20 = GEOID) %>%
  sf::st_transform(4326)


tictoc::tic()
ztrax_bg20 <- intermed_ztrax %>% #[248587:248589, ] %>%# view()#nrow 516579
  #need to get only viable latitudes; head(1350) %>% tail(10) %>% #st_intersects(mn_bg2020) %>% view()
  filter(geocode_lat > 0) %>%
  mutate(bg_id = unlist(st_intersects(., mn_bg2020))) %>% 
  full_join(st_drop_geometry(mn_bg2020) %>% 
              mutate(bg_id = row_number()) %>% 
              select(bg_id, bg20)) 
tictoc::toc()
save(ztrax_bg20, file = "./data/ztrax_bg20.rda")
load("./data/ztrax_bg20.rda")

# AirConditioningTypeOrSystem	
# Code 	Description
# CE	Central
# CW 	Chilled Water
# EC 	Evaporative Cooler
# GT 	Geo Thermal
# NO	None
# OT	Other
# PA	Packaged AC Unit
# PR 	Partial
# RF 	Refrigeration
# VN 	Ventilation
# WA 	Wall Unit
# WU 	Window Unit
# YY	Yes



noAC <- st_drop_geometry(ztrax_bg20) %>% # filter(bg20 %in% c("271630704062", "270030501072")) %>%#head(1000) %>%
  left_join(fst_ztrax) %>%
  count(bg20, AirConditioningTypeorSystemStndCode) %>%
  group_by(bg20) %>%
  mutate(totaln = sum(n),
         percent = n / totaln) %>%
  filter(AirConditioningTypeorSystemStndCode == "NO") %>%
  rename(percent_noAC = percent)

save(noAC, file = "./data/noAC.rda")
load("./data/noAC.rda")


# fst_ztrax %>% filter(str_detect(PropertyFullStreetAddress, "2916 Legion Ave")) %>% left_join(ztrax_bg20)
fst_ztrax %>% filter(str_detect(PropertyFullStreetAddress, "Coffman")) #%>% left_join(ztrax_bg20) %>% .[1,]
fst_ztrax %>% filter(str_detect(OriginalPropertyFullStreetAddress, "Coffman")) #%>% left_join(ztrax_bg20) %>% .[1,]
fst_ztrax %>% filter(PropertyCity == "Minneapolis", 
                     PropertyAddressLatitude > 44.92, PropertyAddressLatitude < 44.93, 
                     PropertyAddressLongitude > -93.2, PropertyAddressLongitude < -93.1) #%>% .[1, ]
  
ggplot(aes(x = percent_noAC), data = noAC) + geom_histogram()
noAC %>%
  left_join(mn_bg2020) %>% st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = percent_noAC)) +
  theme_void()

############ 
#old
############
# ztrans <- readr::read_delim("/Volumes/shared/CommDev/Research/ZTRAX/originals/ZTRAX+schema+information.txt", delim = "/n")[5:30, ] %>%
#   janitor::clean_names() %>%
#   mutate(schema_info = str_sub(ztrax_schema_information, start = 2)) %>%
#   tidyr::separate(schema_info, into = c("TableName", "PrimaryKeyColumns"), sep = " : ") %>%
#   mutate(database = "ZTrans") %>%
#   select(database, TableName, PrimaryKeyColumns)
# 
# 
# zasmt <- readr::read_delim("/Volumes/shared/CommDev/Research/ZTRAX/originals/ZTRAX+schema+information.txt", delim = "/n")[31:53, ] %>%
#   janitor::clean_names() %>%
#   mutate(schema_info = str_sub(ztrax_schema_information, start = 2)) %>%
#   tidyr::separate(schema_info, into = c("TableName", "PrimaryKeyColumns"), sep = " : ") %>%
#   mutate(database = "ZAsmt") %>%
#   select(database, TableName, PrimaryKeyColumns)

zasmt_layout <- readxl::read_xlsx("/Volumes/shared/CommDev/Research/ZTRAX/originals/current_transaction_assessor/20200811/Layout.xlsx", sheet = "ZAsmt") %>%
  as_tibble()

# metro_rows <- readRDS("/Volumes/shared/CommDev/Research/ZTRAX/metro_RowIDs.RDS")

#########
# use the reading function
#########
read_ztrax <- function(date = "20211018",
                       state = 27,
                       type = "ZAsmt",
                       table = table,
                       layout = zasmt_layout,
                       metro_rows = metro_rows,
                       .fieldnames = .fieldnames,
                       rows = 100000) {
  z_COLS <- filter(layout, TableName == paste0("ut", table)) %>% 
    .[2] %>% 
    mutate(rownumber = row_number()) %>% 
    filter(FieldName %in% c(.fieldnames))
  keys <- ifelse(type == "ZAsmt", c("RowID", "FIPS"), c("TransId", "FIPS"))
  
  if (!is.na(rows)) {
    fread(
      # file = paste0("/Volumes/shared/CommDev/Research/ZTRAX/originals/current_transaction_assessor/", date, "/", state, "/", type, "/", table, ".txt"),
      file = paste0("/Users/escheh/Local work docs/ztrax/", date, "/", type, "/", table, ".txt"),
      stringsAsFactors = FALSE,
      nrows = rows,
      sep = "|",
      key = keys,
      quote = "",
      col.names = z_COLS[[1]],
      select = z_COLS[[2]],
      na.strings = ""
    )
  }
  else {
    fread(
      file = paste0("/Volumes/shared/CommDev/Research/ZTRAX/originals/current_transaction_assessor/", date, "/", state, "/", type, "/", table, ".txt"),
      stringsAsFactors = FALSE,
      sep = "|",
      quote = "",
      key = keys,
      col.names = z_colnames
    )
  }
}

########
# get information about airconditioning
########
tictoc::tic()
ztrax_zasmt <- read_ztrax(rows = 1e13,
                          table = "Building",
                          .fieldnames = c("FIPS", "RowID", "TransID", "CensusTract",
                                          "AirConditioningTypeorSystemStndCode", "OccupancyStatusStndCode",
                                          "PropertyLandUseStndCode")
) %>% 
  filter(FIPS %in% c(27003, 27019, 27037, 27053, 27123, 27139, 27163)) %>%
  filter(str_detect(PropertyLandUseStndCode, "^RR|^RI")) 

ztrax_location <- read_ztrax(rows = 1e13,
                             table = "Main",
                             .fieldnames = c("RowID", "FIPS", 
                                             "PropertyAddressCensusTractAndBlock",
                                             "PropertyAddressLatitude", "PropertyAddressLongitude",
                                             "PropertyFullStreetAddress", "PropertyCity", "PropertyZip",
                                             "OriginalPropertyFullStreetAddress", "OriginalPropertyAddressLastline")) %>% 
  filter(FIPS %in% c(27003, 27019, 27037, 27053, 27123, 27139, 27163))
ztrax <- ztrax_zasmt %>% inner_join(ztrax_location) %>% 
  filter(!is.na(PropertyCity))

tictoc::toc()

#############
# clean up geocoding
#############

# AirConditioningTypeOrSystem	
# Code 	Description
# CE	Central
# CW 	Chilled Water
# EC 	Evaporative Cooler
# GT 	Geo Thermal
# NO	None
# OT	Other
# PA	Packaged AC Unit
# PR 	Partial
# RF 	Refrigeration
# VN 	Ventilation
# WA 	Wall Unit
# WU 	Window Unit
# YY	Yes

