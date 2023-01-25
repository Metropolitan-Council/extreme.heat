

library(sf)
library(tidyverse)

# MetroGIS publishes a consolidated parcel dataset "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metrogis/plan_regional_parcels/gpkg_plan_regional_parcels.zip"
# Unfortunately, the data doesn't seem to have been super suitable to combine, and I'd instead recommend going to individual county-level parcel data sets. 
# Regardless of method, these files are MASSIVE, so download them first (put files in raw-data, this is part of gitignore fyi) and then process. 
# Zillow data is not a lot of help here either unfortunately, it's missing large pieces (probably for same reason as metrogis data is missing chunks), and this data has restrictive sharing agreements (so it's not ideal for a couple reasons)


#####
# Anoka
#####
# anoka <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsAnokaPoints.shp"))#,
#                 query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, COOLING, YEAR_BUILT
#                 FROM \"ParcelsAnokaPoints\"
#                 WHERE DWELL_TYPE IN ('RESIDENTIAL, SINGLE FAMILY', 'QUAD', 'PATIO-TOWNHOME', 'MOBILE HOME PARKS', 'APARTMENTS(4 UNITS AND UP)', 'CONDOMINIUMS')
#                 AND COOLING IN ('Yes', 'No')") %>% #, '140 Res V Land')")
#   st_drop_geometry() %>%
#   as_tibble()
# 
# count(st_drop_geometry(anoka), HEATING, COOLING)
#####
# Carver
#####
carver <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsCarverPoints.shp"),
                 query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, COOLING, YEAR_BUILT
                FROM \"ParcelsCarverPoints\"
                WHERE USECLASS1 IN ('Agricultural', 'Apt 4+ units', 'MH Park', 'Res 1 unit', 'Res 2-3 units', 'Res V Land')
                AND COOLING IN ('Yes', 'No')") %>% #, '140 Res V Land')")
  st_drop_geometry() %>%
  as_tibble()

#####
# Dakota
#####
# not reliable, i think
dakota <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsDakotaPoints.shp"),
                  query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, COOLING, YEAR_BUILT
                FROM \"ParcelsDakotaPoints\"
                WHERE DWELL_TYPE IN ('APARTMENT', 'CONDOMINM', 'DUPLEX', 'S.FAM.RES', 'TOWNHOUSE', 'TRIPLEX', 'TWIN HOME')
                AND COOLING != '<NA>'") %>% #, '140 Res V Land')")
  st_drop_geometry() %>%
  as_tibble()

#####
# Hennepin
#####
# no info on cooling?!?!?
hennepin <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsHennepinPoints.shp"),
                  query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, COOLING, YEAR_BUILT
                FROM \"ParcelsHennepinPoints\"
                WHERE USECLASS1 IN ('Apartment', 'Apartment Condominium', 'Condo - Garage/Miscellaneous', 'Condominium (also Market Rate Cooperative)', 'Cooperative (Limited Equity)', 'Double Bungalow', 'Farm', 'Farm - Agricultural Preserve', 'Farm - Hmstd (House & 1 Acre)', 'Housing - Low Income < 4 Units', 'Housing - Low Income > 3 Units', 'Housing - Low Income > 3 Units Excess', 'Manufactured Home Park', 'Manufactured Home Park - EDU Cert', 'Residential', 'Townhouse', 'Triplex')
                AND COOLING != '<NA>'") %>% #, '140 Res V Land')")
  st_drop_geometry() %>%
  as_tibble()

#####
# Ramsey
#####
#wtf, does NA mean no cooling??
ramsey <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsRamseyPoints.shp"))#,
                    query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, HEATING, COOLING, YEAR_BUILT
                FROM \"ParcelsDakotaPoints\"
                WHERE USECLASS1 IN ('Apartment', 'Apartment Condominium', 'Condo - Garage/Miscellaneous', 'Condominium (also Market Rate Cooperative)', 'Cooperative (Limited Equity)', 'Double Bungalow', 'Farm', 'Farm - Agricultural Preserve', 'Farm - Hmstd (House & 1 Acre)', 'Housing - Low Income < 4 Units', 'Housing - Low Income > 3 Units', 'Housing - Low Income > 3 Units Excess', 'Manufactured Home Park', 'Manufactured Home Park - EDU Cert', 'Residential', 'Townhouse', 'Triplex')
                AND COOLING != '<NA>'") %>% #, '140 Res V Land')")
  st_drop_geometry() %>%
  as_tibble()

#####
# Scott
#####
# #aaand NO cooling information here :/ 
# scott <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsScottPoints.shp"))#,
# query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, COOLING, YEAR_BUILT
#                 FROM \"ParcelsDakotaPoints\"
#                 WHERE USECLASS1 IN ('Apartment', 'Apartment Condominium', 'Condo - Garage/Miscellaneous', 'Condominium (also Market Rate Cooperative)', 'Cooperative (Limited Equity)', 'Double Bungalow', 'Farm', 'Farm - Agricultural Preserve', 'Farm - Hmstd (House & 1 Acre)', 'Housing - Low Income < 4 Units', 'Housing - Low Income > 3 Units', 'Housing - Low Income > 3 Units Excess', 'Manufactured Home Park', 'Manufactured Home Park - EDU Cert', 'Residential', 'Townhouse', 'Triplex')
#                 AND COOLING != '<NA>'") %>% #, '140 Res V Land')")
#   st_drop_geometry() %>%
#   as_tibble()

#####
# Washington
#####
wash <- st_read(paste0(here::here(), "/data-raw/shp_plan_regional_parcels/ParcelsWashingtonPoints.shp"),
                query = "select ZIP, CTU_NAME, POSTCOMM, CO_CODE, TAX_NAME, TAX_ADD_L1, TAX_ADD_L2, TAX_ADD_L3, HOMESTEAD, ACRES_POLY, TAX_YEAR, MKT_YEAR, EMV_LAND, EMV_BLDG, EMV_TOTAL, TAX_CAPAC, USECLASS1, USECLASS2, USECLASS3, USECLASS4, DWELL_TYPE, COOLING, YEAR_BUILT
                FROM \"ParcelsWashingtonPoints\"
                WHERE USECLASS1 IN ('100 Res 1 unit', '105 Res 2-3 units', '110 Apt 4+ units', '125 SRR')
                AND COOLING IN ('Yes', 'No')") %>% #, '140 Res V Land')")
  st_drop_geometry() %>%
  as_tibble()



count(st_drop_geometry(ramsey), HEATING, COOLING)


# anoka <- read_parcels("ParcelsAnokaPoints")
# carver <- read_parcels("ParcelsCarverPoints")
# dakota <- read_parcels("ParcelsDakotaPoints")
# hennepin <- read_parcels("ParcelsHennepinPoints")
# ramsey <- read_parcels("ParcelsRamseyPoints")
# scott <- read_parcels("ParcelsScottPoints")
wash <- read_parcels("ParcelsWashingtonPoints")

wash
count(wash, COOLING)

wash %>% filter(YEAR_BUILT > 0) %>% ggplot(aes(x = YEAR_BUILT, fill = COOLING)) + geom_density(alpha = .5)
  group_by(COOLING) %>%
  summarise(median(YEAR_BUILT))

head(wash) %>% View()
count((wash), USECLASS1)
count((wash), DWELL_TYPE)

filter(wash, str_detect(TAX_NAME, "ELLEN H ESCH")) 

(wash) %>% filter(is.na(DWELL_TYPE)) %>% View()# count(COOLING)

wash %>%
