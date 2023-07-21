Land Surface Temperature 2022, Twin Cities
================
18 July 2023

This code repository is an attachment [for the resource on the Minnesota
Geospatial Commons entitled Land Surface Temperature 2022, Twin
Cities](https://gisdata.mn.gov/dataset/us-mn-state-metc-env-cva-lst2022).
This repository contains javascript code to create and processes a
high-resolution (10 meter) dataset on land surface temperatures from
September 1, 2022 for the Twin Cities.

This repository contains a folder “javascript_codes” where you can find
JavaScript Google Earth Engine (GEE) code used to process and create the
downscaled land surface temperature derived from Landsat thermal sensing
using the spectral bands of Sentinel-2. The “r_postprocessing” file
contains R scripts to post-processes the results of the downscaled
raster data.

## A brief description of the methodology:

High resolution (10 meter) land surface temperature (LST) from September
1, 2022 is mapped for the seven-county metropolitan region of the Twin
Cities. The goal of the map is to show the heat differences across the
region and is not intended to show the maximum temperature that any
specific area can reach. The raster dataset was computed at 30 meters
using satellite imagery from Landsat 9 and downscaled to 10 meters using
Copernicus Sentinel-2. These datasets were integrated using techniques
modified from Ermida et al. 2020 and Onačillová et al. 2022). Open water
was removed using ancillary data from OpenStreetMap and 2020 Generalized
Land Use for the Twin Cities (Metropolitan Council).

First, Landsat 9 imagery taken at 11:59 am CDT on September 01, 2022 was
processed into 30-meter resolution LST (based on Ermida et al. 2020). At
this time, the air temperature was 88° F at the Minneapolis-St. Paul
International Airport (NOAA). A model predicting LST based on spectral
indices of Normalized Difference Vegetation Index (NDVI), Normalized
Difference Water Index (NDWI), and Normalized Difference Built-up Index
(NDBI) was created and applied to 10-meter Sentenel-2 imagery.
Sentinel-2 imagery was also taken on September 1, 2022, and this
resulted in a 10-meter downscaled LST image (based on Onačillová et
al. 2022). To account for anomalies in NDVI on the primary image date of
September 1 (e.g., recently harvested agricultural fields), maximum NDVI
occurring between July 1, 2022 and September 1, 2022 was used for both
Landsat and Sentinel image processing. Water bodies were removed for all
processing steps (OpenStreetMap 2023, Metropolitan Council 2021).

This dataset is an update to the [2016 LST data for the Twin Cities
Region (Metropolitan
Council)](https://gisdata.mn.gov/dataset/us-mn-state-metc-env-cva-lst2016).

### Sources:

Ermida, S.L., Soares, P., Mantas, V., Göttsche, F.-M., Trigo, I.F.,
2020. Google Earth Engine open-source code for Land Surface Temperature
estimation from the Landsat series. Remote Sensing, 12 (9), 1471;
<https://doi.org/10.3390/rs12091471>.

Metropolitan Council. 2021. Generalized Land Use 2020. Minnesota
Geospatial Commons.
<https://gisdata.mn.gov/dataset/us-mn-state-metc-plan-generl-lnduse2020>

Metropolitan Council. 2017. Land Surface Temperature 2016, Twin Cities.
Minnesota Geospatial Commons.
<https://gisdata.mn.gov/dataset/us-mn-state-metc-env-cva-lst2016>

NOAA, National Oceanic and Atmospheric Administration, National Centers
for Environmental Information, station USW00014922. September 1, 2022.

Onačillová, K., Gallay, M., Paluba, D., Péliová, A., Tokarčík, O.,
Laubertová, D. 2022. Combining Landsat 8 and Sentinel 2 data in Google
Earth Engine to derive higher resolution land surface temperature maps
in urban environment. Remote Sensing, 14 (16), 4076.
<https://doi.org/10.3390/rs14164076>.

OpenStreetMap contributors. 2023. Retrieved from
<https://planet.openstreetmap.org> on April 12, 2023.
