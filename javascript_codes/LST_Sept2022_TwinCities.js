/* This script produces high resolution (10 meter) map of land surface temperature (LST) 
from September 1, 2022 for the seven-county metropolitan region of the Twin Cities. 

Published online at: https://gisdata.mn.gov/dataset/us-mn-state-metc-env-cva-lst2022

Author: Ellen H. Esch
For more info contact: ellen.h.esch@gmail.com or ellen.esch@state.mn.us

*/

////////
// Initial set-up things
////////

//remove water using the most specific data we have available

var water = metc_glu //metc 2020 GLU data is a lot more precise for our region.
  .filterMetadata('DESC2020', 'equals', 'Open Water');

var dnr_rivers = water_dnr_2022
  .filter(ee.Filter.inList('map_label', 
    ee.List(['Crow River, South Fork', 'Rum River', 'Minnesota River', 'Mississippi River'])))
    
var dnr_lakes = water_dnr_2022
  .filterMetadata('wb_class', 'equals', 'Lake or Pond');
  
var watermask = ee.Image(1).uint8()
  .paint(water, 0)
  .paint(osm_water, 0)
  .paint(extractive_water, 0)
  .clip(metc);

// function to mask sentinel clouds
  function maskS2clouds(image) {
  var qa = image.select('QA60');

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask);
}

// function to mask landsat clouds
function maskLandsatclouds(image) {
  var qa = image.select('pixel_qa|QA_PIXEL');
  var mask = qa.bitwiseAnd(1 << 3) //cloud
    .or(qa.bitwiseAnd(1 << 5)) //cloud shadow
    .or(qa.bitwiseAnd(1 << 1)) // dilated cloud
    .or(qa.bitwiseAnd(1 << 2)); // cirrus cloud 
  return image.updateMask(mask.not());
}


//////////
// Select dates from both Landsat and Sentinel
//////////

// the clearest and hottest days with imagery were previously deteremined
var landsat_date_start = '2022-09-01';
var landsat_date_end = '2022-09-02';

var S2_StartDate = '2022-09-01';
var S2_EndDate = '2022-09-02';

// select NDVI dates. for ndvi we are interested in the GREENEST pixel over the 
// ENTIRE summer (so this will mask any early crop harvests, etc. etc)
var landsat_ndvi_start = '2022-07-10';
var landsat_ndvi_end = '2022-07-31';

var S2_ndvi_StartDate = '2022-07-01';
var S2_ndvi_EndDate = '2022-09-02';

//set max cloud cover allowed
// since the scenes are so big, we can tolerate some clouds
var cloud_cover = 55; 
var cloud_cover_s2 = 25; 


////////
// Compute LST from landsat at 30 meters
// modified from Ermida 2020 
////////
// link to the code that computes the Landsat LST
var LandsatLST = require('users/ehe/MetCouncil:modules/Landsat_LST.js');

var use_ndvi = true;

var LandsatColl = LandsatLST.collection('L8', landsat_date_start, landsat_date_end, metc, use_ndvi)
  .merge(LandsatLST.collection('L9', landsat_date_start, landsat_date_end, metc, use_ndvi))
  .filter(ee.Filter.eq('WRS_PATH', 27)).filter(ee.Filter.eq('WRS_ROW', 29))
  .filterMetadata("CLOUD_COVER_LAND", "less_than", cloud_cover);
print("Landsat Images used:", LandsatColl)//how many images are there to look at?

var LandsatNDVIColl = ee.ImageCollection('LANDSAT/LC08/C02/T1_TOA') //for ndvi, toa is better than sr
  .merge(ee.ImageCollection('LANDSAT/LC09/C02/T1_TOA'))
  .filterBounds(metc)
  .filterDate('2022-06-15', '2022-09-30')

var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B5', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
};

var landsat_greenest = LandsatNDVIColl.map(addNDVI)
  .select('NDVI')
  .qualityMosaic('NDVI');

var greenest = landsat_greenest
    .clip(metc)
    .mask(watermask);

// create a color palette for heat
var cmap1 = ['blue', 'cyan', 'green', 'yellow', 'red'];
var cmap2 = ['F2F2F2','EFC2B3','ECB176','E9BD3A','E6E600','63C600','00A600']; 


///// function to get hottest pixel and add date from the landsat collection
function getMaxAndOthers(collection, maxBand, otherBands) {
  var theseBands = [maxBand].concat(otherBands);
  collection = collection.select(theseBands);
  return collection.reduce(ee.Reducer.max(theseBands.length)).rename(theseBands);
}

var Landsat_image1 = getMaxAndOthers(LandsatColl, 'LST', ['DOY', 'YEAR', 'NDVI',
  'SR_B4', 'SR_B3', 'SR_B2', 'SR_B5', 'SR_B6', 'ST_B10'])
  .addBands(greenest)
  .clip(metc)
  .mask(watermask); //do i want to include water in the calculation?
  
//rescale to LST bands and overwrite the original bands.
var farenheightBands = (Landsat_image1.select('LST').subtract(273.15)).multiply(9/5).add(32);
var Landsat_image2 = Landsat_image1.addBands({
  srcImg: farenheightBands,
  overwrite: true
});
var cloudsinhottest = Landsat_image2.select('DOY').neq(0);

//make final Landsat_image have both ndvi from lst data and lst/temp from summer data
var Landsat_image = Landsat_image2.mask(cloudsinhottest)

// uncomment the code below to export a image band to your drive of 30 m LST
/*
Export.image.toDrive({
  image: Landsat_image.select('LST'),
  description: 'LST',
  scale: 30,
  region: geometry,
  fileFormat: 'GeoTIFF',
});
*/



/////////
// Get Sentinel data; modified from Onacillova 2022
/////////

// Mosaic a collection of Sentinel images together
//Select Sentinel-2 Level-2A collection dataset coverage 
  var S2_selected_collection = ee.ImageCollection("COPERNICUS/S2_SR")
                 .filter(ee.Filter.inList('MGRS_TILE', //https://eatlas.org.au/data/uuid/f7468d15-12be-4e3f-a246-b2882a324f59
                  ['15TVK', '15TVL', //west metro
                  '15TWK', '15TWL']))  //east metro
                  .filterDate(S2_StartDate, S2_EndDate)
                  .filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", ee.Number(cloud_cover_s2))
                 .select('B2|B3|B4|B8|B11|QA60')
                  .map(maskS2clouds);
print("Sentinel Images used:", S2_selected_collection)//how many images are there to look at?

  var S2_image = S2_selected_collection
    .median()
    .mask(watermask)
    .divide(10000)
    .clip(metc);

    var S2_ndvi_selected_collection = ee.ImageCollection("COPERNICUS/S2_SR")
                 .filter(ee.Filter.inList('MGRS_TILE', //https://eatlas.org.au/data/uuid/f7468d15-12be-4e3f-a246-b2882a324f59
                  ['15TVK', '15TVL', //west metro
                  '15TWK', '15TWL']))  //east metro
                  .filterDate(S2_ndvi_StartDate, S2_ndvi_EndDate)
                  .filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", ee.Number(cloud_cover_s2))
                 .select('B2|B3|B4|B8|B11|QA60')
                  .map(maskS2clouds);


///////////
// Calculate things
//////////

// Calculate Landsat 8 spectral indices NDVI, NDWI and NDBI
  //Landsat 8 NDVI (30m spatial resolution)
  var ndvi = Landsat_image.select('NDVI').rename('ndvi'); 

  //Landsat 8 NDWI (30m spatial resolution).
  var ndwi = Landsat_image.normalizedDifference(['SR_B3', 'SR_B5']).rename('ndwi');

  //Landsat 8 NDBI (30m spatial resolution).
  var ndbi = Landsat_image.normalizedDifference(['SR_B6', 'SR_B5']).rename('ndbi');

  

  //Calculate Landsat 8 LST in Celsius Degrees (30m spatial resolution))
  var L8_LST_30m = Landsat_image.select('LST').rename('L8_LST_30m'); //use lst from ermida

  //Calculate Sentinel 2 spectral indices NDVI, NDWI and NDBI
  var addNDVI_s2 = function(img) {
    var ndvi = img.normalizedDifference(['B8', 'B4']).rename('S2_NDVI');
    return img.addBands(ndvi);
    };

    var S2_ndvi = S2_ndvi_selected_collection
    .map(addNDVI_s2)
    .qualityMosaic('S2_NDVI')//gets the greenest
    .select('S2_NDVI')
    .mask(watermask)
    .clip(metc);

    var S2_image = S2_image.addBands(ee.Image([S2_ndvi])); // add greenest ndvi
    

  //Sentinel 2 NDWI (10m spatial resolution).
  var S2_ndwi = S2_image.normalizedDifference(['B3', 'B11']).rename('S2_NDWI');
  var S2_ndbi = S2_image.normalizedDifference(['B11', 'B8']).rename('S2_NDBI');


/////////////////
// Regression Calculation 
/////////////////  
  // preparing bands
  var bands = ee.Image(1).addBands(ndvi).addBands(ndbi).addBands(ndwi).addBands(L8_LST_30m)
    .rename(["constant", "ndvi", "ndbi", "ndwi", "L8"])
    .mask(watermask);

  // run the multiple regression analysis
  var imageRegression = bands.reduceRegion({
                        reducer: ee.Reducer.linearRegression({numX:4, numY:1}),
                        geometry: metc,
                        scale: 30,
                        maxPixels: 1e12,
                        });
  
  // create images from coefficients
  print ("", "* Multipe linear regression model" ,"coefficients for Landsat 8 LST downscaling");
  var coefList2 = ee.Array(imageRegression.get("coefficients")).toList();
  var intercept2 = ee.Image(ee.Number(ee.List(coefList2.get(0)).get(0)));
  var intercept2_list = ee.List(coefList2.get(0)).get(0);
  var slopeNDVI2 = ee.Image(ee.Number(ee.List(coefList2.get(1)).get(0)));
  var slopeNDVI2_list =  ee.List(coefList2.get(1)).get(0);
  var slopeNDBI2 = ee.Image(ee.Number(ee.List(coefList2.get(2)).get(0)));
  var slopeNDBI2_list =  ee.List(coefList2.get(2)).get(0);
  var slopeNDWI2 = ee.Image(ee.Number(ee.List(coefList2.get(3)).get(0)));
  var slopeNDWI2_list =  ee.List(coefList2.get(3)).get(0);
  
  print(intercept2_list, "intercept", "",
        slopeNDVI2_list, "slope NDVI", "", 
        slopeNDBI2_list, "slope NDBI", "",
        slopeNDWI2_list, "slope NDWI");
  
  // calculate the final downscaled image
  var downscaled_LST_10m = ee.Image(intercept2).add(slopeNDVI2.multiply(S2_ndvi))
              .add(slopeNDBI2.multiply(S2_ndbi)).add(slopeNDWI2.multiply(S2_ndwi));
  

  // #######################################################################
  // L8-LST 30 m model calculation
  
  var L8_LST_MODEL = intercept2.add(slopeNDVI2.multiply(ndvi))
              .add(slopeNDBI2.multiply(ndbi))
              .add(slopeNDWI2.multiply(ndwi)).clip(metc);//.clip(selected_geometry);
  
  var L8_RESIDUALS = L8_LST_30m.subtract(L8_LST_MODEL);
  
  // ####################################################################### 
  // Gaussian convolution
  
  // Define a gaussian kernel
  var gaussian = ee.Kernel.gaussian({
    radius: 1.5, units: 'pixels'
  });
  
  // Smooth the image by convolving with the gaussian kernel.
  var L8_RESIDUALS_gaussian = L8_RESIDUALS.resample("bicubic").convolve(gaussian);
  
  var visParam_residuals = {
          min: -10,
          max: 9,
          palette: ['blue', 'yellow', 'red']
          };
  

  // Calculate the final downscaled LSTs
  var downscaled_LST_10m2 = ee.Image(intercept2).add(slopeNDVI2.multiply(S2_ndvi))
                .add(slopeNDBI2.multiply(S2_ndbi)).add(slopeNDWI2.multiply(S2_ndwi));

  var S2_LST_10_w_Residuals = downscaled_LST_10m2.add(L8_RESIDUALS_gaussian).rename('2022 10m LST');


/////////////
// Export
////////////

  // Export the final image to Drive
  // Export.image.toDrive({
  //   image: S2_LST_10_w_Residuals,
  //   description: 'Downscaled_LST_usingS2_10m',
  //   folder: "image EE",
  //   scale: 10,
  //   region: metc,//selected_geometry,
  //   crs: 'EPSG:4326',
  //   maxPixels: 1e12,
  //   fileFormat: 'GeoTIFF',
  //   formatOptions: {
  //     cloudOptimized: true
  //   }
  // });
  
    // Export the image to Cloud Storage.
Export.image.toCloudStorage({
  image: S2_LST_10_w_Residuals,
  description: 'Downscaled_LST_usingS2_10m_2022',
  bucket: 'growing-shade-bucket',
  region: metc,
  scale: 10,
  maxPixels: 1e12
});


Export.image.toCloudStorage({
  image: S2_LST_10_w_Residuals,
  description: 'Downscaled10m_LST2022_32615',
  bucket: 'growing-shade-bucket',
  region: metc,
  scale: 10,
  maxPixels: 1e12,
  crs: 'EPSG:32615' //https://epsg.io/32615
});
  
  /////////////
  //Mapping
  ///////////
  Map.addLayer(Landsat_image.select('LST'), {bands: 'LST', min:70, max:110, palette:cmap1}, "Landsat LST - 30m", false);
  Map.addLayer(downscaled_LST_10m2, {min:70, max:110, palette:cmap1}, 'S2-LST 10m (no residuals)', true);
