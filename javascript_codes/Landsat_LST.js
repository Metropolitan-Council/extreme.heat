/* 
Author: Sofia Ermida (sofia.ermida@ipma.pt; @ermida_sofia)
Modified by: Ellen Esch (ellen.h.esch@gmail.com; ellen.esch@state.mn.us)

This code is free and open. 
By using this code and any data derived with it, 
you agree to cite the following reference 
in any publications derived from them:
Ermida, S.L., Soares, P., Mantas, V., Göttsche, F.-M., Trigo, I.F., 2020. 
    Google Earth Engine open-source code for Land Surface Temperature estimation from the Landsat series.
    Remote Sensing, 12 (9), 1471; https://doi.org/10.3390/rs12091471

This function selects the Landsat data based on user inputs
and performes the LST computation

to call this function use:

var LandsatLST = require('users/sofiaermida/landsat_smw_lst:modules/Landsat_LST.js')
var LandsatCollection = LandsatLST.collection(landsat, date_start, date_end, geometry)

USES:
    - NCEP_TPW.js
    - cloudmask.js
    - compute_NDVI.js
    - compute_FVC.js
    - compute_emissivity.js
    - SMWalgorithm.js

INPUTS:
        - landsat: <string>
                  the Landsat satellite id
                  valid inputs: 'L4', 'L5', 'L7' and 'L8'
        - date_start: <string>
                      start date of the Landsat collection
                      format: YYYY-MM-DD
        - date_end: <string>
                    end date of the Landsat collection
                    format: YYYY-MM-DD
        - geometry: <ee.Geometry>
                    region of interest
        - use_ndvi: <boolean>
                if true, NDVI values are used to obtain a
                dynamic emissivity; if false, emissivity is 
                obtained directly from ASTER
OUTPUTS:
        - <ee.ImageCollection>
          image collection with bands:
          - landsat original bands: all from SR excpet the TIR bands (from TOA) 
          - cloud masked
          - 'NDVI': normalized vegetation index
          - 'FVC': fraction of vegetation cover [0-1]
          - 'TPW': total precipitable water [mm]
          - 'EM': surface emissvity for TIR band
          - 'LST': land surface temperature
          
  14-08-2020: update to avoid using the getInfo() and if() 
    (Thanks Tyler Erickson for the suggestion)
    
  11-07-2022: update to use collection 2
*/

///////////////////////////// ATTENTION //////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
//
// As off 11.07.2022 a new version of the code is released:
//      - update to use collection 2 data
//      - emissivities of water and snow surfaces are now prescribed 
// 
// the previous version of the code will still be available; the replaced code
// is commented
//
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

// MODULES DECLARATION -----------------------------------------------------------
// Total Precipitable Water 
var NCEP_TPW = require('users/ehe/MetCouncil:modules/NCEP_TPW.js')
//cloud mask
var cloudmask = require('users/ehe/MetCouncil:modules/cloudmask.js')
//Normalized Difference Vegetation Index
var NDVI = require('users/ehe/MetCouncil:modules/compute_NDVI.js')
//Fraction of Vegetation cover
var FVC = require('users/ehe/MetCouncil:modules/compute_FVC.js')
//surface emissivity
var EM = require('users/ehe/MetCouncil:modules/compute_emissivity.js')
// land surface temperature
var LST = require('users/ehe/MetCouncil:modules/SMWalgorithm.js')

// --------------------------------------------------------------------------------

// This function adds a band representing the image timestamp.
// var addDate = function(image) {
//   // return image.addBands(image.getString('system::index'));
//   return image.addBands(image.metadata('system:index'));
// };

// var prep = function(image) {
//   image = image.addBands(ee.Image.constant(
//       ee.Date(image.get('DATE_ACQUIRED')).millis()).long());
//   // var toa = ee.Algorithms.Landsat.TOA(image);
//   // var cloud = ee.Algorithms.Landsat.simpleCloudScore(toa).select('cloud')
//   // image = image.addBands(ee.Image(1).subtract(cloud).select([0], ['cloud']))
//   return image;
// }

function addDate(img) {
  // var ndvi = img.normalizedDifference(['B5', 'B4']).rename('NDVI').toFloat();
  var doy = ee.Image(img.date().getRelative('day', 'year')).rename('DOY').toInt();
  var year = ee.Image(img.date().get('year')).rename('YEAR').toInt();
  return img.addBands([doy, year]);
}



var COLLECTION = ee.Dictionary({
  'L4': {
    'TOA': ee.ImageCollection('LANDSAT/LT04/C02/T1_TOA'),
    'SR': ee.ImageCollection('LANDSAT/LT04/C02/T1_L2'),
    'TIR': ['B6',],
    'VISW': ['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7','QA_PIXEL']
  },
  'L5': {
    'TOA': ee.ImageCollection('LANDSAT/LT05/C02/T1_TOA'),
    'SR': ee.ImageCollection('LANDSAT/LT05/C02/T1_L2'),
    'TIR': ['B6',],
    'VISW': ['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7','QA_PIXEL']
  },
  'L7': {
    'TOA': ee.ImageCollection('LANDSAT/LE07/C02/T1_TOA'),
    'SR': ee.ImageCollection('LANDSAT/LE07/C02/T1_L2'),
    'TIR': ['B6_VCID_1','B6_VCID_2'],
    'VISW': ['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7','QA_PIXEL']
  },
  'L8': {
    'TOA': ee.ImageCollection('LANDSAT/LC08/C02/T1_TOA'),
    'SR': ee.ImageCollection('LANDSAT/LC08/C02/T1_L2'),
    'TIR': ['B10','B11'],
    'VISW': ['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','SR_B7','QA_PIXEL']
  },
  'L9': {
    'TOA': ee.ImageCollection('LANDSAT/LC09/C02/T1_TOA'),
    'SR': ee.ImageCollection('LANDSAT/LC09/C02/T1_L2'),
    'TIR': ['B10','B11'],
    'VISW': ['SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B6','SR_B7','QA_PIXEL']
  }
});


exports.collection = function(landsat, date_start, date_end, geometry, use_ndvi) {

  // load TOA Radiance/Reflectance
  var collection_dict = ee.Dictionary(COLLECTION.get(landsat));

  var landsatTOA = ee.ImageCollection(collection_dict.get('TOA'))
                .filter(ee.Filter.date(date_start, date_end))
                .filter(ee.Filter.calendarRange(122, 275, 'day_of_year')) //get May1-Oct1 only;https://groups.google.com/g/google-earth-engine-developers/c/yv5ATPmtxWA/m/7LQPsL7DBgAJ
                .filterBounds(geometry)
                // .map(cloudmask.toa);
              
  // load Surface Reflectance collection for NDVI
  var landsatSR = ee.ImageCollection(collection_dict.get('SR'))
                .filter(ee.Filter.date(date_start, date_end))
                .filterBounds(geometry)
                .map(cloudmask.sr)
                .map(NDVI.addBand(landsat))
                .map(FVC.addBand(landsat))
                .map(NCEP_TPW.addBand)
                .map(EM.addBand(landsat,use_ndvi))
                .map(addDate);

  // combine collections
  // all channels from surface reflectance collection
  // except tir channels: from TOA collection
  // select TIR bands
    var tir = ee.List(collection_dict.get('TIR'));
  var visw = ee.List(collection_dict.get('VISW'))
    .add('NDVI')
    .add('FVC')
    .add('TPW')
    .add('TPWpos')
    .add('EM');
  var landsatALL = (landsatSR.combine(landsatTOA.select(tir), true));
  // var landsatALL = (landsatSR.select(visw).combine(landsatTOA.select(tir), true));
  // var tir = ee.List(collection_dict.get('TIR'));
  // var landsatALL = (landsatSR.combine(landsatTOA.select(tir), true));
  
  // compute the LST
  var landsatLST = landsatALL.map(LST.addBand(landsat));

  return landsatLST;
};


//   .merge(LandsatLST.collection('L5', date_start, date_end, geometry, use_ndvi))
