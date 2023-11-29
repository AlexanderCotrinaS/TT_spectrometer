// Function to mask clouds
function maskS2clouds(image) {
  var qa = image.select('QA60')
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
            qa.bitwiseAnd(cirrusBitMask).eq(0))
  return image.updateMask(mask).divide(10000)
      .select("B.*")
      .copyProperties(image, ["system:time_start"])
}

var s2 = ee.ImageCollection('COPERNICUS/S2_HARMONIZED');

var startDate = ee.Date.fromYMD(2020, 07, 01);
var endDate = ee.Date.fromYMD(2022, 12, 31);

// Function to add a NDVI band to an image
function addNDVI(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('ndvi');
  return image.addBands(ndvi);
} 

//Filtering
var originalCollection = s2
  .filter(ee.Filter.date(startDate, endDate))
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 30))
  .filter(ee.Filter.bounds(geometry))
  .map(maskS2clouds)
  .map(addNDVI);


// Display a time-series chart
var chart = ui.Chart.image.series({
  imageCollection: originalCollection.select("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A"),
  region: geometry,
  reducer: ee.Reducer.mean(),
  scale: 20
}).setOptions({
      title: 'Original NDVI Time Series',
      interpolateNulls: false,
      vAxis: {title: 'NDVI', viewWindow: {min: 0, max: 1}},
      hAxis: {title: '', format: 'YYYY-MM'},
      lineWidth: 1,
      pointSize: 4,
      series: {
        0: {color: '#238b45'},
      },

    })
print(chart);