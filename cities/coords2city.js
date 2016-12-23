// //Converter Class 
// var Converter = require("csvtojson").Converter;
// var converter = new Converter({});

// //end_parsed will be emitted once parsing finished 
// converter.on("end_parsed", function (jsonArray) {
//    console.log(jsonArray); //here is your result jsonarray 
//    return jsonArray;
// });

var fs = require("fs");
var obj = require("/home/lohia/cities/coordinates.json");
var geocoder = require('local-reverse-geocoder');
var crg = require('city-reverse-geocoder');

var data = obj;
var reg = [];
var j = 0;
geocoder.init({},function() {
  // Ready to call lookUp
  for(var i in data) {
  	var lat = data[i].latitude;
  	var long = data[i].longitude;
  	geocoder.lookUp(data[i], function(err, res) {
  		//console.log(res[0][0].admin1Code.name);
  		// if(false && res[0][0].admin1Code!=null)
  		if(false)
  			reg[j] = res[0][0].admin1Code.name;
  		else
  		{
  			var crg_res = crg(lat,long);
  			reg[j] = crg_res[0].region;
  		}
  		//console.log(JSON.stringify(res, null, 2));
  	});
  	j = j+1;
     // var k = crg(lat,long);
     // console.log(k[0])
 }
 console.log('hello world');
 for(var k=0,len=reg.length;k<len;k++) {
 	console.log(reg[k]);
 }
 // console.log(reg);
 // geocoder.lookUp(obj, function(err, res) {
 //  //console.log(JSON.stringify(res, null, 2));
 //  for(var i=0,len=res.length;i<len;i++) {
 //  	console.log(res[i][0].admin1Code.name);
 //  }
});
// geocoder.lookUp(obj, function(err, res) {
//   console.log(res);
// });


// var data = obj;
// var crg = require('city-reverse-geocoder');
// for(var i in data)
// {
// 	var lat = data[i].latitude;
// 	var long = data[i].longitude;
// 	geocoder.lookUp(data[i], function(err, res) {
// 		console.log((JSON.stringify(res, null, 2)));
// 	});
//      // var k = crg(lat,long);
//      // console.log(k[0])
//  }