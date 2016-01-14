# Student: William Schuch & Rik van Berkum
# Team: Geodetic Engineers of Utrecht
# Institute: Wageningen University and Research
# Course: Geo-scripting (GRS-33806)
# Date: 2016-01-13
# Week 2, Lesson 8: Vector - Raster



# # unzip the MODUS pack
# unzip("./data/MODIS.zip", exdir = "./data")
# 
# # bricking and stacking of the raster
# modisgrdpath <- "./data/MOD13A3.A2014001.h18v03.005.grd"
# modisbrick <- brick(modisgrdpath)
# stack <- stack(modisbrick)
# 
# # Download City's
# nlCity <- raster::getData('GADM',country='NLD', level=2, path = "./data")
# # remove rows with nodata
# nlCity@data <- nlCity@data[!is.na(nlCity$NAME_2),]
# # transform to the same CRS
# nlCitySinu <- spTransform(nlCity, CRS(proj4string(modisbrick)))