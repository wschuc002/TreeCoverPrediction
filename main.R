# Student: William Schuch & Rik van Berkum
# Team: Geodetic Engineers of Utrecht
# Institute: Wageningen University and Research
# Course: Geo-scripting (GRS-33806)
# Date: 2016-01-13
# Week 2, Lesson 8: Advanced Raster Analysis

rm(list = ls())  # Clear the workspace!
ls() ## no objects left in the workspace

# Installing packages (If you get an error with the 'extract' function)
#install.packages("raster")

# load librarys
library(raster)
library(sp)
library(rgdal)
library(rgeos)

# referring to functions in R folder
source("./R/Preprocessing.R")

getwd()







# download the data (BONUS)
dir.create("./output", showWarnings = FALSE)



