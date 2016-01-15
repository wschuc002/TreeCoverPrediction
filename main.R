# Student: William Schuch & Rik van Berkum
# Team: Geodetic Engineers of Utrecht
# Institute: Wageningen University and Research
# Course: Geo-scripting (GRS-33806)
# Date: 2016-01-13
# Week 2, Lesson 8: Advanced Raster Analysis

rm(list = ls())  # Clear the workspace!
ls() ## no objects left in the workspace

# Installing/updating packages the random forest model
#install.packages("randomForest")
#install.packages("rasterVis")
#install.packages("rgeos", "randomForest")


# Document
#install.packages(c("knitr", "yaml", "htmltools", "caTools", "bitops", "rmarkdown"))
#install.packages(c("RCurl", "PKI", "packrat", "rstudioapi"))


# load librarys
library(raster)
library(sp)
library(rgdal)
library(rgeos)

library(randomForest)
library(rasterVis)

# referring to functions in R folder
#source("./R/Preprocessing.R")

# Loading the data into memory (band 6 (thermal infra-red) will be excluded from this exercise)
load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")

# filter out flags for water, cloud or cloud shadow pixels
vcfGewata[vcfGewata > 100] <- NA

## build a brick containing all data
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")
summary(alldata)

plot(alldata)

# •produce one or more plots that demonstrate the relationship between...
# the Landsat bands and the VCF tree cover.
pairs(alldata)
# Conclusion: 

## extract all data to a data.frame
df <- as.data.frame(getValues(alldata))

head(df)

# # # USING TRAINING DATA?
load("data/trainingPoly.rda")
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
classes <- rasterize(trainingPoly, vcfGewata, field='Code', progress= "text")
cols <- c("orange", "dark green", "light blue")
# plot without a legend
plot(classes, col=cols, legend=FALSE)
# add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")
# # #

# Example, but then with 'randomForest'
model <- randomForest(lc ~ band1 + band2 + band3 + band4 + band5 + band6 + band7, data = calib2)
# Use the model to predict land cover
lcMap <- predict(wagLandsatCrop, model = model)


model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = df)
model
summary(model)
# Use the model to predict land cover
lcMap <- predict(alldata, model = model)

levelplot(lcMap, col.regions = c('green', 'brown', 'darkgreen', 'lightgreen', 'grey', 'blue'))

?lm
GEWATA.lm <- lm(formula = band4 ~ VCF, data = df)
GEWATA.lm

summary(GEWATA.lm)$r.sqaured

# download the data (BONUS)
dir.create("./output", showWarnings = FALSE)



