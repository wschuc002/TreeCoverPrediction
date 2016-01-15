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


hist(vcfGewata, main="Frequency per value in vcfGewata", xlab="Value")

# filter out flags for water, cloud or cloud shadow pixels
vcfGewata[vcfGewata > 100] <- NA

## build a brick containing all data
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")
summary(alldata)

plot(alldata)

# Plotting correlation between VCF and band 1, 2, 3, 4, 5 and 7
plot(VCF ~ band1, data = df, pch = ".", col = "orange", 
     main="Correlation between VCF and LandSat bands", 
     xlab="Electromagnetic Spectrum of the Landsat imagery", ylab="VCF [%]", 
     xlim=c(0,50000), ylim=c(0,100))

points(VCF ~ band2, data = df, pch = ".", col = "dark green")
points(VCF ~ band3, data = df, pch = ".", col = "dark blue")
points(VCF ~ band4, data = df, pch = ".", col = "grey")
points(VCF ~ band5, data = df, pch = ".", col = "black")
points(VCF ~ band7, data = df, pch = ".", col = "purple")
legend(0.37, 100, box.col='white', legend=c("band 1", "band 2", "band 3", "band 4", "band 5", "band 7"),
       fill=c("dark green", "dark blue", "grey", "black", "purple"), bg="white")




# •produce one or more plots that demonstrate the relationship between...
# the Landsat bands and the VCF tree cover.
pairs(alldata)
# Conclusion: 

## extract all data to a data.frame
df <- as.data.frame(getValues(alldata))

head(df)

model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = df)
summary(model)
# Use the model to predict land cover
lcMap <- predict(alldata, model = model)

levelplot(lcMap, col.regions = c('green', 'brown', 'darkgreen', 'lightgreen', 'grey', 'blue'))

?lm
GEWATA.lm <- lm(formula = band4 ~ VCF, data = df)
GEWATA.lm


# voorbeeld om RMSE te berekenen
RMSE <- sqrt(mean((y-y_pred)^2))


summary(GEWATA.lm)$r.sqaured

# download the data (BONUS)
dir.create("./output", showWarnings = FALSE)



