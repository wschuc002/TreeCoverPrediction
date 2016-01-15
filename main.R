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
library(RColorBrewer)

# referring to functions in R folder
source("./R/RMSE_calculation.R")

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

# •produce one or more plots that demonstrate the relationship between...
# the Landsat bands and the VCF tree cover.
for(i in 1:6){
  pairs(alldata[[c(i,7)]], maxpixels = 10000)
}
# Conclusion: 

## extract all data to a data.frame
df <- as.data.frame(getValues(alldata))

head(df)

model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = df)
model

summary(model)
# Use the model to predict land cover
lcMap <- predict(alldata, model = model)

levelplot(lcMap, col.regions = c('green', 'brown', 'darkgreen', 'lightgreen', 'grey', 'blue'))


# Create rasterbrick
alldata2 <- as.data.frame(alldata, na.rm=T)

model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, alldata2, na.action = na.omit)
summary(model)

# Predicting VCF based on linear regression model
VCFpredict <- predict(alldata, model = model, na.rm=T)
names(VCFpredict) <- "VCF"
VCFpredict[VCFpredict < 0] <- NA
VCFpredict[VCFpredict > 100] <- NA

# Comparing predicted and original VCF
colorPal <- rev(colorRampPalette(c("darkgreen","yellow","brown"))(20)) # Create color palette
plot1 <- spplot(alldata$VCF, main="Original VCF", col.regions = colorPal, 
              sp.layout = list(list("SpatialPolygonsRescale", layout.north.arrow(), #Create north arrow
                                  offset=c(850000, 845000), scale=7000, fill=c('white','black')), 
                             list("SpatialPolygonsRescale", layout.scale.bar(), #Create scale bar
                                  offset=c(842000, 820000), scale=10000, fill=c('white','black')),
                             list("sp.text", c(842000, 821500), "0", font=2), #Add text to scale bar
                             list("sp.text", c(852000, 821500), "10 km", font=2)))
plot2<-spplot(VCFpredict,main="Predicted VCF", col.regions = colorPal, 
              sp.layout = list(list("SpatialPolygonsRescale", layout.north.arrow(), 
                                  offset=c(850000, 845000), scale=7000, fill=c('white','black')), 
                             list("SpatialPolygonsRescale", layout.scale.bar(),
                                  offset=c(842000, 820000), scale=10000, fill=c('white','black')),
                             list("sp.text", c(842000, 821500), "0", font=2),
                             list("sp.text", c(852000, 821500), "10 km", font=2)))
print(plot1, position = c(0,0,.5,1),more = T)
print(plot2, position = c(.5,0,1,1),more = T)





RMSE <- RMSE_calculation(alldata$VCF, VCFpredict)

paste("The RMSE is", format(round(RMSEpredict,2), nsmall = 2))


summary(GEWATA.lm)$r.sqaured

# download the data (BONUS)
dir.create("./output", showWarnings = FALSE)



