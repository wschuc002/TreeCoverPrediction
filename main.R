# Student: William Schuch & Rik van Berkum
# Team: Geodetic Engineers of Utrecht
# Institute: Wageningen University and Research
# Course: Geo-scripting (GRS-33806)
# Date: 2016-01-13
# Week 2, Lesson 8: Advanced Raster Analysis

rm(list = ls())  # Clear the workspace!
ls() ## no objects left in the workspace
start.time <- Sys.time()

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
#source("./R/RMSE_calculation.R")

# Loading the data into memory (band 6 (thermal infra-red) will be excluded from this exercise)
load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")

load("data/trainingPoly.rda")

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

# Create rasterbrick (na.rm=T)
alldata2 <- as.data.frame(alldata, na.rm=T)

model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, alldata2, na.action = na.omit)
summary(model)

# Predicting VCF based on linear regression model
VCFpredict <- predict(alldata, model = model, na.rm=T)
names(VCFpredict) <- "VCF"
VCFpredict[VCFpredict < 0] <- NA
VCFpredict[VCFpredict > 100] <- NA

par(mfrow = c(1, 1)) # reset plotting window

# Comparing predicted and original VCF
GreenToRed <- rev(colorRampPalette(c("darkgreen","yellow","dark red"))(20))

plot_org <- spplot(alldata$VCF, main="Original tree cover (VCF)", col.regions = GreenToRed,
                   scales = list(draw = TRUE))

plot_pre <- spplot(VCFpredict, main="Predicted tree cover (VCF)", col.regions = GreenToRed,
                   scales = list(draw = TRUE))

print(plot_org, position = c(0.0, 0.5, 1.0, 1.0), more = T)
print(plot_pre, position = c(0.0, 0.0, 1.0, 0.5), more = F)

## _ _ _ _
## Calculate the Root Mean Square Error (RMSE): spatial approach

SD <- (alldata$VCF-VCFpredict)^2

RedToGreen <- rev(colorRampPalette(c("dark red","yellow","darkgreen"))(20))
spplot(SD, main="Squared Differences: Original VCF vs. Predicted VCF", col.regions = RedToGreen,
       scales = list(draw = TRUE))

mean_square <- cellStats(SD, stat = 'mean')
RMSE <- sqrt(mean_square)
paste("The Root Mean Square Error (RMSE) is:", format(round(RMSE,3), nsmall = 2))
## _ _ _ _

## calculate the RMSE for the classes separately, by using training data (a priori).
# we can convert to integer by using the as.numeric() function, 
# which takes the factor levels
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
classes <- rasterize(trainingPoly, SD, field='Code')

# zonal statistics to calculate RMSE of per class
mat_mean <- zonal(SD, classes, fun = 'mean', na.rm=T)
RMSE_classes <- sqrt(mat_mean[,2])
stat_df <- as.data.frame(mat_mean)
stat_df$RMSE <- RMSE_classes
stat_df$class <- c("Cropland","Forest","Wetland")
stat_df

## _ _ _ _
## finishing the script and calculate running time of the script
end.time <- Sys.time()
time.taken <- end.time - start.time
paste("The script has finished running in", time.taken, "seconds.")