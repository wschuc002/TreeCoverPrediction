# load
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")

# load vcf
load("data/vcfGewata.rda")
vcfGewata[vcfGewata > 100] <- NA
#summary(vcfGewata)



# put the 3 bands into a rasterBrick object to summarize together
gewata <- brick(GewataB1, GewataB5, GewataB7)
# 3 histograms in one window (automatic, if a rasterBrick is supplied)
par(mfrow = c(1, 1)) # reset plotting window
hist(gewata)

summary(gewata)

par(mfrow = c(1, 1)) # reset plotting window
hist(gewata, xlim = c(0, 5000), ylim = c(0, 500000), breaks = seq(0, 500000, by = 100))

pairs(gewata)
?pairs


# random forest

if(!require(randomForest)) {
  install.packages("randomForest")
}

library(randomForest)

summary(df)

?lm
lm(GewataB1, data = df)
# Calibrate model
model <- randomForest(lc ~ band1 + band2 + band3 + band4 + band5 + band6 + band7, data = calib2)
#model <- lm(band1 + band2 + band3 + band4 + band5 + band6 + band7, data = df)
# Use the model to predict land cover
lcMap <- predict(wagLandsatCrop, model = model)


