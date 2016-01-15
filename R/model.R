# Loading the data into memory (band 6 (thermal infra-red) will be excluded from this exercise)
load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")

# Transform data
vcfGewata[vcfGewata > 100] <- NA
rasterbrick <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
rasterbrick <- calc(rasterbrick, fun=function(x) x / 10000)
rasterbrick <- addLayer(rasterbrick, vcfGewata)
names(rasterbrick) <- c("Band1","Band2", "Band3", "Band4", "Band5", "Band7", "VCF")

# Create rasterbrick
rasterbrickData <- getValues(rasterbrick)
rasterbrickData <- as.data.frame(rasterbrickData, na.rm=T)

#pairs(rasterbrick)

model <- lm(VCF ~ Band1+Band2+Band3+Band4+Band5+Band7, rasterbrickData, na.action=na.omit)

summary(model)

# Predicting VCF based on linear regression model
VCFpredict <- predict(rasterbrick, model = model, na.rm=TRUE)
names(VCFpredict) <- "VCF"
VCFpredict[VCFpredict < 0] <- NA
VCFpredict[VCFpredict > 100] <- NA


# plotting the difference

opar <- par(mfrow=c(1, 2)) # allow 2 plots side-by-side
plot1 <- plot(rasterbrick$VCF)
plot2 <- plot(VCFpredict)


# # plotting the difference
# colorPal <- rev(colorRampPalette(c("darkgreen","yellow","brown"))(20)) # Create color palette
# 
# opar <- par(mfrow=c(1, 2)) # allow 2 plots side-by-side
# plot1 <- spplot(rasterbrick$VCF, main="Original VCF", col.regions=colorPal)
# plot2 <- spplot(VCFpredict, main="Predicted VCF", col.regions=colorPal)
# 

# Calculate RMSE
RMSEpredict <- calculate.RMSE(rasterbrick$VCF, VCFpredict)


#RMSE <- sqrt(mean((rasterbrick$VCF-VCFpredict)^2))


RMSE
plot(RMSE)




# Create trainingareas of the original and predicted values, and adding their classes.
training <- mask.trainingdata(rasterbrick$VCF, trainingPoly)
predictedtraining <- mask.trainingdata(VCFpredict, trainingPoly)

# Calculating RMSE per class
RMSE_crop <- calculate.RMSE.class(trainingOri, trainingPred,1)
RMSE_for <- calculate.RMSE.class(trainingOri, trainingPred,2)
RMSE_wet <- calculate.RMSE.class(trainingOri, trainingPred,3)





