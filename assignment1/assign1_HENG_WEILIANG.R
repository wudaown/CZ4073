# Read the data from file
wineData <- read.csv("assign1_WineData.csv", header = TRUE)

# Dimension of the dataset 
dim(wineData)

# Labels of the data 
names(wineData)

# structure of data, type of each label
str(wineData)

# To view the first few data
head(wineData)

# To view the last few data
tail(wineData)

# To get the summary stastic of the data
summary(wineData)

# Look at histogram
hist(wineData$FixedAcidity)
hist(wineData$VolatileAcidity, breaks = seq(0, 1.4, by=0.05))
# seems to have a outliers at 1.0
hist(wineData$CitricAcid, breaks = seq(0,1, by = 0.05)) 

# most of the points are around 0 to 10.
# Beyond 10 seems to be candidate of outliers
hist(wineData$ResidualSugar, breaks= seq(0, 15.5, by=0.1))

hist(wineData$Chlorides)

hist(wineData$FreeSulphurDioxide)

# seems to have a outliers at farend
hist(wineData$TotalSulphurDioxide, breaks= seq(0, 289, by=1.0))

hist(wineData$Density)

# at both end seems to be outliers
hist(wineData$pH, breaks = seq(2.74, 4.01, by=0.01))

# at the farend seems to be outliers
hist(wineData$Sulphates, breaks = seq(0.37, 2.0, by = 0.01))

# at the farend seems to be outliers
hist(wineData$Alcohol, breaks = seq(8.4, 14.9, by = 0.1))

hist(wineData$Quality, breaks = seq(3, 8, by=1))
# By looking at the histogram, most of the data seems to have some outliers
# that may affect the result later on. The next section will be using boxplot
# to have a closer look the data for examing outliers.


# Indeed there is some outliers in FixedAcidity, I then generate a subset to reduce the subset
# in case I need these data
boxplot(wineData$FixedAcidity, horizontal = TRUE)
subFixedAcidity =wineData$FixedAcidity[wineData$FixedAcidity < 12.3]
boxplot(subFixedAcidity, horizontal = TRUE)

# The same apply to volatile acidity, outliers are presented
boxplot(wineData$VolatileAcidity, horizontal = TRUE)
subVolatitleAcidity = wineData$VolatileAcidity[wineData$VolatileAcidity < 1.1]
boxplot(subFixedAcidity, horizontal = TRUE)

# The same apply to citric acid, outliers are presented
boxplot(wineData$CitricAcid, horizontal = TRUE)
subCitricAcid = wineData$CitricAcid[wineData$CitricAcid < 0.9]
boxplot(subCitricAcid)

# in the case for residual sugar, a lot more outliers are presented
boxplot(wineData$ResidualSugar, horizontal = TRUE)
subResidualSugar = wineData$ResidualSugar[wineData$ResidualSugar < 4]
boxplot(subResidualSugar)

# in the case for chlorides, a lot more outliers are presented
boxplot(wineData$Chlorides, horizontal = TRUE)
subChlorides = wineData$Chlorides[wineData$Chlorides < 0.13]
boxplot(subChlorides, horizontal = TRUE)

# outliers are presented too
boxplot(wineData$FreeSulphurDioxide, horizontal = TRUE)
subChlorides = wineData$FreeSulphurDioxide[wineData$FreeSulphurDioxide < 42]
boxplot(subChlorides)

# outliers are presented too
boxplot(wineData$TotalSulphurDioxide, horizontal = TRUE)
subTotalSulphurDioxide = wineData$TotalSulphurDioxide[wineData$TotalSulphurDioxide < 125]
boxplot(subTotalSulphurDioxide)

# in the case of density, outliers are presented at both ends
# which is what I did not observed from the histogram
boxplot(wineData$Density, horizontal = TRUE)
subDensity = wineData$Density[wineData$Density > 0.992 & wineData$Density < 1.001] 
boxplot(subDensity, horizontal = TRUE)

# in the case of pH levelm outliers are presented at both ends
# which is what I observed from histogram
# also, the subset does not look useful
boxplot(wineData$pH, horizontal = TRUE)
subpH = wineData$pH[wineData$pH > 2.95 & wineData$pH < 3.65]
boxplot(subpH)

# outliers are at right end
boxplot(wineData$Sulphates, horizontal = TRUE)
subSulphates = wineData$Sulphates[wineData$Sulphates < 1.0]
boxplot(subSulphates)

# outliers are the the right end
boxplot(wineData$Alcohol, horizontal = TRUE)
subAlcohol = wineData$Alcohol[wineData$Alcohol < 13.3]
boxplot(subAlcohol)

# although two outlier at each end, they seems to be resonable 
# due to previous outliers which may generate very high and very low
# quality wines, not confirm yet
boxplot(wineData$Quality, horizontal = TRUE)
subQuality = wineData$Quality[wineData$Quality > 3 & wineData$Quality < 8]
boxplot(subQuality)

# Next I look at the correlation of the data
cor(wineData)
library(corrplot)
corrplot.mixed(cor(wineData))

# From the correlation data, quality seems to have a strong correlation with alcohol level
# follow by volatile acidity level. Quality seems to have some correlation with the rest of 
# the vairable not as strong though. 
# However, fixed acidity level seems to have strong linear correlation with citric acid,
# density, pH level. Also, volatitle acidity has strong linear correlation with citric acid.
# Citric acid has strong linear correlation with pH level.
# These indicates that some of the variable may need to be removed in the furture analysis.


# look at the plot of all data in pairs to confirm the previous correlation.
# I notice a strange relation about quality with other variable 
# They seems to be discrete value lying on the plot.
pairs(wineData, pch = 18)


# Initial analysis of data
lmfit1 = lm(Quality ~ ., data = wineData)
summary(lmfit1)
# Initial summary indicate that the Residual standard error being 0.6584 on a degree of freedom
# of 988. This residual standard error seems to be pretty good.
# However, the Multiple R-square only being 0.348 and adjusted R-square being 0.3407 which
# indicate that the variance explained are pretty low being 35%.
# The least significant variable seems to be Fixed Acidity having the p-value of 0.791192

# Remove fixed acidity
lmfit2 = update(lmfit1, ~.- FixedAcidity, data=wineData)
summary(lmfit2)
# Adjusted R-square does improve but not by a lot. 
# Current Adjusted R-square is 0.3413.
# At this point, summary indicates that the least significant variable to be
# Citric Acid with a p-value of 0.754303 and this variable is the next to be removed

# Remove Citric Acid
lmfit3 = update(lmfit2, ~.- CitricAcid, data=wineData)
summary(lmfit3)
# Adjusted R-square improve to 0.3419
# At this point, the least significant variable is ResidualSugar which has a 
# p-value of 0.331627

# Remove Residual Sugar
lmfit4 = update(lmfit3, ~. - ResidualSugar, data=wineData)
summary(lmfit4)
# Adjusted R-square improve to 0.342
# At this point, improvement are so much.
# Least significant variable is density with p-vaule of 0.483177
# I will try to remove some more and see what happens

# Remove density
lmfit5 = update(lmfit4, ~. - Density, data=wineData)
summary(lmfit5)
# Adjusted R-square improve to 0.3423
# A better improvement than last time
# Least significant variable is free sulphur dioxide with a 
# p-value of 0.044441 which is not bad, still I will try to remove it
# from the model

# Remove free sulphur dioxide
lmfit6 = update(lmfit5, ~. - FreeSulphurDioxide, data=wineData)
summary(lmfit6)
# By removing free sulphur dioxide, the Adjusted R-square actually
# decrease which is a bad sign meaning that I should not have removed
# free sulphur dioxide from the model.

# Next Step, I will use lmfit5 as my model and try to look for non-linear relation
# in the model
plot(wineData$VolatileAcidity, wineData$Quality)
plot(wineData$Chlorides, wineData$Quality)
plot(wineData$FreeSulphurDioxide, wineData$Quality)
plot(wineData$TotalSulphurDioxide, wineData$Quality)
plot(wineData$pH, wineData$Quality)
plot(wineData$Sulphates, wineData$Quality)
plot(wineData$Alcohol, wineData$Quality)

# I look at the plot from sulphate against quality 
# and think that the suqare plot would fit more
lmfit7 = update(lmfit5, ~ .+ I(Sulphates^(1/2)), data=wineData)
summary(lmfit7)

# Again, I think the square root plot would fit more
lmfit8 = update(lmfit7, ~ .+ I(pH^(1/2)), data=wineData)
summary(lmfit8)
# current adjusted R-square = 0.3693

# Remove outliers and high leverage points
cd = cooks.distance(lmfit8)
wineData.clean = wineData[abs(cd) < 4/nrow(wineData),]
nrow(wineData.clean)

# Fit the model again with the clean dataset
lmfit = lm(formula(lmfit8), data=wineData.clean)
formula(lmfit)
summary(lmfit)
# Final adjust R-square = 0.4407 
# Before outliers and high leverage point remove is it 0.3793 
plot(lmfit)


# ---------------- Second Question -------------------------
# Clear all data
rm(list=ls())

CarData <- read.csv("assign1_CarData.csv", header = TRUE)

dim(CarData)
names(CarData)
str(CarData)
# mpg = miles per gallon
head(CarData)
tail(CarData)
summary(CarData)

# Look at the histogram
# cylinder has extreme low value for 3 and 5
hist(CarData$cylinders, breaks = seq(2, 8, by = 1))

hist(CarData$displacement)

# some extreme value at right end, may be outliers
hist(CarData$horsepower, breaks = seq(45, 230, by = 1))

# Most of the points on weight are evently distribute
hist(CarData$weight, breaks = seq(1648, 5140, by = 1))

# Some extreme value at both ends, may be outliers
hist(CarData$acceleration, breaks = seq(7, 23.7, by = 0.1))

hist(CarData$mpg)

# Using box plot to check outliers
boxplot(CarData$cylinders, horizontal = TRUE)

boxplot(CarData$displacement, horizontal = TRUE)

# indeed outliers present
boxplot(CarData$horsepower, horizontal = TRUE)

boxplot(CarData$weight, horizontal = TRUE)

# indeed outliers present at both end
boxplot(CarData$acceleration, horizontal = TRUE)

boxplot(CarData$mpg, horizontal = TRUE)


# Next , I look at the correlation of the data
cor(CarData)
library(corrplot)
corrplot.mixed(cor(CarData))

# mpg has strong linear correlation with weight, displacement, cylinders and horsepower
# mpg also has linear correlation with acceleration though not so strong when compare
# to other variable
# In fact, all variable has a relative strong linear correlation with all other variable
# except acceleration.
# Though there is linear correlation between acceleration with other variable
# but so so strong when compare to others.
# It seems that acceleration would be the first to be removed from the model.

# from the pairs data, I observed that cylinders plot against other variable
# has a strange shape. seems discrete value rather.
# Linear line is able to fit, still need to take note of this.
pairs(CarData, pch = 18)


# Initial analysis of data
lmfit1 = lm(mpg ~ ., data = CarData)
summary(lmfit1)
# Residual standard error = 4.273
# degree of freedom = 294
# Multiple R-square = 0.7166
# Adjusted R-square = 0.7118
# The summary show that acceleration is indeed the least significant variable 
# p-value of acceleration = 0.8213
# However, cylinders has to be the next least significant variable, I suspect
# this is due to the shape from the plot of it against variable

# Remove acceleration from model
lmfit2 = update(lmfit1, ~. - acceleration, data=CarData)
summary(lmfit2)
# Adjusted R-square = 0.7127
# Adjusted R-square actually increase but not a lot.
# Also, cylinders now become the least significant variable
# cylinders p-value actually increase indicating that the present of
# acceleration reduce the effect of cylinders to model.
# Notice that the p-value of horsepower actually decrease, the presents of 
# acceleration increase the p-value of horsepower.

# Remove cylinders from model
lmfit3 = update(lmfit2, ~. - cylinders, data=CarData)
summary(lmfit3)
# By removing acceleration adjusted R-square did improve.
# Adjusted R-square = 0.7134
# But by much, at this point the displacement is least significant variable
# p-value = 0.2179
# Looking the the model with degrees of freedom of 296.
# I don't think I should contiune to remove displacement
# But no harm trying. So the next step I will remove it anyway to see

# Remove displacement from model
lmfit4 = update(lmfit3, ~ . - displacement, data=CarData)
summary(lmfit4)
# Indeed, after displacement is removed. Adjusted R-square decrease.
# Adjusted R-square = 0.7129
# So for the following, I will be using lmfit3 as the model to carry on.

# At this point, I would like to look at the graph and find if there exists 
# any non-linear relation
formula(lmfit3)
plot(CarData$displacement, CarData$mpg)
plot(CarData$horsepower, CarData$mpg)
plot(CarData$weight, CarData$mpg)
# In fact, from the plot I think all the reminding three variable seems 
# to have some non-linear relation with mpg.
# With displacement the strongest, follow by horsepower and weight.

lmfit5 = update(lmfit3, ~ . + displacement:horsepower, data=CarData)
summary(lmfit5)

lmfit6 = update(lmfit5, ~ . + displacement:weight, data=CarData)
summary(lmfit6)

lmfit7 = update(lmfit6, ~ . + horsepower:weight, data=CarData)
summary(lmfit7)
# For lmfit5, lmfit6 and lmfit7. By looking at the plot from previous section and 
# the corrplot displacement horsepower and weight they all have high linear relation with each other
# and from the plot, I think there are some non-linear relation with each of these three variable
# Hence I added them to the model

# Remove outliers and high leverage points
cd = cooks.distance(lmfit7)
CarData.clean = CarData[abs(cd) < 4/nrow(CarData),]
nrow(CarData.clean)

# Fit the model again with clean dataset
lmfit = lm(formula(lmfit7), data=CarData.clean)

summary(lmfit)
# Final Adjust R-square = 0.8323
# Before outliers and high leverage points remove is 0.7654

plot(lmfit)