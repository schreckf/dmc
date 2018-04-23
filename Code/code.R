###################################################################
# DataMiningCup 2018
###################################################################


#------------------------------------------------------------------
# Preparing environment
#------------------------------------------------------------------

rm(list = ls())
graphics.off()

# Install Packages
#install.packages("survival") # survival analysis
library("survival")
#install.packages("pec") # pec::predictSurvProb() for prediction in survival analysis
library("pec")
#install.packages("rstudioapi") # allows adaptive working directory 
library("rstudioapi")

# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

# Import datasets
items <- read.csv(file = "../Data/items.csv", sep = "|")
prices <- read.csv(file = "../Data/prices.csv", sep = "|")
train <- read.csv(file = "../Data/train.csv", sep = "|")

# In order to have some more features in training dataset,
# connecting training dataset with features from items dataset and prices dataset (in 2 steps)
train.new <- merge(x = train, y=items, by.x = c("pid", "size"), by.y = c("pid", "size"))
train.new <- merge(x = train.new, y = prices, by.x = c("pid", "size"), by.y = c("pid", "size"))

# Furthermore connect the items data with the price data
items.new <- merge(x = items, y=prices, by.x = c("pid", "size"), by.y = c("pid", "size"))


#------------------------------------------------------------------
# Feature Engineering
#------------------------------------------------------------------

### Variable fixing


### Variable generation

# Variable giving the days until purchase from release date
train.new$time <- round(difftime(strptime(train.new$date, format = "%Y-%m-%d"),
                        strptime(train.new$releaseDate, format = "%Y-%m-%d"),units= "days"),digits = 0)
train.new$time <- as.integer(train.new$time)

# Event-variable, needed for survival analysis (1 for all cases)
train.new$event <- rep(1, NROW(train.new))

# TO DO: Variable giving the days until next purchase (means counter set to zero after purchase of similar product)  


### Category handling


#------------------------------------------------------------------
# Feature Selection
#------------------------------------------------------------------


#------------------------------------------------------------------
# Model Building
#------------------------------------------------------------------

### Survival analysis: Cox proportional hazard model 

# Model
coxmodel <- coxph(Surv(time, event) ~ color + rrp + brand + category, 
                   data = train.new)
summary(coxmodel)

# Plotting
plot(survfit(coxmodel, type = "aalen"), xlab = "Time", ylab = "Survival Probability")
plot(survfit(Surv(time, event) ~ color, data = train.new), xlab = "Time",
     ylab = "Survival Probability", col = train.new$color) # exemplary plot depending on color-variable


# Prediction (using package pec)
# Extract predicted survival probabilities 
# at selected time-points
time.points <- c(1:150)
prob.surv <- predictSurvProb(object = coxmodel, newdata = items.new, times = time.points)
head(prob.surv)
# Problem: no prediction over future timepoints (>122) that did not occur in training data. 
# --> Solution: Possibly better use "days until next purchase" as time variable rather than "days until purchase".

# Adding the predictions for new data to the plot
lines(survfit(coxmodel, newdata=items.new))



#------------------------------------------------------------------
# Model selection and prediction
#------------------------------------------------------------------


