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
#install.packages("dplyr") # handles data for feature engineering
library("dplyr")

# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

# Import datasets
items <- read.csv(file = "../Data/items.csv", sep = "|")
prices <- read.csv(file = "../Data/prices.csv", sep = "|")
train <- read.csv(file = "../Data/train.csv", sep = "|")

# In order to have some more features in training dataset,
# connecting training dataset with features from items dataset and prices dataset (in 2 steps)
train.new <- merge(x = train, y=items, by.x = c("pid", "size"), by.y = c("pid", "size"))
#train.new <- merge(x = train.new, y = prices, by.x = c("pid", "size"), by.y = c("pid", "size"))
train.new <- subset(train.new, select = -stock) # this information does not apply on training data

# Furthermore connect the items data with the price data
#items.new <- merge(x = items, y=prices, by.x = c("pid", "size"), by.y = c("pid", "size"))


#------------------------------------------------------------------
# Feature Engineering
#------------------------------------------------------------------

### Variable fixing


### Variable generation

# Variable giving the days until purchase from release date
train.new$time <- round(difftime(strptime(train.new$date, format = "%Y-%m-%d"),
                        strptime(train.new$releaseDate, format = "%Y-%m-%d"),units= "days"),digits = 0)

# Variable giving the days since last purchase date

train.new <- merge(train.new, 
                   train.new[order(train.new$date),c(1,2,3)] %>%
                     group_by(pid, size) %>%
                     mutate(time_last =  c(NA, diff(date))),
              by = c("pid", "size","date"))


### Category handling


#------------------------------------------------------------------
# Feature Selection
#------------------------------------------------------------------


#------------------------------------------------------------------
# Model Building
#------------------------------------------------------------------

### Survival analysis: Cox proportional hazard model 
source("survival_model.R")


#------------------------------------------------------------------
# Model selection and prediction
#------------------------------------------------------------------


