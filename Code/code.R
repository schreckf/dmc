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


# Generate ID for the c(pid, size)-defined products
train.new$id <- cumsum(!duplicated(train.new[c("pid", "size")]))
items <- unique(merge(x = items, y = train.new[, c("id", "pid", "size")], by.x = c("pid", "size"), by.y = c("pid", "size")))




# Variable giving the days until purchase from release date
train.new$time <- round(difftime(strptime(train.new$date, format = "%Y-%m-%d"),
                        strptime(train.new$releaseDate, format = "%Y-%m-%d"),units= "days"),digits = 0)

# Variable giving the days since last purchase date

train.new <- merge(train.new, 
                   train.new[order(train.new$date),c(1,2,3)] %>%
                     group_by(pid, size) %>%
                     mutate(time_last =  c(NA, diff(date))),
              by = c("pid", "size","date"))

# Generate variable indicating releaseDate 2007-10-01
#train.new$releaseIndicator <- ifelse(is.na(train.new$time_last), 1, 0)

#train.new$time_last[is.na(train.new$time_last)] <- difftime(train.new$date[is.na(train.new$time_last)], train.new$releaseDate[is.na(train.new$time_last)], units = "days")
#train.new$time_last <- as.integer(train.new$time_last)


# Add average price
avg.price <- apply(prices[-c(1,2)], 1, function(x) mean(x))
prices$avg.price <- avg.price
prices.subset <- prices[, c("pid", "size", "avg.price")]

train.new <- merge(x = train.new, y=prices.subset, by.x = c("pid", "size"), by.y = c("pid", "size"))

# Predict prices to impute NAs
linear.price.model <- lm(formula = avg.price ~ subCategory + category + mainCategory + rrp + color + 
                          brand , data = train.new[!is.na(train.new$avg.price),])
summary(linear.price.model)

p.prices <- predict(object = linear.price.model, newdata = train.new[is.na(train.new$avg.price),])
train.new[is.na(train.new$avg.price),]$avg.price <- p.prices

items <- unique(merge(x = items, y=train.new[, c("pid", "size", "avg.price")], by.x = c("pid", "size"), by.y = c("pid", "size")))

# Calculate discount

train.new$discount.raise <- 100*(train.new$rrp - train.new$avg.price)/train.new$rrp
items$discount.raise <- 100*(items$rrp - items$avg.price)/items$rrp




#------------------------------------------------------------------
# Feature Selection
#------------------------------------------------------------------


#------------------------------------------------------------------
# Model Building
#------------------------------------------------------------------

# Splitting train.new-dataset into training and validation data from January on
sum(as.Date(train.new$date) < "2018-01-01") / NROW(train.new$date) # --> it's a 73/27 split
training <- train.new[as.Date(train.new$date) < "2018-01-01", ]
validation <- train.new[as.Date(train.new$date) >= "2018-01-01", ]


# Naive Model evaluation (We should be better than this)
validation$pred.naive <- "2018-01-16"
validation$error <- as.integer(abs(difftime(validation$date, validation$pred.naive, units = "days"))) + 1
avg.naive.error <- sum(validation$error[!is.na(validation$error)]) / NROW(!is.na(validation$error)); avg.naive.error #  8.81961
avg.naive.error2 <- sqrt(sum(validation$error[!is.na(validation$error)])) / NROW(!is.na(validation$error)); avg.naive.error2 #  0.01561617

### Survival analysis: Cox proportional hazard model 
source("survival_model.R")


#------------------------------------------------------------------
# Model selection and prediction
#------------------------------------------------------------------


