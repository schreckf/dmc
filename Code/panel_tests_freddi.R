######################################################################

### Preparations

rm(list = ls())
graphics.off()

# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

# Import datasets
items <- read.csv(file = "../Data/items.csv", sep = "|")
prices <- read.csv(file = "../Data/prices.csv", sep = "|")
train <- read.csv(file = "../Data/train.csv", sep = "|")

# In order to have some more features in training dataset,
# connecting training dataset with features from items dataset and prices dataset (in 2 steps)
train.new <- merge(x = train, y=items, by.x = c("pid", "size"), by.y = c("pid", "size"))
train.new <- subset(train.new, select = -stock) # this information does not apply on training data

#Create a sequence of dates from 2017-10-01 to 2018-02-28---------
my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")
# Change colnames of prices dataframe to dates
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

# Merge with prices data
train.new <- merge(x = train.new, y = prices, by.x = c("pid", "size"), by.y = c("pid", "size"))
items.new <- merge(x = items, y=prices, by.x = c("pid", "size"), by.y = c("pid", "size"))


######################################################################

# Panel Approach & Recurrent Neural Network

library("rnn")
library("plm")

### Prepare Panel structured dataset


# For now we use only sample of train.new here (speedup)
train.new <- train.new[1:1000, ]



# Generate ID for the c(pid, size)-defined products
train.new$id <- cumsum(!duplicated(train.new[c("pid", "size")]))

# Decide which timevariable to take-->dup, dunp or date???

# Generate Panel ID-variable, time-variable and purchase variable
panel.id <- matrix(t(matrix(data = rep(unique(train.new$id), NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel.time <- rep(my.Dates, max(train.new$id))
panel.purchases <- rep(0, NROW(panel.time)) # PROBLEM: for some we dont have data from the start--> better NA instead of 0
# AND: from first of february on we dont know purchases--> NA

panel <- cbind.data.frame(panel.purchases, panel.id, panel.time)

# Generate number of purchases at each date
for (i in 1:NROW(train.new)) {
  panel[panel$panel.id == train.new[i, "id"] & panel$panel.time == as.Date(train.new[i, "date"]),]$panel.purchases <- train.new[i,]$units
}

# Set NA if time is before release date of product
panel$panel.releaseDate <- as.Date(rep(NA, NROW(panel.time)))
for (i in unique(panel$panel.id)) {
  panel[panel$panel.id == i, "panel.releaseDate"] <- train.new[train.new$id == i, "releaseDate"][1]
}
panel[(panel$panel.time < panel$panel.releaseDate) == TRUE, ]$panel.purchases <- NA



# Bring price variable into panel structure
panel$panel.price <- rep(NA, NROW(panel.time))
for (i in unique(train.new$id)) {
  panel[panel$panel.id == i,]$panel.price <- c(unique(train.new[train.new$id == i, 12:162]))
}
panel$panel.price <- as.numeric(panel$panel.price)

# Bring other variables into panel structure
panel$panel.color <- matrix(t(matrix(data = rep(train.new$color[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.color <- as.factor(panel$panel.color)
panel$panel.brand <- matrix(t(matrix(data = rep(train.new$brand[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.brand <- as.factor(panel$panel.brand)
panel$panel.size <- matrix(t(matrix(data = rep(train.new$size[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.size <- as.factor(panel$panel.size)
panel$panel.rrp <- matrix(t(matrix(data = rep(train.new$rrp[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.mainCategory <- matrix(t(matrix(data = rep(train.new$mainCategory[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.mainCategory <- as.factor(panel$panel.mainCategory)
panel$panel.category <- matrix(t(matrix(data = rep(train.new$category[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.category <- as.factor(panel$panel.category)
panel$panel.subCategory <- matrix(t(matrix(data = rep(train.new$subCategory[unique(train.new$id)], NROW(my.Dates)), ncol = NROW(my.Dates))), ncol = 1)
panel$panel.subCategory <- as.factor(panel$panel.subCategory)


### Split into train and test
set.seed(333)
s <- sample(unique(panel$panel.id), size = 100) # sample of 100 products

training <- subset(panel, panel.id %in% s)
testing <- subset(panel, !(panel.id %in% s))

### Build Panel data models


# Set data as panel data
pdata <- pdata.frame(training, index = c("panel.id", "panel.time"))

# Pooled OLS estimator
pooling <- plm(panel.purchases ~ panel.color + panel.brand + panel.subCategory +
                 panel.rrp + panel.size + panel.category + panel.mainCategory +
                 panel.price, data = pdata, model = "pooling")
summary(pooling)

# Between estimator
between <- plm(panel.purchases ~ panel.color + panel.brand + panel.subCategory +
                 panel.rrp + panel.size + panel.category + panel.mainCategory +
                 panel.price, data = pdata, model = "between")
summary(between)

# First Difference estimator
fd <- plm(panel.purchases ~ panel.color + panel.brand + panel.subCategory +
                 panel.rrp + panel.size + panel.category + panel.mainCategory +
                 panel.price, data = pdata, model = "fd")
summary(fd)

# Fixed Effects (or Withing) estimator
fixed <- plm(panel.purchases ~ panel.color + panel.brand + panel.subCategory +
                 panel.rrp + panel.size + panel.category + panel.mainCategory +
                 panel.price, data = pdata, model = "within")
summary(fixed)

# Random Effects estimator
random <- plm(panel.purchases ~ panel.color + panel.brand + panel.subCategory +
                panel.rrp + panel.size + panel.category + panel.mainCategory +
                panel.price, data = pdata, model = "random")
summary(random)


# LM test on random effects vs OLS
plmtest(pooling)

# LM test on fixed effects vs OLS
pFtest(fixed, pooling)

# Hausman test on fixed effects vs random effects
phtest(random, fixed) # -->random effects model probably best


### Prediction with predict-function on testing data
...


######################################################################

# Multiple auto-ARIMAs (using only time, price and google data)

# First: generate individual time series data structure 
data.list <- list()
for (i in unique(panel$panel.id)) {
  data.list[[i]] <- panel[panel$panel.id == i,]
}
# Drop all cases where panel.time < releaseDate
for (i in unique(panel$panel.id)) {
  data.list[[i]] <- data.list[[i]][!is.na(data.list[[i]]$panel.purchases), ]
}


# Second: Model building
...


######################################################################

# Multiple zero-inflated Poisson time series

library("ZIM") # for zero-inflated models









