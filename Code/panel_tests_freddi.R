# Trying some arima stuff


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
train.new <- merge(x = train.new, y = prices, by.x = c("pid", "size"), by.y = c("pid", "size"))
train.new <- subset(train.new, select = -stock) # this information does not apply on training data

# Furthermore connect the items data with the price data
items.new <- merge(x = items, y=prices, by.x = c("pid", "size"), by.y = c("pid", "size"))


######################################################################

# Multivariate ARIMA for each product individually
library("forecast")

### Data preparation (analogously to prices.R)

#split traindataset


######################################################################

# Vector Autoregression model

######################################################################

# Panel Approach: Recurrent Neural Network

#install.packages("rnn")
#library("rnn")
library("plm")

### Dataset preparation

# Step1: Generate ID for the c(pid, size)-defined products
train.new$id <- cumsum(!duplicated(train.new[c("pid", "size")]))

# Step2: decide which timevariable to take-->dup, dunp or date?

# Step3: make panel datastructure
id.vector <- train.new$id

id.panel <- c()


for (i in 1:length(id.vector)) {
  x <- rep(id.vector[i], NCOL(prices))
  id.panel <- append(id.panel, x)
}









