library(rstudioapi)
# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

data = read.csv(file = "train.csv",header = TRUE, sep = "|")
items = read.csv(file = "items.csv",header = TRUE, sep = "|")
prices = read.csv(file = "prices.csv", header = TRUE, sep = "|")

library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)
library(xts)
library(dplyr)
library(tidyr)

#Create a sequence of dates from 2017-10-01 to 2018-02-28---------
my.Dates <- seq(as.Date("2017-10-01"), as.Date("2018-02-28"), by = "day")

#CHange colnames of prices dataframe to dates
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

#Put date vavriables into the rows of the prices dataframe 
prices <- melt(prices, id.vars = c("pid", "size"))
colnames(prices)[colnames(prices)=="variable"] <- "date"
colnames(prices)[colnames(prices)=="value"] <- "price"


#Convert column into date format
prices$date <- as.character(prices$date)
prices$date <- as.Date(prices$date, format = "%Y-%m-%d")

#Convert PID to factor
prices$pid <- as.factor(prices$pid)
str(prices$date)

#Rearrange dataframe so dates are on the rows, each item is its own variable 
#and prices for each item are shown over time
prices.ts <- dcast(prices, date ~ pid + size, value.var = "price")
str(prices.ts)





