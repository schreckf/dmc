# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

data = read.csv(file = "train.csv",header = TRUE, sep = "|")
items = read.csv(file = "items.csv",header = TRUE, sep = "|")
prices = read.csv(file = "prices.csv", header = TRUE, sep = "|")

library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)

#Create a sequence of dates from 2017-10-01 to 2018-02-28---------
my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")

#CHange colnames of prices dataframe to dates
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

#Put date vavriables into the rows of the prices dataframe 
prices.ts <- melt(prices, id.vars = c("pid", "size"))
colnames(prices.ts)[colnames(prices.ts)=="variable"] <- "date"
colnames(prices.ts)[colnames(prices.ts)=="value"] <- "price"

#Convert column into date format
prices.ts$date <- as.Date(prices.ts$date)

#Rearrange dataframe so dates are on the rows, each item is its own variable 
#and prices for each item are shown over time
prices.ts <- dcast(prices.ts, date ~ pid + size, value.var = "price")
str(prices.ts)

#FIlter out the February prices
prices.ts.tr <- prices.ts[prices.ts$date <= "2018-01-31",]






