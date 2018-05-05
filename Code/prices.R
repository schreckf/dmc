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

#Missing value imputation by auto.arima & Kalman Filter. It takes some time.

prices_only <- prices[,-c(1,2)]

for(i in rownames(prices_only)){
  prices_only[i,] <- na.kalman(as.numeric(prices_only[i,]), model = "auto.arima")
}

prices <- cbind(prices[,c(1,2)], prices_only)

#Create a sequence of dates from 2017-10-01 to 2018-02-28---------
my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")

#CHange colnames of prices dataframe to dates
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

#Put date vavriables into the rows of the prices dataframe 
prices <- melt(prices, id.vars = c("pid", "size"))
colnames(prices)[colnames(prices)=="variable"] <- "date"
colnames(prices)[colnames(prices)=="value"] <- "price"

#Convert column into date format
prices$date <- as.Date(prices$date)

#Rearrange dataframe so dates are on the rows, each item is its own variable 
#and prices for each item are shown over time
prices <- dcast(prices, date ~ pid + size, value.var = "price")
str(prices)

#FIlter out the February prices
prices.ts <- prices[prices$date <= "2018-01-31",]

#Turn price data back to long form
prices.ts <- melt(prices.ts, id= "date")
prices.ts <- prices.ts %>%
  separate(variable, c("pid", "size"), "_") 

colnames(prices.ts) <- c("date", "pid", "size", "price")




