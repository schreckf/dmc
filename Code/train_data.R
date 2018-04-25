library(rstudioapi)
library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)

# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

data = read.csv(file = "train.csv",header = TRUE, sep = "|")
items = read.csv(file = "items.csv",header = TRUE, sep = "|")
prices = read.csv(file = "prices.csv", header = TRUE, sep = "|")

#Convert column into date format
data$date <- as.Date(data$date)

#Put date vavriables into the rows of the train dataframe 
data.ts <- dcast(data, date ~ pid + size, value.var = "units")
str(data.ts)

#Turn NAs into 0
data.ts[is.na(data.ts)] <- 0
str(data.ts)

#Turn the 0 to NAs if the date is before therelease date of the product

plot(data.ts[,c(1,10834)])
