library(rstudioapi)
library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)
library(xts)

# (Flexible) working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uses package rstudioapi

data = read.csv(file = "train.csv",header = TRUE, sep = "|")
items = read.csv(file = "items.csv",header = TRUE, sep = "|")
prices = read.csv(file = "prices.csv", header = TRUE, sep = "|")

#Turn items release date to date and other columns to factors
items[c("pid", "mainCategory", "category", "subCategory")] <- lapply(items[c("pid", "mainCategory", "category", "subCategory")], factor)
items["releaseDate"] <- as.Date(items$releaseDate)
str(items)

#Convert column into date format
data$date <- as.Date(data$date)

#Put date vavriables into the rows of the train dataframe 
data.ts <- dcast(data, date ~ pid + size, value.var = "units")
str(data.ts)

#Turn NAs into 0
data.ts[is.na(data.ts)] <- 0
str(data.ts)

#Turn the 0 to NAs if the date is before therelease date of the product
releaseDate.check <- cbind.data.frame(paste(items$pid, items$size, sep = "_"), items$releaseDate)
colnames(releaseDate.check) <- c("pid_size", "releaseDate")
str(releaseDate.check)
head(releaseDate.check)


for (i in 2:12825) {
  a <- data.ts[,c(1,i)]
a[,2] <- ifelse(a$date < releaseDate.check$releaseDate[i-1], NA, a[,2])
data.ts[,i] <- a[,2]
}



#Turn into time series object
data.xts <- xts(data.ts, order.by = data.ts$date)
str(data.xts)
head(data.xts[,1550], 10)
plot(data.ts[,c(1,10834)])
