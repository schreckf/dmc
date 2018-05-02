library(rstudioapi)
library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)
library(xts)
library(forecast)


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

##Turn data back to long format
data.ts <- melt(data.ts, id= "date")

data.ts <- data.ts %>%
separate(variable, c("pid", "size"), "_")  
colnames(data.ts) <- c("date", "pid", "size", "sales")

#Add prices to train data
data.ts <- merge(data.ts, prices.ts, by.x = c("date", "pid", "size"), by.y = c("date", "pid", "size"))
data.ts <- merge(data.ts, items, by.x = c("pid", "size"), by.y = c("pid", "size"))

str(data.ts)

data.ts$pid <- as.factor(data.ts$pid)
data.ts$size <- as.factor(data.ts$size)

summary(data.ts)
summary(data.ts$size)

####Try to collapse size variable
items <- as.data.table(items)
setkey(items, "pid", "size", "color", "brand", "rrp", "mainCategory", "category", "subCategory")

items.sizes <- items[!duplicated(items$size), c("size", "brand", "mainCategory", "category", "subCategory")]


#Turn into time series object
data.xts <- xts(data.ts[,-1], order.by = data.ts$date)
str(data.xts)
str(data.xts[,1550])
head(data.xts[,1550], 10)
plot(data.ts[,c(1,10834)])

str(data.xts)

#ARIMA model
library(doParallel)

cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
output <- lapply(data.xts, function(x) auto.arima(x))
stopCluster
(cl)

output.forecast <- lapply(output, function(x) forecast(x, h = 28))
head(output.forecast)


length(data.ts$date)
plot(x = data.ts$date, data.ts$`15845_M`, type = 'l')
points(data.ts$date, prices.ts.tr$`15845_M`)


#######Exploratory Data Analysis---------
total.daily.sales <- cbind.data.frame(data.ts[,1] , rowSums(data.ts[,-1], na.rm = TRUE))
colnames(total.daily.sales) <- c("date", "sales")
total.daily.sales$day <- weekdays(as.POSIXct(total.daily.sales$date))
total.daily.sales$month <- months(as.POSIXct(total.daily.sales$date))
plot(total.daily.sales[,c(1,2)], type = "l")
 
library(vcd)
library(tidyr)
library("dplyr")
table1 <- as.data.table(total.daily.sales[,-1])
setkey(table1, "day", "month")

table2 <- table1[ , ceiling(mean(sales)), by = c("day", "month")]
table2 <- spread(table2, month, V1)
str(table2)

table2[order(table2$day),] 

mat_data <- data.matrix(table2[,2:ncol(table2)])

rownames(mat_data) <- table2$day 
mat_data

mat_data <- mat_data[order(rownames(mat_data)),]
rownames(mat_data)
library(gplots)
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(1000,2000,length=100), # for red
               seq(2001,3000,length=100),  # for yellow
               seq(3001,5000,length=100)) # for green

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = "Sales by day of month", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering            
