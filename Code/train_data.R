library(rstudioapi)
library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)
library(xts)
library(forecast)
library(forcats)


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

data.ts <- as.data.table(data.ts)
setkey(data.ts, "pid", "size", "color", "brand", "rrp", "mainCategory", "category", "subCategory")


items.k <- data.ts[,sum(sales, na.rm = TRUE), by = "size"]
items.sizes <- items[!duplicated(items$size), c("size", "brand", "mainCategory", "category", "subCategory")]

levels(data.ts$size)
data.ts$size <- fct_collapse(data.ts$size, 
             "L" = c("L ( 152-158 )", "L ( 40/42 )", "L ( 42-46 )", "L ( 42-47 )", "L ( 44 )", 
                     "L (43 - 46)", "L/K", "L/T", "L/XL ( 39-47 )", "YLG 147,5-157,5", "7 ( L )",
                     "140", "1 ( 140 )", "10 (140)", "10/12 (140-152)", "140/152", "146", "2 ( 152 )",
                     "2", "45-48", "45 - 47", "5", "5 ( 46-48 )", "5 ( 47-49 )", "14 (46-48)"),
             "M" = c("M ( 140-152 )", "M ( 38-42 )", "M ( 38/40 )", "M ( 40 )", "M (38 - 42)", "M/L", 
                     "YM 135-147,5", "YSM 125-135", "24 (M)", "38/40 ( M / L )", "102 (M)", "128",
                     "134", "0 ( 128 )", "02 Senior", "12 (41-45)", "4 ( 43-45 )", "4 ( 43-46 )",
                     "4 ( 44-46 )", "41 - 44", "3 ( 39-42 )", "3 ( 40-42 )", "3 ( 41-43 )", "4 ( 39-42 )",
                     "39-42", "39 - 42", "39/42", "4", "43-46", "43 - 46", "43/46",  "5 ( 43-46 )"),
             "S" = c("S ( 128-140 )", "S ( 34-38 )", "S ( 34/36 )", "S ( 36 )", "104", "01 Junior",
                     "1 ( 31-34 )", "1 ( 33-36 )", "1 ( 34-36 )", "1 ( Junior)", "10 (36-40)",
                     "2 ( 35-38 )", "2 ( 37-39 )", "2 ( 37-40 )", "2 ( Senior )", "3 (35-38 )",
                     "35 - 38", "37 - 40", "35/38", "19 (38)", "10", "11", "3"),
             "XL" = c("XL ( 158-170 )", "XL ( 44/46 )", "XL (46-48,5)", "XL (46-50 )","XL/T",
                      "YXL 157,5-167,5", "8", "8 ( XL )", "8", "8 ( XL )", "152", "164/176", "164",
                      "158", "14 (164)", "14/16 (164-176)", "3 ( 164 )", "47 - 50", "47/49", "6",
                      "6 ( 47-50 )"),
             "XS" = c("XS ( 116-128 )", "XS ( 30-34 )", "XS ( 32 )", "XS ( 32/34 )","XS/S", "0 ( 31-33 )",
                      "0 ( Bambini )", "00 ( 27-30 )", "1 ( 25-30 )", "116", "116-122", "116/128", "2 ( 31-34 )",
                      "6/8 (116-128)"), 
             "XXL" = c("9", "2XL", "2XL/T", "28 (3XL)", "XXL", "30 (5XL)", "3XL", "3XL/T", "4XL", "176",
                       "16 (176)", "7"),
             "<40" = c("29", "30", "31", "31,5", "32", "33", "33,5", "34", "35", "35,5", "36", "36 2/3", 
                       "36,5", "37", "37 1/3", "37,5", "38", "38 2/3", "38,5", "39", "39 1/3", "39,5", ""),
             "40" = c("40", "40 2/3", "40,5"),
             "41" = c("41", "41 1/3", "41,5"),
             "42" = c("42", "42 2/3", "42,5"),
             "43" = c("43", "43 1/3", "43,5"),
             "44" = c("44", "44 2/3", "44,5"),
             "45" = c("45", "45 1/3", "45,5"),
             "46" = c("46", "46 2/3", "46,5"),
             ">47" = c("47", "47 1/3", "47,5", "48", "48 2/3", "48,5"))

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
