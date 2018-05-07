library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(rugarch)
library(data.table)
library(zoo)
library(ggplot2)
library(xts)
library(forecast)
library(forcats)
library(cluster) 
library(lubridate) 
library(tidyr)

prices <- read.csv("prices_clean.csv")
items <- read.csv("items_clean.csv")
train <- read.csv("train.csv", sep="|")
items <- items[,-1]
prices <- prices[,-1]

############################################################
############ 1. Missing Value Imputation: prices ############ 
############################################################

### Missing value imputation by auto.arima & Kalman filter. 
### It takes time, DO NOT RUN.

#prices_only <- prices[,-c(1,2)]

#for(i in rownames(prices_only)){
#  prices_only[i,] <- na.kalman(as.numeric(prices_only[i,]), model = "auto.arima")
#}
#prices <- cbind(prices[,c(1,2)], prices_only)


my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

prices <- melt(prices, id.vars = c("pid", "size"))
colnames(prices)[colnames(prices)=="variable"] <- "date"
colnames(prices)[colnames(prices)=="value"] <- "price"

prices$date <- as.Date(prices$date)
data <- merge(train, prices, by.x = c("date", "pid", "size"), by.y = c("date", "pid", "size"))
data <- merge(data, items, by=c("pid","size"))


data <- data[,-1]
data$ID <- paste(data$pid, "-", data$size)

## write.csv(prices, "prices_clean.csv", row.names=F)
## write.csv(data, "data.csv", row.names=F)
## read.csv(prices,"prices_clean.csv", row.names=F) 

############################################################
############ 2. Missing Value Imputation: items ############ 
############################################################

#### Notes:
### NAs exist only in subCategory column
### NAs exist only when mainCategory == 15
### mC = 15 has only 4 categories {16, 24, 30, 33} which do not exist under other mC's
### There are only 3 levels of mC, {1,9,15}

## Interaction across mainCategories is a very RARE event! 
## Only mC_9 and mC_1 have just one common category which is c_37
## Interaction is also weak amongst categories. 
## They usually do not have common subCategories if they belong to the same mainCategory. 
## Even if they belong to the same mainCategory, they usually have different subCategories. 
## The only exceptions I spotted are; c_37 ∩ c_36 = sC_32; mC_1 ∩ mC_9 = sC_37; c_2 ∩ c_37 = {sC_3, sC_6, sC_39}
## That means we won't lose so much information when we cluster them (even when we merge columns brutally)

## Imputation strategy:
## We should not impute NAs with other subCategories that exist under other categories
## because 1) Their categories (c_16, c_24, c_30, c_33) do not exist elsewhere,
## 2) subCategories do very rarely exist under other categories
## We should divide the products in groups when category == 15, and assign them new subCategories
## We should not give all NAs the same value, since under other mainCategories categories have usually subCategories that are different from each other.
## So, the strategy is to cluster products when category == 15 according to their rrp, stock, and brand. 

items[c("pid", "mainCategory", "category", "subCategory")] <- lapply(items[c("pid", "mainCategory", "category", "subCategory")], factor)
items["releaseDate"] <- as.Date(items$releaseDate)

c15 <- subset(items, mainCategory == 15)
c15 <- c15[,c(4,5,7,9)]
c15$category <- as.factor(c15$category)
c15$stock <- as.numeric(c15$stock)
c15$rrp <- as.numeric(c15$rrp)

set.seed(20)
# Calculating Gower Distance
gower_dist <- daisy(c15,
                    metric = "gower",
                    type = list(logratio = 3))
# Calculating Silhouette width to choose the optimal cluster by elbow method
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# k = 10 clusters give the highest value
# However, I find 3 is more feasible since we will consider them as profoundly new subCategories

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

c15 <- c15[,-c(4,5)]
c15 <- cbind(c15, cluster = pam_fit$clustering + 44)

items$subCategory[which(is.na(items$subCategory))] <- c15$cluster

items$subCategory <- as.factor(items$subCategory)

## write.csv(items,"items_clean.csv", row.names=F) 
## items <- read.csv("items_clean.csv")


#######################################
############ 3. Clustering ############ 
#######################################

# Clustering pid, size, releaseDate, mainCategory, category, subCategory, color, brand
# It takes soo much time. DO NOT RUN

# pide <- subset(items, select = c("pid","releaseDate","mainCategory","category","subCategory","color","brand"))

# gower_dist <- daisy(pide,
#                    metric = "gower",
#                    type = list(logratio = 3))

# pam_fit <- pam(gower_dist, diss = TRUE, k = 50)


## unique categories when we paste cluster together with the clustered columns:
# k == 3, 1814 
# k == 5, 1637 
# k == 20, 1747 
# k == 50, 1829 
# I have chosen k == 50 clusters
pide$cluster <- pam_fit$clustering
## read.csv(pide$cluster,"items_50cluster.csv", row.names=F) 

############################################################
############ 4. Feature engineering using dates ############ 
############################################################

#prices <- read.csv("prices_clean.csv")
#items <- read.csv("items_clean.csv")
#train <- read.csv("train.csv", sep="|")
#cluster <- read.csv("items_50cluster.csv")
#items <- cbind(items, cluster = cluster)


my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

prices <- melt(prices, id.vars = c("pid", "size"))
colnames(prices)[colnames(prices)=="variable"] <- "date"
colnames(prices)[colnames(prices)=="value"] <- "price"

data <- merge(train, prices, by.x = c("date", "pid", "size"), by.y = c("date", "pid", "size"))
data <- merge(data, items, by=c("pid","size"))
data$date <- as.Date(data$date)
data$releaseDate <- as.Date(data$releaseDate)
data$daysReleased <- difftime(data$date, data$releaseDate, units = "days")
data$weekdays <- weekdays(data$date)
data$weekend <- ifelse(data$weekdays %in% c("Saturday","Sunday"), 1, 0)
data$weekofmonth <- ifelse(day(data$date) %in% 1:7, "1stweek",
                           ifelse(day(data$date) %in% 8:14, "2ndweek",
                                  ifelse(day(data$date) %in% 15:21, "3rdweek",
                                         ifelse(day(data$date) %in% 22:31, "4thweek", data$date))))

data$month <- month(data$date)
data$discount.raise <- 100*(data$rrp - data$price)/data$rrp

data %>% 
  group_by(pid) %>% 
  summarise(Frequency = sum(Frequency))

write.csv(data, "data.csv", row.names = F)


##########################################
############ 5. Google Trends ############ 
##########################################


install.packages("gtrendsR"); library(gtrendsR)

GT_brand_web <- data.frame(row.names = my.Dates)
GT_brand_images <- data.frame(row.names = my.Dates)

for(i in levels(items$brand)){
  GT_brand_web[i] <-  gtrends(c(i), gprop = "web", geo="DE", time = "2017-10-01 2018-02-28")[[1]][2]
  GT_brand_images[i] <- gtrends(c(i), gprop = "images", geo="DE", time = "2017-10-01 2018-02-28")[[1]][2]
}
# There is an issue with Cinquestelle data in Google Images, I let it all zeroes:

GT_brand_images["Cinquestelle"]  <- as.data.frame(matrix(0, ncol=1, nrow = length(my.Dates)), row.names = length(my.Dates))
GT_brand_images <- GT_brand_images[,c(1,2,25,3:24)]


GT_brand <- data.frame(row.names = my.Dates)

for(i in levels(items$brand)){
  GT_brand[i] <- GT_brand_web[i] + GT_brand_images[i]
}

# write.csv(GT_brand, "GT_brand.csv", row.names =F)

# The same operation for colors
GT_color_web <- data.frame(row.names = my.Dates)
GT_color_images <- data.frame(row.names = my.Dates)

for(i in levels(items$color)){
  GT_color_web[i] <-  gtrends(c(i), gprop = "web", geo="DE", time = "2017-10-01 2018-02-28")[[1]][2]
  GT_color_images[i] <- gtrends(c(i), gprop = "images", geo="DE", time = "2017-10-01 2018-02-28")[[1]][2]
}

GT_color <- data.frame(row.names = my.Dates)

for(i in levels(items$color)){
  GT_color[i] <- GT_color_web[i] + GT_color_images[i]
}
# write.csv(GT_color, "GT_color.csv", row.names =F)


####################################################################
##### 6. The Sequential Data with Non-Sequential Switch Option ##### 
####################################################################

#data <- read.csv("data.csv")
#prices <- read.csv("prices_clean.csv")
#items <- read.csv("items_clean.csv")
#train <- read.csv("train.csv", sep="|")
#GT_brand <- read.csv("GT_brand.csv")
#clusters <- read.csv("items_50cluster.csv")



data$ID <- paste(data$pid, "-", data$size)
my.date.names <- as.character(seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day"))

##### 6.1. exactlywhensold dataset

# Finding how many times each product appears
frequency_total <- as.data.frame(table(data$ID))

# Creating a dummy for each dates, whether the product is sold or not.
exactlywhensold <- as.data.frame.matrix(table(data$ID, data$daysReleased))
ID <- row.names(exactlywhensold)

# Frequency + Dummy
exactlywhensold <- cbind(ID = ID ,total = frequency_total[2],exactlywhensold)

colnames(exactlywhensold) <- c("ID","total", my.date.names[1:123])

# Taking the dates down the rows
exactlywhensold <- melt(exactlywhensold, id.vars=c("ID","Total"))
colnames(exactlywhensold) <- c("ID","total_sold","date","sold_or_not")

exactlywhensold$date <- as.Date(exactlywhensold$date)

##### 6.2. PRICES

## Previously I have imputed NAs in prices by using Kalman filter, see Section 1.

my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

prices$date <- as.Date(prices$date)

# I separate prices which only apply to the training set (until "2018/01/31")

past_prices <- subset(prices, date >= as.Date("2017/10/01") & date <= as.Date("2018/01/31"))
past_prices$ID <- paste(past_prices$pid,"-",past_prices$size)
past_prices <- past_prices[,-c(1,2)]
exactlywhensold <- merge(exactlywhensold, past_prices, by= c("ID","date"))

## Adding releaseDate, if releaseDate > current date, we will flag the row that at that day it was not available 

ID_releaseDate <- data[,c(21,13)]
ID_releaseDate <- ID_releaseDate[!duplicated(ID_releaseDate),]

exactlywhensold <- merge(exactlywhensold, ID_releaseDate, by= c("ID"))

exactlywhensold$releaseDate <- as.Date(exactlywhensold$releaseDate)

exactlywhensold$existence <- ifelse(exactlywhensold$releaseDate <= exactlywhensold$date, "yes", "no")

## We may want to train models only when the product EXISTS at the particular date.
## So, filter out the rows, existence == "yes" when making analysis

train$ID <- paste(train$pid, "-", train$size)
train <- train[,-c(2,3)]
train$date <- as.Date(train$date)
date_ID <- exactlywhensold[,c("ID","date")]
date_ID <- date_ID[!duplicated(date_ID),]

exactlywhensold <- merge(exactlywhensold, train, by=c("ID","date"), all.x=T)
exactlywhensold$units <- ifelse(is.na(exactlywhensold$units) %in% TRUE,0,exactlywhensold$units)

## Add everything else from items
items <- cbind(items, clusters)
items$releaseDate <- as.Date(items$releaseDate)
items$ID <- paste(items$pid, "-", items$size)
exactlywhensold <- merge(exactlywhensold, items, by=c("ID","releaseDate"))

# Add Google Trends - Brand
GT_brand <- melt(GT_brand, id_vars="X")
colnames(GT_brand) <- c("date", "brand", "GT_score")
GT_brand$date <- as.Date(GT_brand$date)
GT_brand_past <- subset(GT_brand, date <= "2018/01/31")
levels(GT_brand_past$brand) <- levels(exactlywhensold$brand)

#Row order has changed during this step, I recovered.
exactlywhensold <- merge(exactlywhensold, GT_brand_past, by=c("brand","date"))
exactlywhensold <- exactlywhensold[order(exactlywhensold$ID), ]
rownames(exactlywhensold) <- 1:1577352 


#################################################################
###### 7. Building the Target Dataset on which to predict! ######
#################################################################

# Add prices and Google Trends data which should be on the test set
prices_feb <- subset(prices,date >= as.Date("2018/02/01") & date <= as.Date("2018/02/28"))
prices_feb$ID <- paste(prices_feb$pid,"-",prices_feb$size)
prices_feb <- prices_feb[,-c(1,2)]

target_dataset <- merge(prices_feb, items, by="ID")

GT_brand_present <- subset(GT_brand, date > "2018/01/31")
GT_brand_present$date <- as.Date(GT_brand_present$date)
target_dataset <- merge(target_dataset, GT_brand_present,by=c("brand","date"))
target_dataset <- target_dataset[order(target_dataset$ID), ]
rownames(target_dataset) <- 1:353472

# We will have 2 target values: 
# 1) A binary whether at that date the product is sold or not
# 2) Units, if the product is sold

units.target <- rep(NA, 353472) 
sold_or_not.target <- rep(NA, 353472) 

target_dataset <- cbind(target_dataset, units = units.target, sold_or_not = sold_or_not.target)

#Last Reorderings
exactlywhensold <- exactlywhensold[,c(3,10,11,4,2,1,7,12:19,6,9,8,5)]

names(exactlywhensold)
target_dataset <- target_dataset[,c(3,5,6,13,2,1,4,7,8,9,10,11,12,14,15,17,16)]

#######################################################
############ 8. Choosing the right dataset ############ 
#######################################################

# To switch the dates off when the product does not even exist: switch to exactlywhensold$existence == "yes" 
yes_existence <- subset(exactlywhensold, existence == "yes") 
# If you want a non-sequential form, switch exactlywhensold$sold_or_not = 1
non_seq <- subset(exactlywhensold, sold_or_not == 1)
# You can use sequential form for time-series analyses to predict whether it is going to sell in each day or not,
# while you can use non-sequential form to make a categorical prediction about how many units.

#write.csv(target_dataset, "target_dataset.csv", row.names = F)
#write.csv(exactlywhensold, "training_dataset.csv", row.names = F)


