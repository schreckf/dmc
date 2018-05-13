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
library(imputeTS)
library(gtrendsR)

# Load the initial datasets

prices <- read.csv("prices.csv", sep="|")
items <- read.csv("items.csv", sep="|")
train <- read.csv("train.csv", sep="|")

############################################################
############ 1. Missing Value Imputation: prices ############ 
############################################################

### Missing value imputation by auto.arima & Kalman filter. 

# It takes time...

prices_only <- prices[,-c(1,2)]

for(i in rownames(prices_only)){
  prices_only[i,] <- na.kalman(as.numeric(prices_only[i,]), model = "auto.arima")
}
prices <- cbind(prices[,c(1,2)], prices_only)

my.Dates <- seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day")
colnames(prices)[-c(1,2)] <- as.character(my.Dates)

prices <- melt(prices, id.vars = c("pid", "size"))
colnames(prices)[colnames(prices)=="variable"] <- "date"
colnames(prices)[colnames(prices)=="value"] <- "price"


# Save the progress so far
## write.csv(prices, "prices_clean.csv", row.names=F)

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

pide <- subset(items, select = c("pid","releaseDate","mainCategory","category","subCategory","color","brand"))

gower_dist <- daisy(pide,
                    metric = "gower",
                    type = list(logratio = 3))

pam_fit <- pam(gower_dist, diss = TRUE, k = 100)


pide$cluster <- pam_fit$clustering

## write.csv(pide$cluster,"items_100cluster.csv", row.names=F) 

############################################################
############ 4. Feature engineering using dates ############ 
############################################################

prices <- read.csv("prices_clean.csv")
items <- read.csv("items_clean.csv")
train <- read.csv("train.csv", sep="|")
cluster <- read.csv("items_100cluster.csv")
items <- cbind(items, cluster = cluster)


train$date <- as.Date(train$date)
prices$date <- as.Date(prices$date)
data <- merge(train, prices, by.x = c("date", "pid", "size"), by.y = c("date", "pid", "size"))
data <- merge(data, items, by=c("pid","size"))
## write.csv(data, "data.csv", row.names=F)

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
data$rrp <- as.numeric(data$rrp)
data$price <- as.numeric(data$price)

data$discount.raise <- 100*(data$rrp - data$price)/data$rrp

#write.csv(data, "data.csv", row.names = F)


######################################################
############ 5. Google Trends and Zalando ############ 
######################################################
install.packages("gtrendsR"); library(gtrendsR)

zalando <- read.csv("zalando.csv")
zalando <- zalando[,c("date","web_traffic","GT_zalando_schuhe")]

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

data <- read.csv("data.csv")
prices <- read.csv("prices_clean.csv")
items <- read.csv("items_clean.csv")
train <- read.csv("train.csv", sep="|")
GT_brand <- read.csv("GT_brand.csv")
clusters <- read.csv("items_50cluster.csv")
zalando <- read.csv("zalando.csv")
zalando <- zalando[,c("date","web_traffic","GT_zalando_schuhe")]

data$ID <- paste(data$pid, "-", data$size)
my.date.names <- as.character(seq(as.Date("2017/10/01"), as.Date("2018/02/28"), by = "day"))

##### 6.1. training_dataset

# Finding how many times each product appears
frequency_total <- as.data.frame(table(data$ID))

# Creating a dummy for each dates, whether the product is sold or not.
exactlywhensold <- as.data.frame.matrix(table(data$ID, data$daysReleased))
ID <- row.names(exactlywhensold)

# Frequency + Dummy
training_dataset <- cbind(ID = ID ,total = frequency_total[2],exactlywhensold)

colnames(training_dataset) <- c("ID","total_sold", my.date.names[1:123])

# Taking the dates down the rows
training_dataset <- melt(training_dataset, id.vars=c("ID","total_sold"))
colnames(training_dataset) <- c("ID","total_sold","date","sold_or_not")

training_dataset$date <- as.Date(training_dataset$date)

##### 6.2. PRICES

## Previously I have imputed NAs in prices by using Kalman filter, see Section 1.

prices$date <- as.Date(prices$date)

# I separate prices which only apply to the training set (until "2018/01/31")

past_prices <- subset(prices, date >= as.Date("2017/10/01") & date <= as.Date("2018/01/31"))
past_prices$ID <- paste(past_prices$pid,"-",past_prices$size)
past_prices <- past_prices[,-c(1,2)]
training_dataset <- merge(training_dataset, past_prices, by= c("ID","date"))

## Adding releaseDate, if releaseDate > current date, we will flag the row that at that day it was not available 

ID_releaseDate <- data[,c(21,13)]
ID_releaseDate <- ID_releaseDate[!duplicated(ID_releaseDate),]

training_dataset <- merge(training_dataset, ID_releaseDate, by= c("ID"))

training_dataset$releaseDate <- as.Date(training_dataset$releaseDate)

training_dataset$existence <- ifelse(training_dataset$releaseDate <= training_dataset$date, "yes", "no")

## We may want to train models only when the product EXISTS at the particular date.
## So, filter out the rows, existence == "yes" when making analysis

train$ID <- paste(train$pid, "-", train$size)
train <- train[,-c(2,3)]
train$date <- as.Date(train$date)
date_ID <- training_dataset[,c("ID","date")]
date_ID <- date_ID[!duplicated(date_ID),]

training_dataset <- merge(training_dataset, train, by=c("ID","date"), all.x=T)
training_dataset$units <- ifelse(is.na(training_dataset$units) %in% TRUE,0,training_dataset$units)

## Add everything else from items
items <- cbind(items, clusters)
items$releaseDate <- as.Date(items$releaseDate)
items$ID <- paste(items$pid, "-", items$size)
training_dataset <- merge(training_dataset, items, by=c("ID","releaseDate"))

# Add Google Trends - Brand
GT_brand <- melt(GT_brand, id_vars="X")
colnames(GT_brand) <- c("date", "brand", "GT_score")
GT_brand$date <- as.Date(GT_brand$date)
GT_brand_past <- subset(GT_brand, date <= "2018/01/31")
levels(GT_brand_past$brand) <- levels(training_dataset$brand)

# Add Zalando
zalando$date <- as.Date(zalando$date)
zalando_past <- subset(zalando, date <= "2018/01/31")

#Row order has changed during this step, I recovered.
training_dataset <- merge(training_dataset, GT_brand_past, by=c("brand","date"))
training_dataset <- training_dataset[order(training_dataset$ID), ]
rownames(training_dataset) <- 1:1577352 

training_dataset <- merge(training_dataset, zalando_past, by=c("date"))
training_dataset <- training_dataset[order(training_dataset$ID), ]
rownames(training_dataset) <- 1:1577352 

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

zalando_present <- subset(zalando, date > "2018/01/31")
zalando_present$date <- as.Date(zalando_present$date)
target_dataset <- merge(target_dataset, zalando_present,by=c("date"))
target_dataset <- target_dataset[order(target_dataset$ID), ]
rownames(target_dataset) <- 1:353472

# We will have 2 target values: 
# 1) A binary whether at that date the product is sold or not
# 2) Units, if the product is sold

units.target <- rep(NA, 353472) 
sold_or_not.target <- rep(NA, 353472) 

target_dataset <- cbind(target_dataset, units = units.target, sold_or_not = sold_or_not.target)

#Last Reorderings
training_dataset <- training_dataset[,c("ID","pid", "size", "color", "brand", "date",
"releaseDate",  "price" ,"rrp", "mainCategory","category","subCategory","stock","cluster_50",
"GT_score","web_traffic", "GT_zalando_schuhe", "sold_or_not","units", "existence", "total_sold")]
target_dataset <- target_dataset[,c("ID","pid", "size", "color", "brand", "date",
"releaseDate",  "price" ,"rrp", "mainCategory","category","subCategory","stock","cluster_50",
"GT_score","web_traffic", "GT_zalando_schuhe", "sold_or_not","units")]

names(training_dataset)
names(target_dataset)



# Simplify the size (I am not sure of this, we could have also reduced the dimensions by some kind of a clustering)
target_dataset$size_simplified <- fct_collapse(target_dataset$size, 
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

training_dataset$size_simplified <- fct_collapse(training_dataset$size, 
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


training_dataset$date <- as.Date(training_dataset$date)
training_dataset$releaseDate <- as.Date(training_dataset$releaseDate)
training_dataset$daysReleased <- difftime(training_dataset$date, training_dataset$releaseDate, units = "days")
training_dataset$weekdays <- weekdays(training_dataset$date)
training_dataset$weekend <- ifelse(training_dataset$weekdays %in% c("Saturday","Sunday"), 1, 0)
training_dataset$weekofmonth <- ifelse(day(training_dataset$date) %in% 1:7, "1stweek",
                           ifelse(day(training_dataset$date) %in% 8:14, "2ndweek",
                                  ifelse(day(training_dataset$date) %in% 15:21, "3rdweek",
                                         ifelse(day(training_dataset$date) %in% 22:31, "4thweek", training_dataset$date))))

training_dataset$month <- month(training_dataset$date)
training_dataset$rrp <- as.numeric(training_dataset$rrp)
training_dataset$price <- as.numeric(training_dataset$price)

training_dataset$discount.raise <- 100*(training_dataset$rrp - training_dataset$price)/training_dataset$rrp



target_dataset$date <- as.Date(target_dataset$date)
target_dataset$releaseDate <- as.Date(target_dataset$releaseDate)
target_dataset$daysReleased <- difftime(target_dataset$date, target_dataset$releaseDate, units = "days")
target_dataset$weekdays <- weekdays(target_dataset$date)
target_dataset$weekend <- ifelse(target_dataset$weekdays %in% c("Saturday","Sunday"), 1, 0)
target_dataset$weekofmonth <- ifelse(day(target_dataset$date) %in% 1:7, "1stweek",
                                       ifelse(day(target_dataset$date) %in% 8:14, "2ndweek",
                                              ifelse(day(target_dataset$date) %in% 15:21, "3rdweek",
                                                     ifelse(day(target_dataset$date) %in% 22:31, "4thweek", target_dataset$date))))

target_dataset$month <- month(target_dataset$date)
target_dataset$rrp <- as.numeric(target_dataset$rrp)
target_dataset$price <- as.numeric(target_dataset$price)

target_dataset$discount.raise <- 100*(target_dataset$rrp - target_dataset$price)/target_dataset$rrp


#######################################################
############ 8. Choosing the right dataset ############ 
#######################################################

# To switch the dates off when the product does not even exist: switch to training_dataset$existence == "yes" 
yes_existence <- subset(training_dataset, existence == "yes") 
# If you want a non-sequential form, switch training_dataset$sold_or_not = 1
non_seq <- subset(training_dataset, sold_or_not == 1)
# You can use sequential form for time-series analyses to predict whether it is going to sell in each day or not,
# while you can use non-sequential form to make a categorical prediction about how many units.

# Column classes
factor_vector <- c("ID", "pid", "size", "size_simplified", "brand",
                   "color","mainCategory", "category", "subCategory",
                   "cluster_50", "sold_or_not", "existence", "weekdays",
                   "weekend", "weekofmonth", "month")

numeric_vector <- c("price", "stock", "units", "total_sold", "daysReleased",
                    "discount.raise", "web_traffic", "GT_zalando_schuhe")

date_vector <- c("date", "releaseDate")
training_dataset[,"pid"] <- as.factor(training_dataset[,"pid"])

for(i in factor_vector){
  training_dataset[,i] = as.factor(training_dataset[,i])
}
for(i in numeric_vector){
  training_dataset[,i] <- as.numeric(training_dataset[,i])
}
for(i in date_vector){
  training_dataset[,i] <- as.Date(training_dataset[,i])
}

factor_vector_target <- factor_vector[!factor_vector %in% "existence"]

numeric_vector_target <- numeric_vector[!numeric_vector %in% "total_sold"]

date_vector <- c("date", "releaseDate")


for(i in factor_vector_target){
  target_dataset[,i] = as.factor(target_dataset[,i])
}

for(i in numeric_vector_target){
  target_dataset[,i] <- as.numeric(target_dataset[,i])
}
for(i in date_vector){
  target_dataset[,i] <- as.Date(target_dataset[,i])
}

training_dataset$daysReleased <- ifelse(training_dataset$existence %in% ("no"), -1,training_dataset$daysReleased)

#write.csv(target_dataset, "target_dataset.csv", row.names = F)
#write.csv(training_dataset, "training_dataset.csv", row.names = F)





### Additional Features
## initial_stock: Total stock of products between training days (I also think about extracting monthly averages)
## It makes more sense (and performs better) to take subsets according to initial_stock instead of stock in February
initial_stock <- aggregate(x = training_dataset$units, by = list(training_dataset$ID), FUN= sum)
colnames(initial_stock) <- c("ID","initial_stock")
training_dataset <- merge(training_dataset, initial_stock, by="ID")


## We need new sizes, I grouped them with respect to their total sales
## Doing this helped a lot in fitting the test set.

size_stock <- aggregate(x = training_dataset$units, by = list(training_dataset$size), FUN= sum)
colnames(size_stock) <- c("size","size_stock")


size_stock$size_cat <- ifelse(size_stock$size_stock >= 1 & size_stock$size_stock < 36, "1",
                              ifelse(size_stock$size_stock >= 36 & size_stock$size_stock < 71,"2",
                                     ifelse(size_stock$size_stock >= 71 & size_stock$size_stock < 100,"3",
                                            ifelse(size_stock$size_stock >= 100 & size_stock$size_stock < 181, "4",
                                                   ifelse(size_stock$size_stock >= 181 & size_stock$size_stock < 260,"5",
                                                          ifelse(size_stock$size_stock >= 260 & size_stock$size_stock < 374,"6",
                                                                 ifelse(size_stock$size_stock >= 374 & size_stock$size_stock < 439,"7",
                                                                        ifelse(size_stock$size_stock >= 439 & size_stock$size_stock < 539,"8",
                                                                               ifelse(size_stock$size_stock >= 539 & size_stock$size_stock < 628,"9",
                                                                                      ifelse(size_stock$size_stock >= 628 & size_stock$size_stock < 1131,"10",
                                                                                             ifelse(size_stock$size_stock >= 1131 & size_stock$size_stock < 1991,"11",
                                                                                                    ifelse(size_stock$size_stock >= 1991 & size_stock$size_stock < 3948,"12",
                                                                                                           ifelse(size_stock$size_stock >= 3948 & size_stock$size_stock < 5858,"13",
                                                                                                                  ifelse(size_stock$size_stock >= 5858 & size_stock$size_stock < 6930,"14",
                                                                                                                         ifelse(size_stock$size_stock == 6930,"15",
                                                                                                                                ifelse(size_stock$size_stock == 11841,"16",
                                                                                                                                ifelse(size_stock$size_stock == 25812, "17",
                                                                                                                                ifelse(size_stock$size_stock == 25872, "18",
                                                                                                                                ifelse(size_stock$size_stock == 68880, "19",
                                                                                                                     ifelse(size_stock$size_stock == 72594,"20","unknown"))))))))))))))))))))
###
size_stock <- size_stock[,-2]
training_dataset <- merge(training_dataset, size_stock, by = "size")

### Here is the new feature pid_codes. I take the first two characters of the "pid" variable.
## (I tried almost all combinations and the first two was the best)
### It increased the predictive power massively

pid_codes <- substr(training_dataset$pid, start = 1, stop = 2)
training_dataset <- cbind(training_dataset, pid_codes)
training_dataset$pid_codes <- as.factor(training_dataset$pid_codes)


