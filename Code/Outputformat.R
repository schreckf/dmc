
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Suppose we have a prediction

library(data.table)
pred <- read.csv("first_pred.csv")

# Take the columns needed. 
# You will also subset "stock" for the final submission. 
prediction <- pred[,c("ID","pid","date","size","units","response")]

# For the validation I construct our own stock in January
train <- read.csv("train.csv", sep="|")
train$ID <- paste(train$pid,"-",train$size)
train$date <- as.Date(train$date)
products <- subset(train, ID %in% unique(prediction$ID) & date >= as.Date("2018/01/01") & date <= as.Date("2018/01/31"))
# Aggregate units by IDs
stock_JAN <- aggregate(x = products$units, by = list(products$ID), FUN = sum)
colnames(stock_JAN) <- c("ID", "stock_JAN")
# Now merge it with prediction
prediction <- merge(prediction, stock_JAN, by="ID")
# Recover back the date order. It is important since later on we take cumulative sums! 
prediction <- prediction[order(prediction$date),]

# Cumulative sums
prediction <- data.table(prediction, key = "ID")
prediction[, csum_units := cumsum(units), by = key(prediction)]
prediction[, csum_response := cumsum(response), by = key(prediction)]

# Now, a new variable, truth_sellout and response_sellout
prediction$truth_sellout <- ifelse(prediction$csum_units >= prediction$stock_JAN, 1,0)
prediction$response_sellout <- ifelse(prediction$csum_response >= prediction$stock_JAN, 1,0)

# Now we have to find the first date of sellout. Cumsum again and take the value that equals to 1
prediction[, csum_truth_sellout := cumsum(truth_sellout), by = key(prediction)]
prediction[, csum_response_sellout := cumsum(response_sellout), by = key(prediction)]

# Extract the dates
prediction$date <- as.character(prediction$date)
prediction$true_dates <- ifelse(prediction$csum_truth_sellout == 1 & prediction$truth_sellout == 1,prediction$date,NA)
prediction$response_dates <- ifelse(prediction$csum_response_sellout == 1 & prediction$response_sellout == 1
                                      ,prediction$date,NA)

#prediction$response_leftout <- as.numeric(prediction$stock_JAN - prediction$csum_response) 
prediction$response_dates <- ifelse(prediction$date == "2018-01-31" & prediction$csum_response_sellout == 0,
                                    "2018-01-31",prediction$response_dates)
prediction$true_dates <- ifelse(prediction$date == "2018-01-31" & prediction$csum_truth_sellout == 0,
                                    "2018-01-31",prediction$true_dates)

true <- subset(prediction, !is.na(prediction$true_dates), select = c("ID","pid","size","true_dates"))
response <- subset(prediction, !is.na(prediction$response_dates), select = c("ID","pid","size","response_dates"))

final <- merge(true, response, by=c("ID","pid","size"))
final$true_dates <- as.Date(final$true_dates)
final$response_dates <- as.Date(final$response_dates)


final$pred.naive <- "2018-01-16"
final$naive.error <- as.integer(abs(difftime(final$true_dates, final$pred.naive, units = "days"))) + 1
final$real.error <- as.integer(abs(difftime(final$true_dates, final$response_dates, units = "days"))) + 1

avg.naive.error <- sum(final$naive.error[!is.na(final$naive.error)]) / NROW(!is.na(final$naive.error))
avg.naive.error2 <- sqrt(sum(final$naive.error[!is.na(final$naive.error)])) / NROW(!is.na(final$naive.error))

avg.real.error <- sum(final$real.error[!is.na(final$real.error)]) / NROW(!is.na(final$real.error))
avg.real.error2 <- sqrt(sum(final$real.error[!is.na(final$real.error)])) / NROW(!is.na(final$real.error))
