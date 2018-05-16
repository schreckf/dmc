### Survival analysis: Cox proportional hazard model 

# use all cases from prediction-dataset and test with 1 unit
validation.surv <- subset(validation, units == 1) 

# EVTL: Delete cases with releaseDate 2007-10-01
#validation.surv <- subset(validation.surv, validation.surv$releaseDate != "2017-10-01")

# Event-variable and time variable
training$event <- rep(1, NROW(training))
training$time <- as.integer(training$time_last)

# Model
coxmodel1 <- coxph(Surv(time, event) ~ size + color + rrp + brand + category + mainCategory + subCategory +
                    avg.price + discount.raise, data = training)
summary(coxmodel1)

# Plotting
#plot(survfit(coxmodel2, type = "aalen"), xlab = "Time", ylab = "Survival Probability")
#plot(survfit(Surv(time, event) ~ color, data = training), xlab = "Time",
#     ylab = "Survival Probability", col = training$color) # exemplary plot depending on color-variable


# Prediction (using package pec)
# Extract predicted survival probabilities 
# at selected time-points
time.points <- c(1:150)
prob.surv <- predictSurvProb(object = coxmodel1, newdata = validation.surv, times = time.points)
head(prob.surv)
# Problem: no prediction over future timepoints (>122) that did not occur in training data. 


# Adding the predictions for new data to the plot
#lines(survfit(coxmodel2, newdata=validation.surv))

# In order to get a predicted date, we need to merge the last purchase of each product in
# the train-data with the items-data. Then we can add the predicted days until next purchase on that

for (i in unique(validation.surv$id)) {
  validation.surv$last.purchase[validation.surv$id == i] <- max(as.Date(training$date[training$id == i]))
} # warning can be ignored (just some ids from validation data never occured in training data)

validation.surv$last.purchase <- as.Date(validation.surv$last.purchase, origin = "1970-01-01")

# we delete the new cases since we cant predict them here
validation.surv[is.na(validation.surv$time_last), ]$last.purchase <- validation.surv[is.na(validation.surv$time_last), ]$releaseDate


# Crossvalidate cutoff-level tau & number of days to round to prediction month
for (tau in c(0.005, 0.01, 0.02,  0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.15)) {
  
    print(c("For Level: ", tau))

  
        # add/substract 0 days
        for (x in 1:NROW(prob.surv)) {
          validation.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
        }
        validation.surv$soldOutDate <- validation.surv$last.purchase + validation.surv$pred.days
        #hist(validation.surv$soldOutDate, breaks = 200)
        
        
        validation.surv$soldOutDate[validation.surv$soldOutDate <= "2017-12-31" | validation.surv$soldOutDate >= "2018-02-01"] <- NA
       
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.val <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error))
        
        #hist(validation.surv$soldOutDate, breaks = 200)
        
        
        # Naive model in comparison
        validation.surv$soldOutDate[!is.na(validation.surv$soldOutDate)] <- "2018-01-16"
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.naive <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error)) 
        
        print(c("error: ", avg.error.val, "naive error: ", avg.error.naive, "difference: ", (avg.error.val - avg.error.naive)))
  
        # add/substract 3 days
        for (x in 1:NROW(prob.surv)) {
          validation.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
        }
        validation.surv$soldOutDate <- validation.surv$last.purchase + validation.surv$pred.days

        validation.surv$soldOutDate[validation.surv$soldOutDate <= "2017-12-28" | validation.surv$soldOutDate >= "2018-02-03"] <- NA
        validation.surv$soldOutDate[validation.surv$soldOutDate > "2017-12-28" & validation.surv$soldOutDate < "2018-01-01" & !is.na(validation.surv$soldOutDate)] <- "2018-01-01"
        validation.surv$soldOutDate[validation.surv$soldOutDate > "2018-01-31" & validation.surv$soldOutDate < "2018-02-03" & !is.na(validation.surv$soldOutDate)] <- "2018-01-31"
        
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.val <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error))
        
        # Naive model in comparison
        validation.surv$soldOutDate[!is.na(validation.surv$soldOutDate)] <- "2018-01-16"
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.naive <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error)) 
        
        print(c("error: ", avg.error.val, "naive error: ", avg.error.naive, "difference: ", (avg.error.val - avg.error.naive)))
        
        # add/substract 5 days 
        for (x in 1:NROW(prob.surv)) {
          validation.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
        }
        validation.surv$soldOutDate <- validation.surv$last.purchase + validation.surv$pred.days
        
        validation.surv$soldOutDate[validation.surv$soldOutDate <= "2017-12-25" | validation.surv$soldOutDate >= "2018-02-05"] <- NA
        validation.surv$soldOutDate[validation.surv$soldOutDate > "2017-12-25" & validation.surv$soldOutDate < "2018-01-01" & !is.na(validation.surv$soldOutDate)] <- "2018-01-01"
        validation.surv$soldOutDate[validation.surv$soldOutDate > "2018-01-31" & validation.surv$soldOutDate < "2018-02-05" & !is.na(validation.surv$soldOutDate)] <- "2018-01-31"
        
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.val <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) /sum(!is.na(validation.surv$error))
        
        # Naive model in comparison
        validation.surv$soldOutDate[!is.na(validation.surv$soldOutDate)] <- "2018-01-16"
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.naive <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error)) 
        
        print(c("error: ", avg.error.val, "naive error: ", avg.error.naive, "difference: ", (avg.error.val - avg.error.naive)))
        
        # add/substract 8 days 
        for (x in 1:NROW(prob.surv)) {
          validation.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
        }
        validation.surv$soldOutDate <- validation.surv$last.purchase + validation.surv$pred.days
        
        validation.surv$soldOutDate[validation.surv$soldOutDate <= "2017-12-22" | validation.surv$soldOutDate >= "2018-02-08"] <- NA
        validation.surv$soldOutDate[validation.surv$soldOutDate > "2017-12-22" & validation.surv$soldOutDate < "2018-01-01" & !is.na(validation.surv$soldOutDate)] <- "2018-01-01"
        validation.surv$soldOutDate[validation.surv$soldOutDate > "2018-01-31" & validation.surv$soldOutDate < "2018-02-08" & !is.na(validation.surv$soldOutDate)] <- "2018-01-31"
        
        
        #hist(validation.surv$soldOutDate, breaks = 200)
        
        # Evaluation
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.val <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error)) # 0.03601428 on known releaseDates | 0.01668198 on all releaseDates 
        
        # Naive model in comparison
        validation.surv$soldOutDate[!is.na(validation.surv$soldOutDate)] <- "2018-01-16"
        validation.surv$error <- as.integer(abs(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days"))) + 1
        avg.error.naive <- sqrt(sum(validation.surv$error[!is.na(validation.surv$error)])) / sum(!is.na(validation.surv$error)) 
        
        print(c("error: ", avg.error.val, "naive error: ", avg.error.naive, "difference: ", (avg.error.val - avg.error.naive)))
}


# Evaluation only for predicted January-cases
jan.cases <- subset(validation.surv, soldOutDate < "2018-02-01" & soldOutDate > "2017-12-31")
jan.cases$error <- as.integer(abs(difftime(jan.cases$date, jan.cases$soldOutDate, units = "days"))) + 1
avg.error.jan.cases <- sqrt(sum(jan.cases$error[!is.na(jan.cases$error)])) / NROW(!is.na(jan.cases$error)); avg.error.jan.cases # 0.0900816 | 0.02614358


# # # # # # # # # # # # 




# Run model again on full train.new-dataset and predict on items dataset

# use all cases from prediction-dataset and test with a stock of 1
items.surv <- subset(items, stock == 1) 
NROW(items.surv) # 7616 cases

# EVTL: Delete cases with releaseDate 2007-10-01
#items.surv <- subset(items.surv, items.surv$releaseDate != "2017-10-01") # only 1019 cases after that

# Event-variable and time variable
train.new$event <- rep(1, NROW(train.new))
train.new$time <- as.integer(train.new$time_last)

# Model
coxmodel2 <- coxph(Surv(time, event) ~ size + color + rrp + brand + category + mainCategory + subCategory +
                    avg.price + discount.raise, data = train.new)
summary(coxmodel2)

# Plotting
plot(survfit(coxmodel2, type = "aalen"), xlab = "Time", ylab = "Survival Probability")
plot(survfit(Surv(time, event) ~ color, data = train.new), xlab = "Time",
     ylab = "Survival Probability", col = train.new$color) # exemplary plot depending on color-variable


# Prediction
# Extract predicted survival probabilities 
# at selected time-points
time.points <- c(1:150)
prob.surv <- predictSurvProb(object = coxmodel2, newdata = items.surv, times = time.points)
head(prob.surv)
# Problem: no prediction over future timepoints (>122) that did not occur in training data. 


# Adding the predictions for new data to the plot
lines(survfit(coxmodel2, newdata=items))

# In order to get a predicted date, we need to merge the last purchase of each product in
# the train-data with the items-data. Then we can add the predicted days until next purchase on that

for (i in unique(items.surv$id)) {
  items.surv$last.purchase[items.surv$id == i] <- max(as.Date(train.new[train.new$id == i,]$date))
}

items.surv$last.purchase <- as.Date(items.surv$last.purchase, origin = "1970-01-01")


# Define tuned cut-off level for prediction 
tau <- 0.06 # use tuned tau from above

for (x in 1:NROW(prob.surv)) {
  items.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
}

items.surv$soldOutDate <- as.Date(items.surv$last.purchase + items.surv$pred.days, origin = "1970-01-01")



### Predicted days outside february? --> How to treat these??
hist(items.surv$soldOutDate, breaks = 200)

# Use tuned days here!!
#validation.surv$soldOutDate[validation.surv$soldOutDate < "2017-12-25" | validation.surv$soldOutDate > "2018-02-05"] <- "2018-01-16"
items.surv$soldOutDate[items.surv$soldOutDate <= "2018-01-31" | items.surv$soldOutDate >= "2018-03-01"] <- NA
#items.surv$soldOutDate[items.surv$soldOutDate > "2018-01-25" & items.surv$soldOutDate < "2018-02-01" & !is.na(items.surv$soldOutDate)] <- items.surv$soldOutDate[items.surv$soldOutDate > "2018-01-25" & items.surv$soldOutDate < "2018-02-01" & !is.na(items.surv$soldOutDate)] + 7
#items.surv$soldOutDate[items.surv$soldOutDate > "2018-02-28" & items.surv$soldOutDate < "2018-03-05" & !is.na(items.surv$soldOutDate)] <- items.surv$soldOutDate[items.surv$soldOutDate > "2018-02-28" & items.surv$soldOutDate < "2018-03-05" & !is.na(items.surv$soldOutDate)] - 5


sum(!is.na(items.surv$soldOutDate)) # This is effective the number of predictions in the end
hist(items.surv$soldOutDate, breaks = 200)
table(items.surv$soldOutDate)



# Writing file
write.table(x = items.surv[,c("pid", "size", "soldOutDate")], file = "Uni_HU_Berlin_2", sep = "|", row.names = FALSE)


# Not predictable stock = 1 cases
not.pred <- items.surv[is.na(items.surv$soldOutDate), c("pid", "size")]
pred <- items.surv[!is.na(items.surv$soldOutDate), c("pid", "size")]
write.csv2(x = not.pred, file = "survival.not.pred")
write.csv2(x = pred, file = "survival.pred")



