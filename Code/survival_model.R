### Survival analysis: Cox proportional hazard model 

# use all cases from prediction-dataset and test with 1 unit
validation.surv <- subset(validation, units == 1) 

# Event-variable and time variable
training$event <- rep(1, NROW(training))
training$time <- as.integer(training$time_last)

# Model
coxmodel <- coxph(Surv(time, event) ~ color + rrp + brand + category, 
                  data = training)
summary(coxmodel)

# Plotting
plot(survfit(coxmodel, type = "aalen"), xlab = "Time", ylab = "Survival Probability")
plot(survfit(Surv(time, event) ~ color, data = training), xlab = "Time",
     ylab = "Survival Probability", col = training$color) # exemplary plot depending on color-variable


# Prediction (using package pec)
# Extract predicted survival probabilities 
# at selected time-points
time.points <- c(1:150)
prob.surv <- predictSurvProb(object = coxmodel, newdata = validation.surv, times = time.points)
head(prob.surv)
# Problem: no prediction over future timepoints (>122) that did not occur in training data. 


# Adding the predictions for new data to the plot
lines(survfit(coxmodel, newdata=validation.surv))

# In order to get a predicted date, we need to merge the last purchase of each product in
# the train-data with the items-data. Then we can add the predicted days until next purchase on that

for (i in unique(validation.surv$id)) {
  validation.surv$last.purchase[validation.surv$id == i] <- max(as.Date(training[training$id == i,]$date))
}

validation.surv$last.purchase <- as.Date(validation.surv$last.purchase, origin = "1970-01-01")

# we delete the new cases since we cant predict them here
validation.surv[is.na(validation.surv$time_last), ]$last.purchase <- validation.surv[is.na(validation.surv$time_last), ]$releaseDate

# Define cut-off level for prediction (Later crossvalidate!)
tau <- 0.05

for (x in 1:NROW(prob.surv)) {
  validation.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
}

validation.surv$soldOutDate <- validation.surv$last.purchase + validation.surv$pred.days


# Predicted days outside february?-->how to treat??
hist(validation.surv$soldOutDate, breaks = 200)

validation.surv$soldOutDate[validation.surv$soldOutDate < "2017-12-25" | validation.surv$soldOutDate > "2018-02-05"] <- "2018-01-15"
validation.surv$soldOutDate[validation.surv$soldOutDate > "2017-12-25" & validation.surv$soldOutDate < "2018-01-01" ] <- "2018-01-01"
validation.surv$soldOutDate[validation.surv$soldOutDate > "2018-01-31" & validation.surv$soldOutDate < "2018-02-05"] <- "2018-01-31"

hist(validation.surv$soldOutDate, breaks = 200)

# Evaluation
validation.surv$error <- as.integer(difftime(validation.surv$date, validation.surv$soldOutDate, units = "days") + 1)

avg.error.val <- sum(validation.surv$error[!is.na(validation.surv$error)]) / NROW(!is.na(validation.surv$error)); avg.error.val #  error of 3.723859


# # # # # # # # # # # # 

# Run model again and predict on items dataset

# use all cases from prediction-dataset and test with a stock of 1
items.surv <- subset(items, stock == 1) 


# Event-variable and time variable
train.new$event <- rep(1, NROW(train.new))
train.new$time <- as.integer(train.new$time_last)

# Model
coxmodel <- coxph(Surv(time, event) ~ color + rrp + brand + category, 
                  data = train.new)
summary(coxmodel)

# Plotting
plot(survfit(coxmodel, type = "aalen"), xlab = "Time", ylab = "Survival Probability")
plot(survfit(Surv(time, event) ~ color, data = train.new), xlab = "Time",
     ylab = "Survival Probability", col = train.new$color) # exemplary plot depending on color-variable


# Prediction (using package pec)
# Extract predicted survival probabilities 
# at selected time-points
time.points <- c(1:150)
prob.surv <- predictSurvProb(object = coxmodel, newdata = items.surv, times = time.points)
head(prob.surv)
# Problem: no prediction over future timepoints (>122) that did not occur in training data. 


# Adding the predictions for new data to the plot
lines(survfit(coxmodel, newdata=items))

# In order to get a predicted date, we need to merge the last purchase of each product in
# the train-data with the items-data. Then we can add the predicted days until next purchase on that

for (i in unique(items.surv$id)) {
  items.surv$last.purchase[items.surv$id == i] <- max(as.Date(train.new[train.new$id == i,]$date))
}

# Define cut-off level for prediction (Later crossvalidate!)
tau <- 0.05

for (x in 1:NROW(prob.surv)) {
  items.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
}

items.surv$soldOutDate <- as.Date(items.surv$last.purchase + items.surv$pred.days, origin = "1970-01-01")


### Predicted days outside february? --> How to treat these??
hist(items.surv$soldOutDate, breaks = 200)











