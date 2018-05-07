### Survival analysis: Cox proportional hazard model 

# use all cases from items-dataset with a stock of 1
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
  items.surv$last.purchase[items.surv$id == i] <- max(date(train.new[train.new$id == i,]$date))
}

# Define cut-off level for prediction (Later crossvalidate!)
tau <- 0.05

for (x in 1:NROW(prob.surv)) {
  items.surv$pred.days[x] <- which(prob.surv[x,] < tau)[1]
}

items.surv$soldOutDate <- items.surv$last.purchase + items.surv$pred.days


### Predicted days outside february? --> How to treat these??
hist(items.surv$soldOutDate, breaks = 200)




















