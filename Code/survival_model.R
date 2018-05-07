### Survival analysis: Cox proportional hazard model 

# Event-variable and time variable
train.new$event <- rep(1, NROW(train.new))
train.new$time <- as.integer(train.new$time_last)

# Model
coxmodel <- coxph(Surv(time, event) ~ color + rrp + brand + category, 
                  data = data)
summary(coxmodel)

# Plotting
plot(survfit(coxmodel, type = "aalen"), xlab = "Time", ylab = "Survival Probability")
plot(survfit(Surv(time, event) ~ color, data = train.new), xlab = "Time",
     ylab = "Survival Probability", col = train.new$color) # exemplary plot depending on color-variable


# Prediction (using package pec)
# Extract predicted survival probabilities 
# at selected time-points
time.points <- c(1:150)
prob.surv <- predictSurvProb(object = coxmodel, newdata = items.new, times = time.points)
head(prob.surv)
# Problem: no prediction over future timepoints (>122) that did not occur in training data. 
# --> Solution: Possibly better use "days until next purchase" as time variable rather than "days until purchase".

# Adding the predictions for new data to the plot
lines(survfit(coxmodel, newdata=items.new))

# Change Survival to probability of purchase
#prob.purchase <- apply(prob.surv, 1, function(x) 1-x)
prob.purchase <- data.frame(matrix(nrow = NROW(prob.surv), ncol = NCOL(prob.surv)))
for (x in 2:NCOL(prob.purchase)) {
  prob.purchase[,x] <- (1 - prob.surv[,x]) - (1 - prob.surv[,x-1])
}
