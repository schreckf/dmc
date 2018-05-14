library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(caret)
library(mlr)

training_dataset <- read.csv("training_dataset.csv")

# Choose the values I am interested
working_subset <- subset(training_dataset, stock > 1 & stock <= 100)
summary(working_subset$initial_stock)

# subset the products with many initial_stock
e <- subset(working_subset,initial_stock > 34)
partition <- e


# Make sure that columns are of the right class. Class changes whenever I write them as csv.

factor_vector <- c("size_cat","ID","pid_codes","pid", "size", "size_simplified", "brand",
                   "color","mainCategory", "category", "subCategory",
                   "cluster_100", "sold_or_not", "existence", "weekdays",
                   "weekend", "weekofmonth", "month")

numeric_vector <- c("reach_percent","price", "stock", "units", "total_sold", "daysReleased",
                    "discount.raise", "web_traffic", "GT_zalando_schuhe")

date_vector <- c("date", "releaseDate")
partition[,"pid"] <- as.factor(partition[,"pid"])

for(i in factor_vector){
  partition[,i] = as.factor(partition[,i])
}
for(i in numeric_vector){
  partition[,i] <- as.numeric(partition[,i])
}
for(i in date_vector){
  partition[,i] <- as.Date(partition[,i])
}


partition$date <- as.Date(partition$date)


## Select columns from the main training_dataset and select those which exists
selected <- partition[,c("color","reach_percent","pid_codes","pid","daysReleased","weekofmonth","date","ID", 
                         "brand","size", "initial_stock", "GT_score","discount.raise","price","weekdays", 
                         "weekend", "existence","units","sold_or_not", "size_cat")]

#Take products that exist at a particular date
selected <- subset(selected, selected$existence == "yes")
#Standardize numerics
original_stock <- selected$initial_stock

selected$units <- as.character(selected$units)
for(i in colnames(selected)){
  if(is.numeric(selected[,i]) %in% T){
    selected[,i] <- scale(selected[,i])
  }
}
selected$units <- as.numeric(selected$units)

#Choose 10 products for simplicity
set.seed(12)
place <- sample(1:length(unique(selected$pid)),20)
products <- unique(selected$pid)[place]
complete_selection <- subset(selected, pid %in% products)

# Train and test sets
train <- subset(complete_selection, date <= as.Date("2017-12-31"))
test <- subset(complete_selection, date > as.Date("2017-12-31"))

## Remove pid, date, ID,size, existence
train <- train[order(train$date), ]
test <- test[order(test$date), ]
train_wo_id <- train[,!colnames(train) %in% c("pid","stock","date","size","ID","existence")]
test_wo_id <- test[,!colnames(test) %in% c("pid","stock","date","size","ID","existence")]

##################
## RANDOMFOREST ##
##################

# Build a dummy matrix with right column names
encoder_train  <- dummyVars(~ ., data = train_wo_id, fullRank = T,sep="_")
tr <- as.data.frame(predict(encoder_train, train_wo_id))
colnames(tr) <- make.names(colnames(tr))

encoder_test  <- dummyVars(~ ., data = test_wo_id, fullRank = T, sep="_")
ts <- as.data.frame(predict(encoder_test, test_wo_id))
colnames(ts) <- make.names(colnames(ts))

# During this part, we predict whether it is sold_or_not,
# Therefore I take units out, temporarily

units_tr <- tr[,"units"]
units_ts <- ts[,"units"]

tr_wo_units <- tr[,!colnames(tr) %in% "units"]
ts_wo_units <- ts[,!colnames(ts) %in% "units"]
tr_wo_units$sold_or_not_1 <- as.factor(tr_wo_units$sold_or_not_1)
ts_wo_units$sold_or_not_1 <- as.factor(ts_wo_units$sold_or_not_1)


task.rf <- makeClassifTask(data = tr_wo_units, target = "sold_or_not_1", positive = "1")

modelLib <- list()
yhat <- list()
auc <- list()

rf <- makeLearner("classif.randomForest", 
                  predict.type = "prob", # prediction type needs to be specified for the learner 
                  par.vals = list("replace" = TRUE, "importance" = FALSE))

rf.parms <- makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 10), # Number of features selected at each node, smaller -> faster
  makeDiscreteParam("sampsize", values = c(300, 200)), # bootstrap sample size, smaller -> faster
  makeIntegerParam("ntree", lower = 200, upper = 1000) # Number of tree, smaller -> faster
) 

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
rdesc <- makeResampleDesc(method = "CV", iters = 3L, stratify = TRUE)

timing <- list()
timing[["simple"]] <- system.time(
  tuning <- tuneParams(rf, task = task.rf, resampling = rdesc,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)
)

tuning_results <- generateHyperParsEffectData(tuning, partial.dep = TRUE)
tapply(tuning_results$data$auc.test.mean, INDEX = c(tuning_results$data$mtry), mean)
rf_tuned <- setHyperPars(rf, par.vals = tuning$x)

modelLib[["rf"]] <- mlr::train(rf_tuned, task = task.rf)
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = ts_wo_units)
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)


# Replace prediction with true value in the test set,
# in order to go on with predicting units

ts <- cbind(ts, pred_sold = yhat[["rf"]]$data[,4])
ts$sold_or_not.1 <- ts$pred_sold
ts <- ts[,!colnames(ts) %in% "pred_sold"]

#############
## XGBoost ##
#############

task.xgb = makeRegrTask(data = tr, target = "units")
xgboost = makeLearner("regr.xgboost", fix.factors.prediction = TRUE)
mod = mlr::train(xgboost, task = task.xgb)
prediction = predict(mod, newdata = ts)
performance(prediction)

# This was without tuning, now tune xgboost
xgboost.parms <- makeParamSet(
  makeDiscreteParam("nrounds", values = c(20, 100, 200)), 
  makeDiscreteParam("max_depth", values = c(2, 4)), 
  makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)), 
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 0.8),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 0.8)
) 
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
res = tuneParams("regr.xgboost", task = task.xgb, resampling = rdesc,
                 par.set = xgboost.parms, control = ctrl)
tuned = setHyperPars(makeLearner("regr.xgboost"), par.vals = res$x)
trained <- mlr::train(tuned, task.xgb)
pred <- predict(trained, newdata = ts)
performance(pred)

ts_predicted <- cbind(ts, response = pred[["data"]][,2])

# Recall product pid and size
ts_predicted <- cbind(ID = test$ID, pid = test$pid, date = test$date,size = test$size, ts_predicted)

#Take a look at individual products
analysis <- ts_predicted[,c("ID","date","pid","size","units","sold_or_not.1","response")]

unique(analysis$ID)

a <- subset(analysis, ID == "14836 - 2 ( 37-39 )")

ggplot(a, aes(x = date)) +
  stat_summary(aes(y = units), colour = "blue", fun.y = sum, geom = "line") +
  stat_summary(aes(y = response), colour = "red", fun.y = sum, geom = "line")
