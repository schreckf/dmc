#setwd("C:/Users/Gerome/Desktop/DMC 2018")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#install.packages("feather")
#install.packages("forecast")
#install.packages("ggforce")
#install.packages("tabplot")

library(feather) # data import
library(data.table) # data handle
library(rpart) # decision tree method
library(rpart.plot) # tree plot
library(party) # decision tree method
library(forecast) # forecasting methods
library(ggplot2) # visualizations
library(ggforce) # visualization tools
library(plotly) # interactive visualizations
library(grid) # visualizations
library(animation) # gif
library(tabplot)
library(MASS)

theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))



#data <- read.csv("Test_Frame.csv")
data <- read.csv(file = "../Data/Test_Frame.csv")

data$selling.date <- as.Date(data$selling.date)
data <- as.data.table(data)
n_date <- unique(data[, selling.date])

data_train <- data[selling.date %in% n_date[1:92]]
data_test <- data[selling.date %in% n_date[93:123]]
data_predict <- data[selling.date %in% n_date[124:151]]

ggplot(data_train, aes(selling.date, quantity)) + geom_line() +
  labs(x = "Date", y = "quantity sold") +
  theme_ts

                          
data_train <- as.data.frame(data_train)                    
data_test <- as.data.frame(data_test)

data_train <- mlr::createDummyFeatures(data_train, target = "quantity")
data_test <- mlr::createDummyFeatures(data_test, target = "quantity")

str(data_train)

cols <- c("week_num", "is_weekend", "weekday.Monday","weekday.Tuesday", "weekday.Wednesday",
          "weekday.Thursday", "weekday.Friday", "weekday.Saturday", "weekday.Sunday")

data_train[cols] <- lapply(data_train[cols], factor)
data_test[cols] <- lapply(data_test[cols], factor)

data_train$quantity <- log(data_train$quantity)
data_test$quantity <- log(data_test$quantity)

tree_2 <- rpart(quantity ~ . -selling.date, data = data_train, control = rpart.control(minsplit = 2,
                                                                                       maxdepth = 30,
                                                                                       cp = 0.0001))
tree_2$variable.importance

paste("Number of splits: ", tree_2$cptable[dim(tree_2$cptable)[1], "nsplit"])

rpart.plot(tree_2, digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")

datas <- data.table(Quantity = c(data_train$quantity,
                                       predict(tree_2)),
                    Time = rep(1:length(data_train$quantity), 2),
                    Type = rep(c("Real", "RPART"), each = length(data_train$quantity)))

ggplot(datas, aes(Time, Quantity, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Original quantity", title = "Fitted values from RPART tree") +
  theme_ts

mse <- mean((data_train$quantity - predict(tree_2))^2)
mse

for_rpart <- predict(tree_2, data_test)

data_for <- data.table(Quantity = c(data_train$quantity, data_test$quantity, for_rpart),
                       Date = c(data_train$selling.date, rep(data_test$selling.date, 2)),
                       Type = c(rep("Train data", nrow(data_train)),
                                rep("Test data", nrow(data_test)),
                                rep("Forecast", nrow(data_test))))

ggplot(data_for, aes(Date, Quantity, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  facet_zoom(x = Date %in% data_test$selling.date, zoom.size = 1.2) +
  labs(title = "Forecast from RPART") +
  theme_ts

mse <- mean((data_test$quantity - predict(tree_2, newdata = data_test))^2)
mse
