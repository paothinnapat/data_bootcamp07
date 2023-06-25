# caret = Classification And REgression Tree
library(caret)
library(tidyverse)

# train test split
# 1. split data
# 2. train
# 3. score
# 4. evaluate

glimpse(mtcars)

# split data 80%: 20%
train_test_split <- function(data, trainRatio=0.7) {
  set.seed(3)
  (n <- nrow(data))
  (id <- sample(1:n, size = trainRatio*n))
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return(list(train=train_data, test=test_data))
}

set.seed(3)
splitData <- train_test_split(mtcars, 0.8)
train_data <- splitData$train
test_data <- splitData$test

# train model
model <- lm(mpg ~ hp + wt + am, data = train_data)

# score model
mpg_pred <- predict(model, newdata = test_data)

# evaluate
# MAE, MSE, RMSE

mae_metric <- function(actual, prediction) {
  # mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}

mse_metric <- function(actual, prediction) {
  # mean squared error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}

rmse_metric <- function(actual, prediction) {
  # root mean squared error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error)) ## back to normal unit
}

## CARET = Classification And REgression Tree
## Supervised Learning = Prediction

# 1. split data
splitData <- train_test_split(mtcars,0.7)
train_data <- splitData[[1]]
test_data <- splitData[[2]]

# 2. train
# mpg = f(hp, wt, am)
set.seed(4)

ctrl <- trainControl(
  method = "cv", # k-fold golden standard
  number = 5,
  verboseIter = TRUE
)

lm_model <- train(mpg ~ hp + wt + am,
               data = train_data,
               method = "lm",  # algorithm
               trControl = ctrl)

rf_model <- train(mpg ~ hp + wt + am,
                  data = train_data,
                  method = "rf",  # algorithm
                  trControl = ctrl)

knn_model <- train(mpg ~ hp + wt + am,
                  data = train_data,
                  method = "knn",  # algorithm
                  trControl = ctrl)

# 3. score
p <- predict(model, newdata = test_data)

# 4. evaluate
rmse_metric(test_data$mpg, p)

# 5. save model
saveRDS(model, "linear_regression_v1.RDS")

(new_cars <- data.frame(
  hp = c(150, 200, 250),
  wt = c(1.25, 2.2, 2.5),
  am = c(0, 1, 1)
))

model <- readRDS("linear_regression_v1.RDS")

new_cars$mpg_pred <- predict(model, newdata = new_cars)


#HW
library(dplyr)
library(readxl)
library(caret)
library(tidyverse)
inhouse <- read_xlsx(path = "House Price India.xlsx")

glimpse(inhouse)
colnames(inhouse)[3]="bedrooms"
colnames(inhouse)[4]="bathrooms"
colnames(inhouse)[22]="airport"
colnames(inhouse)[21]="school"
colnames(inhouse)[10]="condition"
colnames(inhouse)[5]="area"
colnames(inhouse)[7]="floors"
inhouse <- inhouse %>%
  mutate(Price_log = log(Price))

# 1. split data
train_test_split <- function(data, trainRatio=0.7) {
  set.seed(3)
  (n <- nrow(data))
  (id <- sample(1:n, size = trainRatio*n))
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return(list(train=train_data, test=test_data))
}

set.seed(4)
splitData <- train_test_split(inhouse,0.7)
train_data <- splitData$train
test_data <- splitData$test

# 2. train
set.seed(4)

ctrl <- trainControl(
  method = "cv", # k-fold golden standard
  number = 5,
  verboseIter = TRUE
)


rf_model <- train(Price_log ~ bedrooms
                  + bathrooms
                  + condition
                  + area
                  + floors,
                  data = train_data,
                  method = "rf",  # algorithm
                  trControl = ctrl)


# 3. score
p <- predict(rf_model, newdata = test_data)

# 4. evaluate

mae_metric <- function(actual, prediction) {
  # mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}

mse_metric <- function(actual, prediction) {
  # mean squared error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}

rmse_metric <- function(actual, prediction) {
  # root mean squared error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error)) ## back to normal unit
}

rmse_metric(test_data$Price_log, p)
mae_metric(test_data$Price_log, p)
mse_metric(test_data$Price_log, p)

