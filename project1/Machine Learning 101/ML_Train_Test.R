install.packages("caret")
library(caret)


## split data
train_test_split <- function(data) {
set.seed(3)
n <- nrow(mtcars)
id <- sample(n, size = 0.8*n)
train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]
return(list(train_data,test_data))
}
split_data <- train_test_split(mtcars)

## train data
lm_model <- train(mpg ~ hp,
      data = split_data[[1]],
      method = "lm")

## score and evaluate
p <- predict(lm_model, newdata = split_data[[2]])

error <- split_data[[2]]$mpg - p
rmse <- sqrt(mean(error**2))

data("mtcars")

## prepare data
mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c("Auto", "Manual"))

## split data
train_test_split <- function(data) {
  set.seed(4)
  n <- nrow(data)
  id <- sample(n, size = 0.7*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return(list(train_data,test_data))
}

split_data <- train_test_split(mtcars)

## train data
glm_model <- train(am ~ mpg,
                   data = split_data[[1]],
                   method = "glm")

## score and evaluate
p <- predict(glm_model, newdata = split_data[[2]])

acc <- mean(p == split_data[[2]]$am)
