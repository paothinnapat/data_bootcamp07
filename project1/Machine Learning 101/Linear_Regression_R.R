## Corrrelation
cor(mtcars$hp, mtcars$mpg)
cor(mtcars$wt, mtcars$mpg)

plot(mtcars$hp, mtcars$mpg, pch=16)
plot(mtcars$wt, mtcars$mpg, pch=16)
plot(mtcars$wt, mtcars$hp, pch=16)

cor(mtcars[ ,c("mpg", "wt", "hp")])

## dplyr (tidyverse)
library(dplyr)
corMat <- mtcars %>%
  select(mpg, wt, hp, am) %>%
  cor()

## comput correlation (r) and sig test
cor(mtcars$hp, mtcars$mpg)
cor.test(mtcars$hp, mtcars$mpg)

## Linear Regression
## mpg = f(hp)

lmFit <- lm(mpg ~ hp, data = mtcars)

summary(lmFit)

## prediction
lmFit$coefficients[[1]] + lmFit$coefficients[[2]]*200

new_cars <- data.frame(
  hp = c(250, 320, 400 , 410, 450)
)

## predict()
new_cars$mpg_pred <- predict(lmFit, newdata = new_cars)
new_cars$hp_pred <- NULL
new_cars

summary(mtcars$hp)

## Root Mean Squared Error (rmse)
## Multiple Linear Regression
## mpg = f(hp, wt, am)
## mpg = intercept + b0*hp + b1*wt + b2*am

lmFit_V2 <- lm(mpg ~ hp + wt + am, data = mtcars)

coefs <- coef(lmFit_V2)

coefs[[1]] + coefs[[2]]*200 + coefs[[3]]*3.5 + coefs[[4]]*1

## Build Full Model
lmFit_Full <- lm(mpg ~ . - gear, data = mtcars)
mtcars$predicted <- predict(lmFit_Full)

head(mtcars)

## Train RMSE
squared_error <- (mtcars$mpg - mtcars$predicted) **2
rmse <- sqrt(mean(squared_error))

## Split Data
set.seed(2)
n <- nrow(mtcars)
id <- sample(1:n, size = n*0.7)
train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]

## Train Model
model1 <- lm(mpg ~ hp + wt + am + disp, data = train_data)
p_train <- predict(model1)
rmse_train <- sqrt(mean((train_data$mpg - p_train)**2))

## Test Model
p_test <- predict(model1, newdata = test_data)
error_test <- test_data$mpg - p_test
(rmse_test <- sqrt(mean(error_test)**2))

## Print Result
cat("RMSE Train:", rmse_train, 
    "\nRMSE Test:", rmse_test) # \n = new line



















