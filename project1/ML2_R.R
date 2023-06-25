## mlbench rpart randomForest class glmnet

##load library
library(caret)
library(tidyverse)
library(mlbench)
library(MLmetrics)
library(forcats)
library(rpart)
library(rpart.plot)
library(glmnet)

##see available data
data("BostonHousing")

## glimpse data
df <- BostonHousing
glimpse(df)

## clustering => segmentation
subset_df <- df %>%
  select(crim, rm, age, lstat, medv) %>%
  as_tibble() #show only subset

## test different k (k = 2-5)
result <- kmeans(x = subset_df, centers = 2)

## membership [1,2,3]
subset_df$cluster <- result$cluster #create new column cluster

## -----------------------------------------------------------
## lm, knn
## standardization
## center and scale



df <- as_tibble(df)

# split data
set.seed(2)
n <- nrow(df)
id <- sample(1:n, size = 0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

# train model
lm_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "lm",
                  preProcess = c("center", "scale"))

set.seed(4)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

# grid search
k_grid <- data.frame(k = c(3,5,7,9,11))

knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   tuneGrid = k_grid,
                   preProcess = c("center", "scale"),
                   trControl = ctrl)

# tuneLength random search
knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   tuneLength = 5,
                   preProcess = c("center", "scale"),
                   trControl = ctrl)
  
# score
medv_pred <- predict(knn_model, newdata = test_data)

# evaluate
RMSE(medv_pred, test_data$medv)

## -----------------------------------------------------------
## classification problem
data("PimaIndiansDiabetes")
glimpse(df)

df <- PimaIndiansDiabetes

# library(forcats)
df$diabetes <- fct_relevel(df$diabetes, "pos")

subset_df <- df %>%
  select(glucose, insulin, age, diabetes)

# split data

set.seed(4)
n <- nrow(df)
id <- sample(1:n, size = 0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

# train model
set.seed(4)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = prSummary,
                     classProbs = TRUE)
(knn_model <- train(diabetes ~ .,
                   data = train_data,
                   method = "knn",
                   preProcess = c("center", "scale"),
                   metric = "Accuracy", #Recall
                   trControl = ctrl))


# score
p <- predict(knn_model, newdata = test_data)


# evaluate
table(test_data$diabetes, p, dnn = c("Actual",
                                     "Prediction"))

confusionMatrix(p, test_data$diabetes,
                positive ="pos",
                mode = "prec_recall")

## -----------------------------------------------------------
## Logistic Regression
set.seed(4)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)
(glm_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "glm",
                    metric = "Accuracy", #Recall
                    trControl = ctrl))

## -----------------------------------------------------------
## Decision Tree (Rpart)
set.seed(4)
(tree_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = ctrl))
rpart.plot(tree_model$finalModel)

## -----------------------------------------------------------
## Random Forest
## Model accuracy the highest >= 75%

## mtry = number of features used to train model
## bootstrap sampling

## bagging technique
set.seed(4)
mtry_gird <- data.frame(mtry = 2:8)

(rf_model <- train(diabetes ~ .,
                     data = train_data,
                     method = "rf",
                     metric = "Accuracy",
                    tuneGrid = mtry_gird,
                     trControl = ctrl))

## -----------------------------------------------------------
## Compare models

list_models <- list(knn = knn_model,
                    logistic = glm_model,
                    decisionTree = tree_model,
                    randomForest = rf_model)
result <- resamples(list_models)
summary(result)

## -----------------------------------------------------------
## ridge vs lasso regression

# 0 = Ridge, 1 = Lasso
glmnet_grid <- expand.grid(alpha = 0:1,
                          lambda = c(0.1, 0.2, 0.3))

(glmnet_model <- train(diabetes ~ .,
                   data = train_data,
                   method = "glmnet",
                   metric = "Accuracy",
                   tuneGrid = glmnet_grid,
                   trControl = ctrl))
