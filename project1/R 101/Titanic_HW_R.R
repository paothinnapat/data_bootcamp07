library(titanic)

head(titanic_train)

## DROP NA (Missing Value)
titanic_train <- na.omit(titanic_train) # delete NULL data
nrow(titanic_train)

## SPLIT DATA
set.seed(99)
n <- nrow(titanic_train)
id <- sample(1:n, size = n*0.7) ## 70% train 30% test
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

## Train Model
glm(Survived ~ Pclass + Age, data = train_data, family = "binomial")

## Test Model

## Accuracy