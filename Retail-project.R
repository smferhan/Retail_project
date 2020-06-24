########## Estimating using Decision Tree ##########33
library(tidyverse)
store_train <- read.csv("store_train.csv")
store_test <- read.csv("store_test.csv")
str(store_train)
store_train$store <- as.factor(store_train$store)
class(store_train$store)
unique(store_train$store)
store_train %>%
  is.na() %>%
  colSums()
store_test %>%
  is.na() %>%
  colSums()
library(naniar)
store_train <- impute_below_all(store_train)
store_test <- impute_below_all(store_test)
store_train %>%
  is.na() %>%
  colSums()
store_test %>%
  is.na() %>%
  colSums()
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(RColorBrewer)
new_train <- store_train %>%
select(-Id,-countyname,-storecode,-Areaname,-countytownname,-state_alpha)
model_1 <- rpart(store ~ ., data = new_train, method = "class")
plotcp(model_1)
print(model_1$cptable)
x <- which.min(model_1$cptable[, "xerror"])
new_cp <- model_1$cptable[x,"CP"]
new_cp
pruned_model <- prune(model_1, cp = 0.01)
fancyRpartPlot(pruned_model)
predict_1 <- predict(pruned_model, store_test, type = "class")
table(predict_1)
paste("Result:-", "570 out of 1431 stores should get openend")


######## Estimating using Random Forest ##########

library(tidyverse)
store_train <- read.csv("store_train.csv")
store_test <- read.csv("store_test.csv")
str(store_train)
str(store_test)
store_train$store <- as.factor(store_train$store)
class(store_train$store)
unique(store_train$store)
store_train %>%
  is.na() %>%
  colSums()
store_test %>%
  is.na() %>%
  colSums()
library(naniar)
store_train <- impute_below_all(store_train)
store_test <- impute_below_all(store_test)
store_train %>%
  is.na() %>%
  colSums()
store_test %>%
  is.na() %>%
  colSums()
library(ranger)
new_train <- store_train %>%
  select(-Id,-countyname,-storecode,-Areaname,-countytownname,-state_alpha)
model_2 <- ranger(store ~ ., data = new_train, num.trees = 500, 
                  respect.unordered.factors = "order")
model_2
predict_2 <- predict(model_2, store_test)$predictions
table(predict_2)
paste("Result:-", "542 out of 1431 stores should get openend")


###### By Cross-Validation Technique #############

library(caret)
library(tidyverse)
store_train <- read.csv("store_train.csv")
store_test <- read.csv("store_test.csv")
str(store_train)
str(store_test)
store_train$store <- as.factor(store_train$store)
class(store_train$store)
unique(store_train$store)
store_train %>%
  is.na() %>%
  colSums()
store_test %>%
  is.na() %>%
  colSums()
library(naniar)
store_train <- impute_below_all(store_train)
store_test <- impute_below_all(store_test)
store_train %>%
  is.na() %>%
  colSums()
store_test %>%
  is.na() %>%
  colSums()
new_train <- store_train %>%
  select(-Id,-countyname,-storecode,-Areaname,-countytownname,-state_alpha)
model_3 <- train(
  store ~ .,
  data = new_train,
  tuneLength = 3,
  method = "ranger",
  trControl = trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 5,
    verboseIter = TRUE
  )
)
model_3
pred_3 <- predict(model_3, store_test)
table(pred_3)
paste("Result:-", "532 out of 1431 stores should get openend")
