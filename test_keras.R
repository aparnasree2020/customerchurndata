#pkgs <- c("keras", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr")
#install.packages(pkgs)


## Install Keras if you have not installed before
#install_keras()
# tensorflow::install_tensorflow(version="2.0.0")


# Load libraries
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(corrr)
library(dplyr)
library(ggplot2)
library(tidyverse)

setwd("E:/Aparnasree/courses/mondayclass/project/")

churn_data_raw <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

glimpse(churn_data_raw)

churn_data_tbl <- churn_data_raw %>%
  select(-customerID) %>%
  drop_na() %>%
  select(Churn, everything())

glimpse(churn_data_tbl)

set.seed(100)
train_test_split <- initial_split(churn_data_tbl, prop = 0.8)
train_test_split


# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)


train_tbl %>%
  select(Churn, TotalCharges) %>%
  mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%
  focus(Churn) %>%
  fashion()


# Create recipe
rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
  step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(TotalCharges) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_tbl)

# Print the recipe object
rec_obj

# Predictors
x_train_tbl <- bake(rec_obj, new_data = train_tbl) %>% select(-Churn)
x_test_tbl  <- bake(rec_obj, new_data = test_tbl) %>% select(-Churn)

glimpse(x_train_tbl)


# Response variables for training and testing sets
y_train_vec <- ifelse(pull(train_tbl, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(test_tbl, Churn) == "Yes", 1, 0)

# Building our Artificial Neural Network
model_keras <- keras_model_sequential()


model_keras %>% 
  # First hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )
model_keras


# Fit the keras model to the training data
history <- keras::fit( model_keras, as.matrix(x_train_tbl), y_train_vec,
  batch_size       = 50, 
  epochs           = 35,
  validation_split = 0.30)

# Print a summary of the training history
print(history)

# Plot the training/validation history of our Keras model
plot(history)


# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()

# Format test data and predictions for yardstick metrics
estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)

estimates_keras_tbl
# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# AUC
estimates_keras_tbl %>% roc_auc(truth, class_prob)

# Precision
tibble(
  precision = estimates_keras_tbl %>% precision(truth, estimate),
  recall    = estimates_keras_tbl %>% recall(truth, estimate)
)


