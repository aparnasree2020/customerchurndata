getwd()
setwd("C:/Users/Ankita/Documents/BigDataAnalytics/group project/")
data1 <- read.csv("WA_Fn_UseC_Telco_Customer_Churn.csv")
head(data1,5)
install.packages("DataExplorer")
install.packages("GGally")
install.packages("plotly")
install.packages("ggcorrplot")
install.packages("caTools")
install.packages("hrbrthemes")
install.packages("MASS")
library(MASS)
library(caTools)
library(ggcorrplot)
library(plotly)
library(float)
library(dplyr)
library(ggplot2)
library(scales)
library(plyr)
library(grid)
library(gridExtra)
library(tidyverse)
library(ggrepel)
library(DataExplorer)
library(GGally)
library(gridExtra)
library(caret)

#size of dataset - output is first # of rows aka points, then columns aka variables
dim(data1)
#list variables in dataset
names(data1)
#list structure, datatypes of dataset
str(data1)
#print first 10 rows 
head(data1, n=10)
#summary of data
summary(data1)

# to understand the data better
plot_intro(data1)

##Preprocessing the data
install.packages("float")
library(float)
#convert TotalCharges to float type (currently a num)
fl(data1$TotalCharges)
#replace 'No Internet Service' to 'No' for the following columns:
data1[data1 == 'No internet service'] <- 'No'
#replace values 1 and 0 with Yes and No for SeniorCitizen
data1$SeniorCitizen[data1$SeniorCitizen == 1] <- 'Yes'
data1$SeniorCitizen[data1$SeniorCitizen == 0] <- 'No'
#Change tenure to a categorical column
library(dplyr)
data1 <- (mutate(data1, tenure_group = ifelse(data1$tenure %in% 0:12, "Tenure_0-12",
                                              ifelse(data1$tenure %in% 13:24, "Tenure_13-24",
                                                     ifelse(data1$tenure %in% 25:36, "Tenure_25-36",
                                                            ifelse(data1$tenure %in% 37:48, "Tenure_37-48",
                                                                   ifelse(data1$tenure %in% 49:60, "Tenure_49-60","Tenure_gt_60")))))))
#convert new tenure_group from a character column to a factor column
data1$tenure_group <- as.factor(data1$tenure_group)

#splitting churn and non-churn data 
churn <- filter(data1, Churn == "Yes")
non_churn <- filter(data1, Churn == "No")


## **Exploratory Analysis**

library(ggplot2)
library(scales)
library(dplyr)
library(plyr)
#Churn V Non-Churn Plot
churn <- filter(data1, Churn == "Yes")
non_churn <- filter(data1, Churn == "No")
churn_plot <- ggplot(data1, aes(x=factor(Churn))) +
  geom_bar(fill="steelblue", width = .75) +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Churn') +
  xlab('') +
  ylab('Clients')
churn_plot


## Understandig the distribution 

female <- filter(data1, gender == "Female")
male <- filter(data1, gender == "Male")
female_plot <- ggplot(data=female, aes(x=Churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='steelblue') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Female Clients') +
  xlab('Churn') +
  ylab('Clients')
female_plot


# Payment mode used by Females

#Churn distribution for male

male_plot <- ggplot(data=male, aes(x=Churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='steelblue') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Male Clients') +
  xlab('Churn') +
  ylab('Clients')

male_plot


# dependents_plot
dependents <- filter(data1, Dependents == "Yes")
no_dependents <- filter(data1, Dependents == "No")
dep_plot <- ggplot(data=dependents, aes(x=Churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='steelblue') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients with Dependents') +
  xlab('Churn') +
  ylab('Clients')

no_dep_plot <- ggplot(data=no_dependents, aes(x=Churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='steelblue') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients without Dependents') +
  xlab('Churn') +
  ylab('Clients')

#partner plot
partner <- filter(data1, Partner == "Yes")
single <- filter(data1, Partner == "No")
partnership_plot <- ggplot(data=partner, aes(x=Churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='steelblue') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Clients with Partner') +
  xlab('Churn') +
  ylab('Clients')

single_plot <- ggplot(data=single, aes(x=Churn)) + 
  geom_bar(position = 'dodge', stat='count', fill='steelblue') +
  geom_text(aes(label = paste0(round(prop.table(..count..) * 100, 2), '%')),
            stat = 'count',
            position = position_dodge(1),
            size = 4,
            vjust = 0.5) +
  theme_minimal() +
  ggtitle('Single Clients') +
  xlab('Churn') +
  ylab('Clients')


## correlation matrix
#Checking the correlation between continuous variables

#Total Charges has positive correlation with MonthlyCharges and tenure

options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(data1[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))


#Creating derived features

#I am trying to create a derived feature from tenure, where i have made different bins of tenure(which is in months) such as '0-1 year', '2-3 years', '3-4 years' etc.
#min(telco$tenure)
data1 <- mutate(data1, tenure_bin = tenure)

data1$tenure_bin[data1$tenure_bin >=0 & data1$tenure_bin <= 12] <- '0-1 year'
data1$tenure_bin[data1$tenure_bin > 12 & data1$tenure_bin <= 24] <- '1-2 years'
data1$tenure_bin[data1$tenure_bin > 24 & data1$tenure_bin <= 36] <- '2-3 years'
data1$tenure_bin[data1$tenure_bin > 36 & data1$tenure_bin <= 48] <- '3-4 years'
data1$tenure_bin[data1$tenure_bin > 48 & data1$tenure_bin <= 60] <- '4-5 years'
data1$tenure_bin[data1$tenure_bin > 60 & data1$tenure_bin <= 72] <- '5-6 years'

data1$tenure_bin <- as.factor(data1$tenure_bin)


#After checking the distribution of data in each tenure bin, we found that maximum number of customers have a tenure of either 0-1 years and followed by 5-6 years.

options(repr.plot.width =6, repr.plot.height = 3)
ggplot(data1, aes(tenure_bin, fill = tenure_bin)) + geom_bar()




#creating dummy variables 

data2 <- data1[,-c(1,6,19,20)]

#Creating Dummy Variables
dummy<- data.frame(sapply(data2,function(x) data.frame(model.matrix(~x-1,data =data2))[,-1]))

tail(dummy)

#Standardising Continuous features
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
data1[num_columns] <- sapply(data1[num_columns], as.numeric)

telco_int <- data1[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

#Creating the final dataset by combining the numeric and dummy data frames.


telco_final <- cbind(telco_int,dummy)
head(telco_final)


### Model Building 

#have mentioned two approches below we cna go with either 

#################### We can go with this #########################
#H2O

library(h2o)
library(data.table)
library(purrr)
library(ggplot2)
library(hrbrthemes)
h2o.init()

# Split DT into train, valid, test
data <- as.h2o(telco_final)
splits <- h2o.splitFrame(data, ratios = c(0.6, 0.2), seed = 123)

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Baseline if Churn = 0 always
nos <- test["Churn"] %>% as.data.table() %>% .[, .N, by=Churn] %>% .[,N] %>% .[2]
baseline <- nos / h2o.dim(test)[1]

#Define target and features to pass to models:

y <- "Churn"
x <- setdiff(names(telco_final), c(y, "customerID"))


#Run H2O Models
#Train models on validation data.



#Random Forest Model

rf.mod <- h2o.randomForest(x, y,
                           training_frame = train,
                           model_id = "RandomForest_Model",
                           validation_frame = valid,
                           nfolds = 5,
                           seed = 123)
rf.cm <- h2o.confusionMatrix(rf.mod, test)
rf.cm %>% knitr::kable()


#Gradient Boosting

gbm.mod <- h2o.gbm(x, y,
                   training_frame = train,
                   validation_frame = valid,
                   nfolds = 5,
                   seed = 123)

gbm.cm <- h2o.confusionMatrix(gbm.mod, test)
gbm.cm %>% knitr::kable()


#Deep Learning

dl.mod <- h2o.deeplearning(x, y,
                           training_frame = train,
                           model_id = "DeepLearning_Model",
                           validation_frame = valid,
                           nfolds = 5,
                           seed = 123)


dl.cm <- h2o.confusionMatrix(dl.mod, test)
dl.cm %>% knitr::kable()



################################### OR ###################

#Splitting the data into train and validation data.

#set.seed(123)
#indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
#train = telco_final[indices,]
#validation = telco_final[!(indices),]

##########################
#using data1
lr_data_telco <- na.omit(data1)
set.seed(1)
n_lr = nrow(lr_data_telco)
lr_trainIndex = sample(1:n_lr, size = round(0.8*n_lr), replace=FALSE)
lr_telco_train = lr_data_telco[lr_trainIndex ,]
lr_telco_test = lr_data_telco[-lr_trainIndex ,]

logr_telco <- glm(Churn ~., family = binomial, data = lr_telco_train)
## this function is not working for me see it it works for you aftr installing MASS package
logr_aic_telco <- stepAIC(logr_telco, direction = "backward")
#Based on results above we select following features 
#We will use OnlineSecurity, Dependents, TechSupport, SeniorCitizen, MonthlyCharges, StreamingTV, StreamingMovies, TotalCharges, PaymentMethod, MultipleLines, PaperlessBilling, InternetService, Contract & tenure after performing variable selection.

logr_telco_train <- glm(Churn ~ OnlineSecurity + Dependents + TechSupport + SeniorCitizen + MonthlyCharges + StreamingTV + StreamingMovies + TotalCharges + PaymentMethod + MultipleLines + PaperlessBilling + InternetService + Contract + tenure, family = binomial, data = lr_telco_train)
summary(logr_telco_train)

head(dummy)

# Random Forest
rf_data_telco <- na.omit(data1)
set.seed(1)
n_rf = nrow(rf_data_telco)
rf_trainIndex = sample(1:n_rf, size = round(0.8*n_rf), replace=FALSE)
rf_train_telco = rf_data_telco[rf_trainIndex ,]
rf_test_telco = rf_data_telco[-rf_trainIndex ,]

rf_telco <- randomForest(formula = Churn ~ ., data = rf_data_telco, importance = TRUE, ntree = 200)
telco_var_importance <- data.frame(importance(rf_telco))
varImpPlot(rf_telco)

#We'll use cross validation for random forest to find optimal number of predictors. We take the optimal value of number of predictors (all) from the graph of Number of variables v/s. Error plotted below. We fit a Random Forest model to the data.

telco_crossval <- rfcv(rf_data_telco[,c(1:19)], rf_data_telco$Churn, cv.fold = 10, step = .5)
with(telco_crossval, plot(n.var, error.cv, log="x", type="o", lwd=2, xlab="Number of Variables", ylab="Error Rate"))

rf_final_telco <- randomForest(formula = Churn ~ ., data = rf_train_telco, importance = TRUE, ntree = 500)
rf_final_telco

#Calculate the accuracy on training data

rf_train_telco <- rf_train_telco %>% mutate(Imp_preds = predict(rf_final_telco, rf_train_telco))
rf_train_accuracy <- mean(rf_train_telco$Churn==rf_train_telco$Imp_preds)
rf_train_accuracy

#Calculate the accuracy on test data.

rf_test_telco <- rf_test_telco %>% mutate(Imp_preds = predict(rf_final_telco, rf_test_telco))
rf_testaccuracy <- mean(rf_test_telco$Churn==rf_test_telco$Imp_preds)
rf_testaccuracy

#We note that accuracy on the OOB (Out Of Bag) training data is close to 80% and the test accuracy is also close to 80%. Thus, the model has a good predictive performance on the dataset.

#SVM

#Prepare the train and test data sets for SVM.

svm_data_telco <- na.omit(data1)
set.seed(1)
n_svm = nrow(svm_data_telco)
svm_trainIndex = sample(1:n_svm, size = round(0.8*n_svm), replace=FALSE)
svm_telco_train = svm_data_telco[svm_trainIndex ,]
svm_telco_test = svm_data_telco[-svm_trainIndex ,]

#We will fit a SVM model with default parameters on training data and make predictions on training set. Ideally, we would perform a grid search on the parameters C and e for the SVM model to evaluate the best parameters for the model on this data but that is highly computationally expensive and hence not applied here.

svm_fit <- svm(Churn ~ ., probability = TRUE, data = svm_telco_train)
svm_fit

#We make predictions on training set and plot the ROC.

svm_preds_train <- predict(svm_fit, svm_telco_train, probability = TRUE)
svm_train_probs <- data.frame(attr(svm_preds_train, "probabilities"))
svmrocplot_train_simple <- plot(roc(svm_telco_train$Churn, svm_train_probs$X1))

#Calculate the AUC for the predictions on training set.
svmroc_train_simple <- roc(svm_telco_train$Churn, svm_train_probs$X1)
svmauc_train_simple <- auc(svmroc_train_simple)
svmauc_train_simple

#Make predictions on test set and plot ROC.
svm_preds_test <- predict(svm_fit, svm_telco_test, probability = TRUE)
svm_test_probs <- data.frame(attr(svm_preds_test, "probabilities"))
svmrocplot_test <- plot(roc(svm_telco_test$Churn, svm_test_probs$X1))


#Calculate the AUC for the predictions on test set.
svmroc_test <- roc(svm_telco_test$Churn, svm_test_probs$X1)
svmauc_test <- auc(svmroc_test)
svmauc_test
