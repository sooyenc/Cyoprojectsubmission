# title: "HarvardX: PH125.9x Data Science: Choose Your Own Project Submission"
# author: "Chang Soo Yen"
# date: "28 March 2020"
#output: pdf_document

## 1 Introduction ##
  
## 1.1 Dataset ##

################################
# Creation of Dataset
################################

# Installation of packages required in this project
if(!require(tidyverse)) install.packages("tidyverse", repos="http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos="http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos="http://cran.us.r-project.org")

# Indian Liver Patient Dataset:
# Either from https://archive.ics.uci.edu/ml/machine-learning-databases/00225/ 
# or https://www.kaggle.com/uciml/indian-liver-patient-records

# Creation of column names based on the Indian Liver Patient Dataset
# Abbreviations for Medical Terms Used for Better Clarity
colNames <- c("Age", "Sex", "TB", "DB", "ALP", "ALT", "AST", "TP", "AB", "AGR", "Current")

# Assembling dataset together 
# Separation of elements in each column
# Renaming of column names
downloadData <- read.table(
"https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv",
sep=",", col.names=colNames, header=FALSE)

# Create a new column called 'Status', 1 in 'Current': Disease, 2 in 'Current': No Disease
# Remove 'Current' column
liverData <- downloadData %>% 
  mutate(Status = ifelse(Current=="1", "Disease", "No Disease")) %>% 
  mutate(Status = as.factor(Status)) %>%
  select("Age", "Sex", "TB", "DB", "ALP", "ALT", "AST", "TP", "AB", "AGR", "Status")

## 1.2 Project Goals ##

## 1.3 Key Steps ##

## 2 Data Exploration and Visualization ##

## 2.1 Exploration of Entire Dataset ##

################################
# View head of liverData dataset
################################

head(liverData)

################################
# View structure of liverData dataset
################################

str(liverData)

################################
# Check if liverData dataset has NA values
################################

any(is.na(liverData))

################################
# Remove NA values in liverData dataset
################################

liverData <- liverData[complete.cases(liverData),]

################################
# View structure of liverData dataset
################################

str(liverData)

## 2.2 Exploration of Individual Variables ##

### 2.2.1 Age: Age of the patient ###

################################
# View proportion with and without Liver Disease based on Age
################################

liverData %>% ggplot(aes(Age, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on Age 
################################

liverData %>% ggplot(aes(Age)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.2 Sex: Gender of the patient ###

################################
# View spread of patients, one with and one without Liver Disease, based on Sex 
################################

liverData %>% ggplot(aes(Sex)) + geom_bar() + facet_grid(~Status)

### 2.2.3 TB: Total Bilirubin (mg/dL) ###

################################
# View proportion with and without Liver Disease based on TB
################################

liverData %>% ggplot(aes(TB, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on TB 
################################

liverData %>% ggplot(aes(TB)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.4 DB: Direct Bilirubin (mg/dL) ###

################################
# View proportion with and without Liver Disease based on DB
################################

liverData %>% ggplot(aes(DB, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on DB
################################

liverData %>% ggplot(aes(DB)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.5 ALP: Alkaline Phosphatase ###

################################
# View proportion with and without Liver Disease based on ALP
################################

liverData %>% ggplot(aes(ALP, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on ALP
################################

liverData %>% ggplot(aes(ALP)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.6 ALT: Alanine Aminotransferase ###

################################
# View proportion with and without Liver Disease based on ALT
################################

liverData %>% ggplot(aes(ALT, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on ALT
################################

liverData %>% ggplot(aes(ALT)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.7 AST: Aspartate Aminotransferase ###

################################
# View proportion with and without Liver Disease based on AST
################################

liverData %>% ggplot(aes(AST, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on AST
################################

liverData %>% ggplot(aes(AST)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.8 TP: Total Protein ###

################################
# View proportion with and without Liver Disease based on TP
################################

liverData %>% ggplot(aes(TP, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on TP
################################

liverData %>% ggplot(aes(TP)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.9 AB: Albumin ###

################################
# View proportion with and without Liver Disease based on AB
################################

liverData %>% ggplot(aes(AB, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on AB
################################

liverData %>% ggplot(aes(AB)) + geom_histogram(bins=20) + facet_grid(~Status)

### 2.2.10 AGR: Albumin Globulin Ratio ###

################################
# View proportion with and without Liver Disease based on AGR
################################

liverData %>% ggplot(aes(AGR, col=Status)) + geom_histogram(bins=20)

################################
# View spread of patients, one with and one without Liver Disease, based on AGR
################################

liverData %>% ggplot(aes(AGR)) + geom_histogram(bins=20) + facet_grid(~Status)

## 3 Modeling Approaches ##

## 3.1 Establishing Training and Test Sets ##

### 3.1.1 Separating the liverData Dataset ###

################################
# Separate liverData dataset into train, temptest, test dataset
################################

# test dataset will be 10% of liverData dataset
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y=liverData$Status, times=1, p=0.1, list=FALSE)

# Creation of temptrain (90%) and test (10%) datasets from liverData dataset
temptrain <- liverData[-test_index,]
test <- liverData[test_index,]

# temptest dataset will be 10% of temptrain dataset
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y=temptrain$Status, times=1, p=0.1, list=FALSE)

# Creation of train (90%) and temptest (10%) datasets from temptrain dataset
train <- temptrain[-test_index,]
temptest <- temptrain[test_index,]

################################
# View structure of train, temptest, train dataset
################################

str(train)
str(temptest)
str(test)

### 3.1.2 Key Findings from Data Exploration ###
  
### 3.1.3 The Variables to Keep ###

### 3.1.4 The Variables to Remove ###

################################
# Check correlation of TB and DB
################################

cor_tb_db <- cor(temptrain$TB, temptrain$DB)
cor_tb_db

################################
# Check correlation of ALT and AST
################################

cor_alt_ast <- cor(temptrain$ALT, temptrain$AST)
cor_alt_ast

### 3.1.5 Final Dataset - Training Set, Test Set ###

################################
# Remove DB, AST and TP from train, temptest, test dataset
################################

train <- train %>% select(-c(DB,AST,TP))
temptest <- temptest %>% select(-c(DB,AST,TP))
test <- test %>% select(-c(DB,AST,TP))

################################
# Check structure of train, temptest, train dataset
################################

str(train)
str(temptest)
str(test)

## 3.2 Selecting Modeling Methods from Caret Package ##

## 3.3 Modeling with Naive Bayes ##

################################
# Running of Naive Bayes Model
################################

naives_bayes=train(Status ~ ., data=train, method="naive_bayes")
predicted_status=predict(naives_bayes,newdata=temptest) 
results <- confusionMatrix(predicted_status, temptest$Status, prevalence=0.1)

################################
# Check Accuracy, Sensitivity and Specificity of Naive Bayes Model
################################

nb_accuracy <- results$overall["Accuracy"]
nb_accuracy
nb_sensitivity <- results$byClass["Sensitivity"]
nb_sensitivity
nb_specificity <- results$byClass["Specificity"]
nb_specificity

################################
# Create tibble to store results
################################

options(pillar.sigfig=5)
modelresults <- tibble(method= "Naive Bayes Model", Accuracy=nb_accuracy, 
                       Sensitivity=nb_sensitivity, Specificity=nb_specificity)
modelresults

## 3.4 Modeling with Generalized Linear Model ##

################################
# Running of Generalized Linear Model
################################

glm=train(Status ~ ., data=train, method="glm")
predicted_status=predict(glm,newdata=temptest) 
results <- confusionMatrix(predicted_status, temptest$Status, prevalence=0.1)

################################
# Check Accuracy, Sensitivity and Specificity of Generalized Linear Model
################################

glm_accuracy <- results$overall["Accuracy"]
glm_accuracy
glm_sensitivity <- results$byClass["Sensitivity"]
glm_sensitivity
glm_specificity <- results$byClass["Specificity"]
glm_specificity

################################
# Update tibble to store new results
################################ 

options(pillar.sigfig=5)
modelresults <- bind_rows(modelresults, tibble(method= "Generalized Linear Model", 
                                               Accuracy=glm_accuracy, Sensitivity=glm_sensitivity, Specificity=glm_specificity))
modelresults

## 3.5 Modeling with K-Nearest Neighbours ##

################################
# Running of K-Nearest Neighbours Model
################################

knn=train(Status ~ ., data=train, method="knn")
predicted_status=predict(knn,newdata=temptest) 
results <- confusionMatrix(predicted_status, temptest$Status, prevalence=0.1)

################################
# Check Accuracy, Sensitivity and Specificity of K-Nearest Neighbours Model
################################

knn_accuracy <- results$overall["Accuracy"]
knn_accuracy
knn_sensitivity <- results$byClass["Sensitivity"]
knn_sensitivity
knn_specificity <- results$byClass["Specificity"]
knn_specificity

################################
# Update tibble to store new results
################################ 

options(pillar.sigfig=5)
modelresults <- bind_rows(modelresults, tibble(method= "K-Nearest Neighbours Model", 
                                               Accuracy=knn_accuracy, Sensitivity=knn_sensitivity, Specificity=knn_specificity))
modelresults

## 3.6 Modeling with Random Forest ##

################################
# Running of Random Forest Model
################################

rf=train(Status ~ ., data=train, method="rf")
predicted_status=predict(rf,newdata=temptest) 
results <- confusionMatrix(predicted_status, temptest$Status, prevalence=0.1)

################################
# Check Accuracy, Sensitivity and Specificity of Random Forest Model
################################

rf_accuracy <- results$overall["Accuracy"]
rf_accuracy
rf_sensitivity <- results$byClass["Sensitivity"]
rf_sensitivity
rf_specificity <- results$byClass["Specificity"]
rf_specificity

################################
# Update tibble to store new results
################################ 

options(pillar.sigfig=5)
modelresults <- bind_rows(modelresults, tibble(method= "Random Forest Model", 
                                               Accuracy=rf_accuracy, Sensitivity=rf_sensitivity, Specificity=rf_specificity))
modelresults

## 4 Results ##

## 4.1 Modeling Results and Performance ##

################################
# View tibble of modeling results
################################ 

modelresults

## 4.2 'Best' Model ##

################################
# Running of Random Forest Model with test dataset
################################

rf=train(Status ~ ., data=train, method="rf")
predicted_status=predict(rf,newdata=test) 
results <- confusionMatrix(predicted_status, test$Status, prevalence=0.1)

################################
# Check Accuracy, Sensitivity and Specificity of Random Forest Model with test dataset
################################

rf_accuracy <- results$overall["Accuracy"]
rf_accuracy
rf_sensitivity <- results$byClass["Sensitivity"]
rf_sensitivity
rf_specificity <- results$byClass["Specificity"]
rf_specificity

## 5 Conclusion ##

## 5.1 Summary ##

## 5.2 Limitations and Future Work ##

## 6 Operating System ##

################################
# View version of machine
################################

version
