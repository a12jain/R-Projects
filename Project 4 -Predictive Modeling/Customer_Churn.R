
##### Project 4 ######## 

# Deploying necessary Libraries
library(readxl)
library(DataExplorer)
library(corrplot)
library(dplyr)
library(InformationValue)
library(caret)
library(ROCR)
library(caTools)
library(blorr)
library(MASS)
library(class)
library(e1071)
library(car)
library(lmtest)
library(pscl)
library(ineq)

### Setting up Working Directory

setwd("D:/Great Learning/Predictive Modeling/Project 4")

### Reading the Dataset from Excel

cellphone <- read_xlsx("Cellphone.xlsx",sheet = 2)

### Performing Exploratory Data Analysis

head(cellphone)

str(cellphone)

summary(cellphone)

# Checking for any Null Values in the data 

sum(is.na(cellphone))

# Checking for Multicollinearity

mat<- cor(cellphone)
corrplot(mat,method = "number",type = "lower", number.cex = 0.7)

# Converting Binary variables to Factors

cellphone$Churn <- as.factor(cellphone$Churn)
cellphone$ContractRenewal <- as.factor(cellphone$ContractRenewal)
cellphone$DataPlan <- as.factor(cellphone$DataPlan)

str(cellphone)
# Performing Univariate Analysis

plot_histogram(cellphone)

# Performing Bivariate Analysis on Continuous Variables


ggplot(cellphone, aes(x = cellphone$DataUsage)) + 
  geom_density(aes(fill = cellphone$Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-1,5)

boxplot(cellphone$DataUsage~cellphone$Churn,horizontal = TRUE)


ggplot(cellphone, aes(x = cellphone$AccountWeeks)) + 
  geom_density(aes(fill = cellphone$Churn), alpha = 0.3) +
  scale_fill_manual(values = c("darkturquoise","lightcoral")) + xlim(-30,250)

boxplot(cellphone$AccountWeeks~cellphone$Churn)


ggplot(cellphone, aes(x = cellphone$CustServCalls)) + 
  geom_density(aes(fill = cellphone$Churn), alpha = 0.3) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) + xlim(-5,10)
  
boxplot(cellphone$CustServCalls~cellphone$Churn)


ggplot(cellphone,aes(x = cellphone$DayMins)) +
  geom_density(aes(fill = cellphone$Churn,alpha = 0.3)) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) +xlim(-5,400)
  
boxplot(cellphone$CustServCalls~cellphone$Churn)


ggplot(cellphone,aes(x = cellphone$DayCalls)) +
  geom_density(aes(fill = cellphone$Churn,alpha = 0.3)) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) +xlim(-5,200)

boxplot(cellphone$DayCalls~cellphone$Churn)


ggplot(cellphone,aes(x = cellphone$MonthlyCharge)) +
  geom_density(aes(fill = cellphone$Churn,alpha = 0.3)) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) +xlim(-5,200)

boxplot(cellphone$MonthlyCharge~cellphone$Churn)


ggplot(cellphone,aes(x = cellphone$OverageFee)) +
  geom_density(aes(fill = cellphone$Churn,alpha = 0.3)) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) +xlim(-5,30)

boxplot(cellphone$OverageFee~cellphone$Churn)


ggplot(cellphone,aes(x = cellphone$RoamMins)) +
  geom_density(aes(fill = cellphone$Churn,alpha = 0.3)) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) +xlim(-5,30)

boxplot(cellphone$RoamMins~cellphone$Churn)


# Performing Bi-Variate analysis on Categorical Variables

ggplot(cellphone,aes(x = cellphone$ContractRenewal, fill = cellphone$Churn)) +
  geom_bar(width = 0.25,alpha = 0.5) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral"))


ggplot(cellphone, aes(x = cellphone$DataPlan, fill = cellphone$Churn)) + 
  geom_bar(width = 0.25, alpha=0.5) + 
  scale_fill_manual(values = c('darkturquoise', 'lightcoral'))


str(cellphone)
# Performing Cooks Distance to check the Outliers.
cellphone$Churn<- as.numeric(cellphone$Churn)
cook_lm <- lm(cellphone$Churn~. -ContractRenewal -DataPlan, data = cellphone,)

cooks_dist<- cooks.distance(cook_lm)

plot(cooks_dist)

abline(h=4*mean(cooks_dist,na.rm = TRUE),col="red")

influential <- as.numeric(names(cooks_dist)[(cooks_dist > 4*mean(cooks_dist, na.rm=T))])

out <- (cellphone[influential, ])

View(out)

cellphone$Churn<- as.factor(cellphone$Churn)

str(cellphone)
#Splitting the Data to Test and Train.

set.seed(1234)

index<- sample.split(cellphone$Churn,SplitRatio = .70)

trainData <- subset(cellphone, index == TRUE)
testData <- subset(cellphone,index == FALSE)

str(trainData)

# Creating the Logistic Regression Model on Train data.

logModel <- glm(trainData$Churn~., data = trainData,family = "binomial")

summary(logModel)
logModel$coefficients

# Checking for Multicollinearity using VIF

vif(logModel)

# Model refining - Logistic Model

# Performing Step Wise AIC on Logistic Model
blr_step_aic_both(logModel)

logModel2 <- glm(trainData$Churn ~ trainData$AccountWeeks + trainData$ContractRenewal + trainData$DataPlan +trainData$CustServCalls+trainData$DayMins+ trainData$DayCalls+ trainData$OverageFee+ trainData$RoamMins, 
                 data= trainData, 
                 family = "binomial")
summary(logModel2)
logModel2$coefficients


#Likelihood Ratio

lrtest(logModel2) 
?pR2
# Calculating Psudo R Sq Value
pR2(logModel)

#Odds Ratio

exp(logModel2$coefficients)

# Predicting the Model on Train Set.
log_pred_train <- predict(logModel2,type = "response",data = trainData)

table_pred_train <- table(trainData$Churn, log_pred_train>.5)
table_pred_train

pred_train_num <- ifelse(log_pred_train >.5,1,0)
pred_train_num <- as.factor(pred_train_num)
str(pred_train_num)
actual_train_num <- trainData$Churn

str(actual_train_num)
confusionMatrix(pred_train_num,actual_train_num)

# Baseline Accuracy
nrow(trainData[trainData$Churn ==0,])/nrow(trainData)

# Plotting ROC Curve

perfObj_train <- prediction(log_pred_train,trainData$Churn)
perf_train <- performance(perfObj_train,"tpr","fpr")
plot(perf_train)

plot(perf_train, colorize = TRUE, print.cutoffs.at = seq(0,1,0.05),text.adj = c(-0.2,1.7))

auc <- performance(perfObj_train,"auc")
auc@y.values

# Calculating KS Value

KS_Train <- max(perf_train@y.values[[1]] - perf_train@x.values[[1]])
KS_Train
# Calculating Gini Value
gini_train <- ineq(log_pred_train,type = "Gini")
gini_train

# Calculating the Concordance and Discordance Values.

Concordance(actuals = trainData$Churn, predictedScores = log_pred_train)


# Predicting values on Test Dataset.
log_pred_test <- predict(logModel, newdata = testData, type = "response")


pred_test_num <- ifelse(log_pred_test >.5,1,0)
pred_test_num <- as.factor(pred_test_num)
str(pred_test_num)
actual_test_num <- (testData$Churn)
confusionMatrix(pred_test_num,actual_test_num)

# Baseline Accuracy
nrow(testData[testData$Churn ==0,])/nrow(testData)

# Plotting ROC Curve

length(log_pred_test)
length(testData$Churn)
perfObj_test <- prediction(log_pred_test,testData$Churn)
perf_test <- performance(perfObj_test,"tpr","fpr")
plot(perf_test)

plot(perf_test, colorize = TRUE, print.cutoffs.at = seq(0,1,0.05),text.adj = c(-0.2,1.7))

auc <- performance(perfObj_test,"auc")
auc@y.values

# Calculating KS Value

KS_Test <- max(perf_test@y.values[[1]] - perf_test@x.values[[1]])

# Calculating Gini Value
gini_test <- ineq(log_pred_test,type = "Gini")
gini_test
# Calculating the Concordance and Discordance Values.

Concordance(actuals = testData$Churn, predictedScores = log_pred_test)

# Transforming Train and Test Dataset to Data Frames

train <- as.data.frame(trainData)
class(train)
test<- as.data.frame(testData)
class(test)

# Performing KNN Model

trainData_scale <- scale(trainData[,-c(1,3,4)])
trainData_scale1 <- cbind(trainData[,c(1,3,4)], trainData_scale)

testData_scale <- scale(testData[,-c(1,3,4)])
testData_scale1 <- cbind(testData[,c(1,3,4)], testData_scale)
trctrl <- trainControl(method = "cv", number = 3)
knn_fit = train(Churn ~., data = trainData_scale1, method = "knn",
                trControl = trctrl,
                tuneLength = 10)

knn_fit$bestTune$k


#Performance Matrix on Train data

pred_knn_train <- predict(knn_fit, data = trainData_scale1, type = "raw")
confusionMatrix(pred_knn_train, trainData_scale1$Churn)


#Performance Matrix on Test data

pred_knn_test <- predict(knn_fit, newdata = testData_scale1, type = "raw")
confusionMatrix(pred_knn_test, testData_scale1$Churn)




# Performing Naive Bayes

NB <- naiveBayes(trainData_scale1$Churn~., data = trainData_scale1)
summary(NB)

#Performance Matrix on Train Data 

pred_nb_train <- predict(NB, newdata = trainData_scale1)
confusionMatrix(pred_nb_train, trainData_scale1$Churn)


#Performance Matrix on Test Data 

pred_nb_test <- predict(NB, newdata = testData_scale1)
confusionMatrix(pred_nb_test, testData_scale1$Churn)