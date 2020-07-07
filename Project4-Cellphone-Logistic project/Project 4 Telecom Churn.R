setwd("D:/Great Learning/Predictive Modeling/Project 4")
getwd()
library(readxl)
library(ggplot2)
library(corrplot)
library(DataExplorer)
library(dplyr)
library(caTools)
library(ROCR)
library(caret)

Data_Customer_Churn=read_xlsx("Cellphone.xlsx",sheet=2)

###EDA Univariate and bivariate analysis. Plots and charts illustrating the relationships between variables
summary(Data_Customer_Churn)
dim(Data_Customer_Churn)
str(Data_Customer_Churn)
names(Data_Customer_Churn)
attach(Data_Customer_Churn)

summary(Churn)

plot_histogram(Data_Customer_Churn)

hist(Churn,main="Dependent Varaible:Customer Churn",col="red")

boxplot(DataPlan,DataUsage)

boxplot(DataUsage,MonthlyCharge)

boxplot(Data_Customer_Churn[,c(1,2,3,4,5,6)],cex.axis = 0.5, horizontal = TRUE)

boxplot(Data_Customer_Churn[,c(7,8,9,10,11)],cex.axis = 0.5, horizontal = TRUE)

boxplot(DataPlan,DataUsage,MonthlyCharge,Churn,col="red", 
        main ="Boxplot for correlated Independent Variables")

boxplot(AccountWeeks,ContractRenewal,OverageFee,RoamMins,col="blue", 
        main ="Boxplot for other Independent Variables")

Densityplot=density(Churn)

plot(Densityplot, xlab = "Churn")

polygon(Densityplot, col = "red", border = "blue")

Densityplot1=density(DataPlan)
plot(Densityplot1)
polygon(Densityplot1, col = "red", border = "blue")

Densityplot2=density(DataUsage)
plot(Densityplot2)
polygon(Densityplot2,col="green",border="red")

Densityplot3=density(MonthlyCharge)
plot(Densityplot3)
polygon(Densityplot3,col="blue",border="green")

par(mfrow=c(1,1))


###Outlier Identification/Multivariate Method/Cooks Distance                       

mod=lm(Churn ~ ., data=Data_Customer_Churn)
cooksd=cooks.distance(mod)

plot(cooksd, pch="*", cex=1, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red", cex = 0.8)

influential=as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) # Find Outliers Values index positions
outliers=(Data_Customer_Churn[influential, ]) # Filter data for Outliers
View(outliers) # View Outliers

########################DO NOT REMOVE OUTLIERS, NOT BEEN ASKED TO DO SO#######


# Finding Data except outliers
# Method 1 - using anti_join from dplyr

Data_Customer_Churn1=anti_join(Data_Customer_Churn, outliers)
str(Data_Customer_Churn1)

###############################################################################


###checking for null values and removing the null values

sum((is.na(Data_Customer_Churn)))
colSums(is.na(Data_Customer_Churn))
Data_Customer_Churn = na.omit(Data_Customer_Churn) #Why Perform this step if we don't even find any NA
colSums(is.na(Data_Customer_Churn))

###Check for multicollinearity & treating it  
cor(Data_Customer_Churn)
plot_correlation(Data_Customer_Churn)
?plot_correlation
sum(cor(Data_Customer_Churn))
matrix=cor(Data_Customer_Churn)

corrplot(matrix,method = "number", type = "lower", number.cex = 0.7)


### The insights from EDA 
###The data has ouliers of 322 observations and it has no missing  values, there is high correlation between 
##DataPlan and Datausage and monthly charge and dataplan which is the evidence of multi collinearity


# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(Data_Customer_Churn$Churn, SplitRatio = 0.70)

# Split up the data using subset
train = subset(Data_Customer_Churn, split==TRUE)
test = subset(Data_Customer_Churn, split==FALSE)
dim(train)
dim(test)

# Logistic Regression Model
Customer_Churn_Log = glm(Churn ~ ., data = train, family=binomial)
summary(Customer_Churn_Log)

Customer_Churn_Log$coefficients
likelihood=exp(Customer_Churn_Log$coefficients)
print(likelihood)


 
############# Perform Model Refining using STEPWISE AIC on BOTH.
# Remove unwanted Variables

############# Perform Prediction using refined Model on Train Data ##########
#
# Perform Confusion Matrix using the below Code

# Confusion matrix with threshold of 0.5
# predictTest1 <- ifelse(predictTest >0.5,1,0)
# predictTest1 <- as.factor(predictTest1)
# actualTest <- as.factor(test$Churn)
# str(actualTest)
# confusionMatrix(predictTest1, actualTest)
# Perform psudo R Sq
# Perform ROC
# Perform AUC
# Perform Gini
# Perform KS
# Perform Con-Dis
#
############################################################################

#Odds ratio for ContractRenewal  is  ; for a customer, 
#one unit increase in .....increases the odds of Churn by %



############# Perform Prediction on Test Data #############
#
# Perform Confusion Matrix
# Perform ROC
# Perform AUC
# Perform Gini
# Perform KS
# Perform Con-Dis
#
#############################################################

# Predictions on the test set
predictTest = predict(Customer_Churn_Log, type="response", newdata=test)
head(predictTest)

# Confusion matrix with threshold of 0.5
predictTest1 <- ifelse(predictTest >0.5,1,0)
predictTest1 <- as.factor(predictTest1)
actualTest <- as.factor(test$Churn)
str(actualTest)
confusionMatrix(predictTest1, actualTest)


table(test$Churn, predictTest > 0.25)


# Accuracy
(826+29)/(826+23+25+29)

# Baseline accuracy
table(Data_Customer_Churn$Churn)[2]/nrow(Data_Customer_Churn)

# Test set AUC 

ROCRpred = prediction(predictTest, test$Churn)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)

plot(perf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.05), text.adj = c(-0.2, 1.7))
as.numeric(performance(ROCRpred, "auc")@y.values)


# Calculating the KS value from Prediction and Plot

KS=max(perf@y.values[[1]] - perf@x.values[[1]])
KS

# Calculating the AUC value.

AUC = performance(ROCRpred,"auc")
AUC


install.packages("blorr")
library(blorr) # to build and validate binary logistic models (StepWise)
blr_step_aic_forward(Customer_Churn_Log, details = FALSE)
both = blr_step_aic_both(Customer_Churn_Log, details = TRUE)

both$steps
both$model
both$model$coefficients


# Pseudo Rsq (McFadden)
blr_rsq_mcfadden(Customer_Churn_Log)
# Any value between 0.2 and 0.4 is valid model
# Below 0.2 - Underfit
# Over 0.4 - Overfit



###KNN Computation and Interpretation of results

install.packages("class")
library(class)


# Scale The Test and Train Data removing the Factor Variables from the data sets
# and then add the Factor Variables to the Scaled dataset.
# Perform the KNN and NB model on the final dataset.
# Perform the Prediction on Train Dataset for both the models and create Confusion Matrix.
# Predict the Values on test Set and create Confusion Matrix




###KNN for k=13

scale_train <- scale(train)
summary(scale_train)

scale_test <- scale(test)
knn(scale(Data_Customer_Churn_train[,-1]),scale(Data_Customer_Chrun_test[,-1]),scale(Data_Customer_Churn_train$Churn),k=13)
KNN.Churn=knn(scale(Data_Customer_Churn_train[,-1]),scale(Data_Customer_Chrun_test[,-1]),scale(Data_Customer_Churn_train$Churn),k=13)
table.KNN=table(Data_Customer_Chrun_test$Churn,KNN.Churn)
table.KNN
###Accuracy of KNN

KNN.Churn.Accuracy=sum(diag(table.KNN))/sum(table.KNN)
KNN.Churn.Accuracy


###loss of knn
KNN.Churn.Loss=table.KNN[2,1]/(table.KNN[2,1]+table.KNN[1,1])
KNN.Churn.Loss


###KNN for k=15
knn(scale(Data_Customer_Churn_train[,-1]),scale(Data_Customer_Chrun_test[,-1]),scale(Data_Customer_Churn_train$Churn),k=15)
KNN.Churn15=knn(scale(Data_Customer_Churn_train[,-1]),scale(Data_Customer_Chrun_test[,-1]),scale(Data_Customer_Churn_train$Churn),k=15)
table.KNN15=table(Data_Customer_Chrun_test$Churn,KNN.Churn15)
table.KNN15
###Accuracy of KNN

KNN.Churn15.Accuracy=sum(diag(table.KNN15))/sum(table.KNN15)
KNN.Churn15.Accuracy
###loss of knn
KNN.Churn15.Loss=table.KNN15[2,1]/(table.KNN15[2,1]+table.KNN15[1,1])
KNN.Churn15.Loss

###KNN for k=17
knn(scale(Data_Customer_Churn_train[,-1]),scale(Data_Customer_Chrun_test[,-1]),scale(Data_Customer_Churn_train$Churn),k=17)
KNN.Churn17=knn(scale(Data_Customer_Churn_train[,-1]),scale(Data_Customer_Chrun_test[,-1]),scale(Data_Customer_Churn_train$Churn),k=15)
table.KNN17=table(Data_Customer_Chrun_test$Churn,KNN.Churn17)
table.KNN17
###Accuracy of KNN

KNN.Churn17.Accuracy=sum(diag(table.KNN17))/sum(table.KNN17)
KNN.Churn17.Accuracy
###loss of knn
KNN.Churn17.Loss=table.KNN17[2,1]/(table.KNN15[2,1]+table.KNN17[1,1])
KNN.Churn17.Loss

### naive Bayes Calculation
install.packages("e1071")
library(e1071)
help("naiveBayes")
names(Data_Customer_Churn)
naiveBayes(Churn~DataPlan+DataUsage+MonthlyCharge,data=Data_Customer_Churn)
NB.Churn=naiveBayes(Churn~DataPlan+DataUsage+MonthlyCharge,data=Data_Customer_Churn)
predict(NB.Churn,type="raw",newdata=Data_Customer_Churn)
plot(Data_Customer_Churn$Churn,predict(NB.Churn,type="raw",newdata=Data_Customer_Churn)[,2])
NB.Churn.predict=predict(NB.Churn,newdata=Data_Customer_Churn)


#Confusion matrix (wrong dont run the code below this)

tab.NB=table(NB.Churn,NB.Churn.predict)
tab.NB



accuracy.NB=sum(diag(tab.NB))/sum(tab.NB)
accuracy.NB
loss.NB=tab.NB[2,1]/(tab.NB[2,1]+tab.NB[1,1])
loss.NB
opp.loss.NB=tab.NB[1,2]/(tab.NB[1,2]+tab.NB[2,2])
opp.loss.NB
tot.loss.NB=0.95*loss.NB+0.05*opp.loss.NB
tot.loss.NB
