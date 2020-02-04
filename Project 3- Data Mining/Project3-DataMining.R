install.packages("e1071")
install.packages("ROCR")
install.packages("rpart.plot")
install.packages("readxl")
install.packages("randomForest")
install.packages("data.table")
install.packages("ineq")
install.packages("InformationValue")
install.packages("caret")

library(readxl)
library(DataExplorer)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(data.table)
library(ROCR)
library(ineq)
library(InformationValue)
library(caret)
library(e1071)
# Setting the Working Directory.
setwd("D:/Great Learning/Project 3")

# Reading the Dataset
theraData <- read_xlsx("TheraBank.xlsx", sheet = 1)

# Performing Exploratory Data Analysis
head(theraData)

View(theraData)

names(theraData)

theraData<- theraData[,-1]

str(theraData)

summary(theraData)

# Removing negative records from the Dataset.
theraData <- subset(theraData, theraData$`Experience (in years)` >= 0)

# Changing the Column names for the dataset

names(theraData) <- make.names(c("Age (in years)","Experience (in years)","Income (in K/month)","ZIP Code","Family members","CCAvg",
             "Education","Mortgage","Personal Loan","Securities Account","CD Account","Online","CreditCard"),allow_ = TRUE, unique = FALSE)

# Performing Univariate Analysis.

plot_histogram(theraData)


par(mfrow = c(1,2))
boxplot(theraData[,c(1,2,3,5,6,7)],cex.axis = 0.5, horizontal = TRUE)
boxplot(theraData[,c(8,9,10,11,12,13)],cex.axis = 0.5, horizontal = TRUE)

# Checking for the Null Values.


sum(is.na(theraData))

colSums(is.na(theraData))

# Removing Null Values.

theraData<- na.omit(theraData)
colSums(is.na(theraData))

# Checking for the Correlation between the Varibles.

matrix <- cor(theraData)
corrplot(matrix, method = "number", type = "lower",number.cex = 0.5)

# Removing Experience as there exist a huge Correlation between Age and Experience
theraData<- theraData[,-1]

# Converting the variables to Factors

#theraData$Education <- as.factor(theraData$Education)
theraData$Personal.Loan <- as.factor(theraData$Personal.Loan)
theraData$Securities.Account <- as.factor(theraData$Securities.Account)
theraData$CD.Account <- as.factor(theraData$CD.Account)
theraData$Online <- as.factor(theraData$Online)
theraData$CreditCard <- as.factor(theraData$CreditCard)

str(theraData)

nrow(theraData)

# Splitting data into Train and Test with a split of 70, 30 respectively.
set.seed(1000)
index <- sample.split(theraData$Personal.Loan,SplitRatio = 0.7)

Train_Cart <- subset(theraData, index ==TRUE)
Test_Cart <- subset(theraData, index ==F)

# Building CART Model on Train Data

Model_Train_Cart <- rpart(Personal.Loan~.,data = Train_Cart,method = "class")
Model_Train_Cart

prp(Model_Train_Cart)
rpart.plot(Model_Train_Cart,tweak = 1.2)

Model_Train_Cart$cptable

########## Predicting the values on Train.

pred_train_cart <- predict(Model_Train_Cart,newdata = Train_Cart, type = "class")
pred_train_cart
Train_Cart<- cbind(Train_Cart,pred_train_cart) 

# Predicting the probability on Train Data

Train_Cart$probs <- predict(Model_Train_Cart, Train_Cart, type = "prob")[,2]

tbl <- table(Train_Cart$Personal.Loan,Train_Cart$pred_train)
tbl

print((tbl[1,2]+tbl[2,1])/nrow(Train_Cart))
## Create Confusion matrix on the above prediction

caret::confusionMatrix(Train_Cart$pred_Train,Train_Cart$Personal.Loan)

# Preparing the Rank Table on the Train Data.

prob <- seq (0,1, length = 11)
prob

qs_train<- quantile(Train_Cart$probs,prob)
qs_test <- quantile(Test_Cart$probs_rf)
Train_Cart$Decile <- cut(Train_Cart$probs,unique(qs_train), include.lowest = TRUE, right = FALSE)

table(Train_Cart$Decile)
TrainDT_CART<- data.table(Train_Cart)

TrainRankTbl_CART <- TrainDT_CART[,list(
  cnt = length(Personal.Loan),
  cnt_tar1 <- sum(Personal.Loan==1),
  cnt_tar0 <- sum(Personal.Loan==0)
), by = Decile][order(-Decile)] 

names(TrainRankTbl_CART) <- c("Decile","Count","Count_One","Count_Zero")
names(TrainRankTbl_CART)

TrainRankTbl_CART$rrate <- round(TrainRankTbl_CART$Count_One/TrainRankTbl_CART$Count,4)*100
TrainRankTbl_CART$cum_res<- cumsum(TrainRankTbl_CART$Count_One)
TrainRankTbl_CART$cum_non_res <- cumsum(TrainRankTbl_CART$Count_Zero)
TrainRankTbl_CART$cum_rel_res <- round(TrainRankTbl_CART$cum_res/sum(TrainRankTbl_CART$Count_One),4)*100
TrainRankTbl_CART$cum_rel_non_res <- round(TrainRankTbl_CART$cum_non_res/sum(TrainRankTbl_CART$Count_Zero),4)*100
TrainRankTbl_CART$ks <- abs(TrainRankTbl_CART$cum_rel_res - TrainRankTbl_CART$cum_rel_non_res)

TrainRankTbl_CART

# Plotting the ROC Curve.

predobj_train_cart <- prediction(Train_Cart$probs,Train_Cart$Personal.Loan)
perf_train_cart <- performance(predobj_train_cart,"tpr","fpr")
plot(perf_train_cart)

# Calculating the KS value from Prediction and Plot
KS_train_cart<- max(perf_train_cart@y.values[[1]] - perf_train_cart@x.values[[1]])
KS_train_cart

# Calculating the AUC value.

auc_train_Cart = performance(predobj_train_cart,"auc")
auc_train_Cart

# Calculating the GINI Value.

gini_train_cart = ineq(Train_Cart$probs,type = "Gini")
gini_train_cart

# Calculating the Concordence and Discordence %.

Concordance(actuals = Train_Cart$Personal.Loan, predictedScores = Train_Cart$probs)

########## Predicting the Values on Test Data.

pred_test_Cart <- predict(Model_Train_Cart,newdata = Test_Cart, type = "class")
pred_test_Cart
Test_Cart<- cbind(Test_Cart,pred_test_Cart)

# Predicting the probability on Test Data
Test_Cart$probs <- predict(Model_Train_Cart,Test_Cart,type = "prob")[,2]

tbl_test<- table(Test_Cart$Personal.Loan,Test_Cart$pred_test_Cart)

print((tbl_test[1,2]+tbl_test[2,1])/nrow(Test_Cart))


## Create Confusion matrix on the above prediction

caret::confusionMatrix(Test_Cart$pred_Test,Test_Cart$Personal.Loan)


# Preparing the Rank Table.
Test_Cart$Decile <- cut(Test_Cart$probs,unique(qs_train),include.lowest = TRUE, right = FALSE)
TestDT_CART <- data.table(Test_Cart)

TestRanktbl_Cart <- TestDT_CART[,list(
  count <- length(Personal.Loan),
  Count_One <- sum(Personal.Loan ==1),
  Count_Zero <- sum(Personal.Loan == 0)
), by = Decile][order(-Decile)]

TestRanktbl_Cart

names(TestRanktbl_Cart) <- c("Decile","Count","Count_One","Count_Zero")

TestRanktbl_Cart$rrate <- round((TestRanktbl_Cart$Count_One/TestRanktbl_Cart$Count),4)*100
TestRanktbl_Cart$cum_res <- cumsum(TestRanktbl_Cart$Count_One)
TestRanktbl_Cart$cum_non_res <- cumsum((TestRanktbl_Cart$Count_Zero))
TestRanktbl_Cart$cum_rel_res <- round(TestRanktbl_Cart$cum_res/sum(TestRanktbl_Cart$Count_One),4)*100
TestRanktbl_Cart$cum_rel_non_res <- round(TestRanktbl_Cart$cum_non_res/sum(TestRanktbl_Cart$Count_Zero),4)*100
TestRanktbl_Cart$ks <- abs(TestRanktbl_Cart$cum_rel_res - TestRanktbl_Cart$cum_rel_non_res)

# Plotting the ROC Curve

predobj_test_cart <- prediction(Test_Cart$probs, Test_Cart$Personal.Loan)
perf_test_cart <- performance(predobj_test_cart,"tpr","fpr")
plot(perf_test_cart)

# Calculating KS from the Plot

KS_Test_cart <- max(perf_test_cart@y.values[[1]] - perf_test_cart@x.values[[1]])
KS_Test_cart
# Calculating AUC

auc_test_Cart <- performance(predobj_test_cart,"auc")
auc_test_Cart

#Calculating GINI on Test Cart.

gini_test_cart <- ineq(Test_Cart$probs,"Gini")
gini_test_cart

# Calculating Concordence on Test Cart

Concordance(actuals = Test_Cart$Personal.Loan, predictedScores = Test_Cart$probs)


###### Building RF Model on the Dataset.

set.seed(1000)
index <- sample.split(theraData$Personal.Loan,SplitRatio = 0.7)

Train_RF <- subset(theraData, index ==TRUE)
Test_RF <- subset(theraData, index ==F)

model_train_rf <- randomForest(Personal.Loan~.,data = Train_RF,type = "class", mtry = 3,
                         nodesize = 10, ntree= 1200, importance = TRUE)
model_train_rf

plot(model_train_rf, main = "")
legend("topright", c("OOB","0","1"),text.col = 1:6,lty = 1:3,col = 1:3)
names(Train_RF)

trf<- tuneRF(x = Train_RF[,-8],
             y = Train_RF$Personal.Loan, 
             mtryStart = 5,
             ntreeTry = 1200,
             stepFactor = 1.5,
             improve = 0.0001,
             trace = TRUE,
             plot = TRUE,
             doBest = FALSE,
             importance = TRUE,
             nodesize = 50)

trf
model_train_rf1 <- randomForest(Personal.Loan~.,data = Train_RF,type = "class", mtry = 7,
                               nodesize = 10, ntree= 800, importance = TRUE)

# Predicting the RF model on Train dataset
pred_train_rf <- predict(model_train_rf,Train_RF,type = "class")

Train_RF <- cbind(Train_RF,pred_train_rf)
Train_RF$probs_rf <- predict(model_train_rf, Train_RF, type = "prob")[,2]


## Create Confusion matrix on the above prediction

caret::confusionMatrix(Train_RF$pred_train_rf,Train_RF$Personal.Loan)


# Creating the Rank Table for RF Model on Train Dataset

Train_RF$Decile_RF <- cut(Train_RF$probs_rf,unique(qs_train),include.lowest = TRUE,right = FALSE)

table(Train_RF$Decile_RF)

TrainDT_RF <- data.table(Train_RF)

TrainRanktbl_RF <- TrainDT_RF[,list(
  count <- length(Personal.Loan),
  count_One <- sum(Personal.Loan == 1),
  count_zero <- sum(Personal.Loan == 0)
),by = Decile_RF][order(-Decile_RF)]


names(TrainRanktbl_RF) <- c("Decile_RF", "Count","Count_One","Count_Zero")

TrainRanktbl_RF$rrate <- round((TrainRanktbl_RF$Count_One/TrainRanktbl_RF$Count),4)*100
TrainRanktbl_RF$cum_res <- cumsum(TrainRanktbl_RF$Count_One)
TrainRanktbl_RF$cum_non_res <- cumsum(TrainRanktbl_RF$Count_Zero)
TrainRanktbl_RF$cum_rel_res <- round((TrainRanktbl_RF$cum_res/sum(TrainRanktbl_RF$cum_res)),4)*100
TrainRanktbl_RF$cum_rel_non_res <- round((TrainRanktbl_RF$cum_non_res/sum(TrainRanktbl_RF$cum_non_res)),4)*100
TrainRanktbl_RF$ks <- abs(TrainRanktbl_RF$cum_rel_res - TrainRanktbl_RF$cum_rel_non_res)
TrainRanktbl_RF

# Ploting the ROC Curve
predObj_train_RF <- prediction(Train_RF$probs_rf,Train_RF$Personal.Loan)
perf_Train_RF <- performance(predObj_train_RF, "tpr","fpr")
plot(perf_Train_RF)

# Calculating the KS value on Train

KS_Train_RF <- max(perf_Train_RF@y.values[[1]] - perf_Train_RF@x.values[[1]])
KS_Train_RF
#Calculating the AUC for Train in RF.

auc_train_rf <- performance(predObj_train_RF,"auc")
auc_train_rf
# Calculating the GINI

gini_train_rf <- ineq(Train_RF$probs_rf,"Gini")
gini_train_rf
# Calculating Concordence

Concordance(actuals = Train_RF$Personal.Loan, predictedScores = Train_RF$probs_rf)


#Validating the RF Model on Test Data
pred_test_RF <- predict(model_train_rf,newdata = Test_RF, type = "class")
pred_test_RF
Test_RF<- cbind(Test_RF,pred_test_RF)

Test_RF$probs_test_rf <- predict(model_train_rf, newdata = Test_RF, type = "prob")[,2]


## Create Confusion matrix on the above prediction

caret::confusionMatrix(Test_RF$pred_test_RF,Test_RF$Personal.Loan)



#Preparing the rank Table

prob <- seq (0,1, length = 11)
prob

qs_test <- quantile(Test_RF$probs_test_rf)
Test_RF$Decile_Test <- cut(Test_RF$probs_test_rf,unique(qs_test),include.lowest = TRUE,right = FALSE)
TestDS_RF <- data.table(Test_RF)

TestRanktbl_RF <- TestDS_RF[, list(
  count <- length(Personal.Loan),
  count_one <- sum(Personal.Loan == 1),
  count_zero <- sum(Personal.Loan== 0)
  ),by = Decile_Test][order(-Decile_Test)]


names(TestRanktbl_RF) <- make.names(c("Decile_Test","Count","Count_One","Count_Zero"))
TestRanktbl_RF$rrate <- round((TestRanktbl_RF$Count_One/TestRanktbl_RF$Count),4)*100 
TestRanktbl_RF$cum_res <- cumsum(TestRanktbl_RF$Count_One)
TestRanktbl_RF$cum_non_res <- cumsum(TestRanktbl_RF$Count_Zero)
TestRanktbl_RF$cum_rel_res <- round((TestRanktbl_RF$cum_res/sum(TestRanktbl_RF$cum_res)),4)*100
TestRanktbl_RF$cum_rel_non_res <- round((TestRanktbl_RF$cum_non_res/sum(TestRanktbl_RF$cum_non_res)),4)*100
TestRanktbl_RF$ks <- abs(TestRanktbl_RF$cum_rel_res - TestRanktbl_RF$cum_rel_non_res)
TestRanktbl_RF

# Plotting the ROC Curve

predObj_test_RF <- prediction(Test_RF$probs_test_rf,Test_RF$Personal.Loan)
perf_test_RF <- performance(predObj_test_RF,"tpr","fpr")
plot(perf_test_RF)

# Calculating the FS from ROC Plot  for test RF

KS_Test_RF <- max(perf_test_RF@y.values[[1]] - perf_test_RF@x.values[[1]])
KS_Test_RF

# Calculating the AUC on RF Model Test Dataset

auc_test_RF <- performance(predObj_test_RF, "auc")
auc_test_RF
# Calculating GINI on RF Model Test dataset

gini_test_rf <- ineq(Test_RF$probs_test_rf,"Gini")
gini_test_rf
# Calculating the Concordence on RF Model Test Dataset

Concordance(actuals = Test_RF$Personal.Loan, predictedScores = Test_RF$probs_test_rf)
