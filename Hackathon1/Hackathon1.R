library(caTools)
library(rpart)
library(mice)
library(rpart.plot)
library(ipred)
library(caret)
library(e1071)
setwd("D:/Great Learning/HackAll")

# Test Data Prep


Test_data<- read.csv("test_set.csv")
Test_data.xl <- readxl::read_xlsx("test_set.xlsx")
TestData <- Test_data.xl[,-c(1,3,14,17)]
str(TestData)
TestData$marital <- as.factor(TestData$marital)
TestData$education <- as.factor(TestData$education)
TestData$default <- as.factor(TestData$default)
TestData$housing <- as.factor(TestData$housing)
TestData$loan <- as.factor(TestData$loan)
TestData$contact <- as.factor(TestData$contact)
TestData$month <- as.factor(TestData$month)
TestData$day_of_week <- as.factor(TestData$day_of_week)
TestData$poutcome <- as.factor(TestData$poutcome)
str(TestData)

md.pattern(TestData)
TestData.final <- mice(TestData,m=5,maxit=50,meth='pmm',seed=500 )
TestData.final$imp
TestData.final <- complete(TestData.final,1)

#####################################Test Prep Complete#####




data.csv <- read.csv("train_set_v1.csv",na.strings = TRUE)
str(data.csv)
data.xl <- readxl::read_xlsx("train_set_v1.xlsx")
head(data.xl)
str(data.xl)
summary(data.xl)
colSums(is.na(data.xl))
sum(is.na(data.xl))
str(data.xl)
names(data.xl)

data_new <- data.xl[,-c(1,3,14,17)]
names(data_new)
str(data_new)
colSums(is.na(data_new))
sum(is.na(data_new))
str(data_new)

data_new$marital <- as.factor(data_new$marital)
data_new$education <- as.factor(data_new$education)
data_new$default <- as.factor(data_new$default)
data_new$housing <- as.factor(data_new$housing)
data_new$loan <- as.factor(data_new$loan)
data_new$contact <- as.factor(data_new$contact)
data_new$month <- as.factor(data_new$month)
data_new$day_of_week <- as.factor(data_new$day_of_week)
data_new$poutcome <- as.factor(data_new$poutcome)
data_new$Term<- as.factor(data_new$Term)
str(data_new)


md.pattern(data_new)
dataNew <- mice(data_new,m=5,maxit=50,meth='pmm',seed=500 )
dataNew$imp
dataNew <- complete(dataNew,1)
#data_new <-na.omit(data_new)
#str(data_new)
colSums(is.na(dataNew))
str(dataNew)
names(dataNew)

summary(dataNew$duration)
#Correlation
corrplot::corrplot(cor(dataNew[,c(1,10,11,12,14,15,16,17)]),method = "number",type = "lower",number.cex = 0.6)


#Outlier Capping

summary(dataNew)
#Outlier Treatment Age
qnt <- quantile(dataNew$age, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataNew$age, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataNew$age, na.rm = T)
dataNew$age[dataNew$age < (qnt[1] - H)] <- caps[1]  # Flooring
dataNew$age[dataNew$age > (qnt[2] + H)] <- caps[2]  # Ceiling


#Outlier Treatment duration
qnt <- quantile(dataNew$duration, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataNew$duration, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataNew$duration, na.rm = T)
dataNew$duration[dataNew$duration < (qnt[1] - H)] <- caps[1]  # Flooring
dataNew$duration[dataNew$duration > (qnt[2] + H)] <- caps[2]  # Ceiling


#Outlier Treatment duration
qnt <- quantile(dataNew$duration, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataNew$duration, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataNew$duration, na.rm = T)
dataNew$duration[dataNew$duration < (qnt[1] - H)] <- caps[1]  # Flooring
dataNew$duration[dataNew$duration > (qnt[2] + H)] <- caps[2]  # Ceiling

View(dataNew)

#Outlier Treatment Campaign
qnt <- quantile(dataNew$campaign, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataNew$campaign, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataNew$campaign, na.rm = T)
dataNew$campaign[dataNew$campaign < (qnt[1] - H)] <- caps[1]  # Flooring
dataNew$campaign[dataNew$campaign > (qnt[2] + H)] <- caps[2]  # Ceiling


#Outlier Treatment previous
qnt <- quantile(dataNew$previous, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataNew$previous, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataNew$previous, na.rm = T)
dataNew$previous[dataNew$previous < (qnt[1] - H)] <- caps[1]  # Flooring
dataNew$previous[dataNew$previous > (qnt[2] + H)] <- caps[2]  # Ceiling

#Outlier Treatment cons.price.idx
qnt <- quantile(dataNew$cons.price.idx, probs=c(.25, .75), na.rm = T)
caps <- quantile(dataNew$cons.price.idx, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(dataNew$cons.price.idx, na.rm = T)
dataNew$cons.price.idx[dataNew$cons.price.idx < (qnt[1] - H)] <- caps[1]  # Flooring
dataNew$cons.price.idx[dataNew$cons.price.idx > (qnt[2] + H)] <- caps[2]  # Ceiling

summary(dataNew)
boxplot(dataNew$age)
boxplot(dataNew$duration)
boxplot(dataNew$campaign)
boxplot(dataNew$previous)
boxplot(dataNew$cons.price.idx)
boxplot(dataNew$nr.employed)
dataNew
#Outliers check
outCheck <- lm(as.numeric(dataNew$Term)~.,data = dataNew)
cooks_D <- cooks.distance(outCheck)

plot(cooks_D)
abline(h=4*mean(cooks_D,na.rm = TRUE),col="red")

influential <- as.numeric(names(cooks_D)[(cooks_D > 4*mean(cooks_D, na.rm=T))])
out <- dataNew[influential, ]  # influential observations.
dim(out)
View(out)

names(dataNew.out)
library(dplyr)
dataNew.out <- anti_join(dataNew, out)
str(dataNew.out)
dim(dataNew.out)

summary(dataNew.out1)

#mahal <- mahalanobis(dataNew[,-c(2,3,4,5,6,7,8,9,13,18)],
                     colMeans(dataNew[,-c(2,3,4,5,6,7,8,9,13,18)],na.rm = TRUE),
                     cov(dataNew[,-c(2,3,4,5,6,7,8,9,13,18)],use = "pairwise.complete.obs"))
#summary(mahal<cutoff)
#cutoff <- qchisq(.995,ncol(dataNew[,-c(2,3,4,5,6,7,8,9,13,18)])
#noout <- dataNew[mahal<cutoff,]

#dim(dataNew)
#dim(noout)                 

logit.Model1 <- glm(dataNew$Term~.,data = dataNew,family = "binomial")
summary(logit.Model1)
logit.pred1 <- predict(logit.Model1,dataNew, type = "response")
logit.pred11 <- ifelse(logit.pred1<.5,0,1)
logit.pred11<- as.factor(logit.pred11)
caret::confusionMatrix(logit.pred11,dataNew$Term)

test.pred1 <- predict(logit.Model1, newdata=TestData.final, type = "response" )
test.pred11 <- ifelse(test.pred1<.5,0,1)
test.pred11 <- as.factor(test.pred11)

predicted <-cbind(Test_data$ID,test.pred11)
#View(predicted3)
predicted<-as.data.frame(predicted)
predicted$test.pred11 <-ifelse(predicted$test.pred11 == "1",0,1)
colnames(predicted) <- c("ID","Term")
View(predicted)
write.csv(predicted,"D:/Great Learning/HackAll/predicted.csv")


glmControl <- trainControl(method = "repeatedcv",number = 10, repeats = 5)
logit.Model2 <- train(Term ~.,
                     data = dataNew,
                     method = "glmnet",
                     trControl = glmControl,
                     tuneLength=10)

logit.pred2 <- predict(logit.Model2, dataNew)
caret::confusionMatrix(dataNew$Term, logit.pred2)

test.pred2 <- predict(logit.Model2, newdata=TestData.final )
test.pred21 <- ifelse(test.pred1<.5,0,1)
test.pred21 <- as.factor(test.pred11)

predicted <-cbind(Test_data$ID,test.pred11)
#View(predicted3)
predicted<-as.data.frame(predicted)
predicted$test.pred11 <-ifelse(predicted$test.pred11 == "1",0,1)
colnames(predicted) <- c("ID","Term")
View(predicted)
write.csv(predicted,"D:/Great Learning/HackAll/predicted.csv")


# XGBoosing
xgbModel1 <- train(Term~.,dataNew,
                   trControl = trainControl("cv", number = 5),method = "xgbTree",tuneLength=10)

summary(xgbModel1)

xgb1.pred <- predict(xgbModel1,dataNew)
caret::confusionMatrix(dataNew$Term,xgb1.pred)





#RF Model

RFModel <- randomForest::randomForest(Term~.,data = dataNew,type = "class", mtry = 3,
                                      nodesize = 10, ntree= 1200, importance = TRUE,
                                      tuneLength =10)
summary(RFModel)
predrf <- predict(RFModel,dataNew)
caret::confusionMatrix(predrf, dataNew$Term)

predtest <- predict(RFModel,TestData.final)
dataset <- cbind(Test_data$ID,predtest)
View(dataset)
colnames(dataset)<- c("ID","Term")
dataset<- as.data.frame(dataset)
dataset$Term<- ifelse(dataset$Term==1,0,1)

write.csv(dataset,"D:/Great Learning/HackAll/dataset.csv")


str(TestData.final)
str(dataNew)
sapply(dataNew, levels)

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Term~., data=dataNew, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)




library(e1071)

svmmodel <- svm(Term~.,dataNew)

predsvm <- predict(svmmodel,dataNew)


svm_tune <- tune(svm, Term~ .,data = dataNew,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
)

bestsvm <- svm_tune$best.model
??tune


predsvm <- predict(bestsvm,dataNew)

confusionMatrix(predsvm,dataNew$Term)

predsvmtest <- predict(bestsvm,TestData.final)

model <- train(Term ~ ., data = dataNew,method = "vglmCumulative")
control <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)

predmodel <- predict(model,dataNew)

confusionMatrix(predmodel,dataNew$Term)

predtest1 <- predict(model,TrainData.Final)



dataset <- cbind(Test_data$ID,predsvmtest)
View(dataset)
colnames(dataset)<- c("ID","Term")
dataset<- as.data.frame(dataset)
dataset$Term<- ifelse(dataset$Term==1,0,1)

write.csv(dataset,"D:/Great Learning/HackAll/dataset.csv")