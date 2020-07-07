library(caTools)
library(rpart)
library(rpart.plot)
setwd("D:/Great Learning/HackAll")

data.xl <- readxl::read_xlsx("train_set_v1.xlsx")
head(data.xl)
str(data.xl)
summary(data.xl)
colSums(is.na(data.xl))
sum(is.na(data.xl))
str(data.xl)
names(data.xl)

data_new <- data.xl[,-c(1,3,17)]
colSums(is.na(data_new))
sum(is.na(data_new))
str(data_new)
data_new <-na.omit(data_new)
str(data_new)
colSums(is.na(data_new))
str(data.csv)
??dummyfy


??prediction
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


# Cart 1
cartModel <- rpart(data_new$Term~.,data = data_new,method = "class")
cartModel
prp(cartModel)
rpart.plot(cartModel)
cartTrain <- predict(cartModel, data_new, type = "class")
cartTrain
str(cartTrain)
caret::confusionMatrix(cartTrain,data_new$Term)




cartTest <- predict(cartModel, testData, type = "class")
caret::confusionMatrix(cartTest,testData$Term)

Test_data<- read.csv("test_set.csv")
Test_data.xl <- readxl::read_xlsx("test_set.xlsx")
TestData <- Test_data.xl[,-c(1,3,17)]
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
str(data_new)

CartUnseen <- predict(cartModel, newdata = TestData ,type = "class")
View(CartUnseen)
predicted <-cbind(Test_data$ID,CartUnseen)
View(predicted)
predicted<-as.data.frame(predicted)
predicted$CartUnseen 
predicted$CartUnseen <-ifelse(predicted$CartUnseen == "1",0,1)
colnames(predicted) <- c("ID","Term")

str(predicted)


#Cart 2

ctrl <- rpart.control(minsplit = 180,xval = 100)
cartModel2 <- rpart(trainData$Term~., data = trainData,control = ctrl,method = "class")
cartModel2
prp(cartModel2)
rpart.plot(cartModel2)
cartTrain2 <- predict(cartModel2, trainData, type = "class")
cartTrain2
caret::confusionMatrix(cartTrain2,trainData$Term)
str(cartTrain2)


cartTest2 <- predict(cartModel2, testData, type = "class")
caret::confusionMatrix(cartTest2,testData$Term)


CartUnseen2 <- predict(cartModel2, newdata = TestData ,type = "class")
View(CartUnseen2)
predicted2 <-cbind(Test_data$ID,CartUnseen2)
View(predicted2)
predicted2<-as.data.frame(predicted2)
predicted2$CartUnseen2 
predicted2$CartUnseen2 <-ifelse(predicted2$CartUnseen2 == "1",0,1)
colnames(predicted2) <- c("ID","Term")


#Cart 3


ctrl <- rpart.control(minsplit = 100,xval = 100)
cartModel3 <- rpart(trainData$Term~., data = trainData,control = ctrl,method = "class")
cartModel3
prp(cartModel3)
rpart.plot(cartModel3)
cartTrain3 <- predict(cartModel3, trainData, type = "class")
cartTrain3
caret::confusionMatrix(cartTrain3,trainData$Term)
str(cartTrain3)


cartTest3 <- predict(cartModel3, testData, type = "class")
caret::confusionMatrix(cartTest3,testData$Term)


CartUnseen3 <- predict(cartModel3, newdata = TestData ,type = "class")
#View(CartUnseen3)
predicted3 <-cbind(Test_data$ID,CartUnseen3)
#View(predicted3)
predicted3<-as.data.frame(predicted3)
predicted3$CartUnseen3
predicted3$CartUnseen3 <-ifelse(predicted3$CartUnseen3 == "1",0,1)
colnames(predicted3) <- c("ID","Term")

write.csv(predicted3,"D:/Great Learning/HackAll/predicted3.csv")


#Cart4


printcp(cartModel3)
cartprune <- prune(cartModel3,cp = 0.010412,"CP")
prunepred <- predict(cartprune,newdata = TestData, type = "class")
predicted4 <-cbind(Test_data$ID,prunepred)
#View(predicted3)
predicted4<-as.data.frame(predicted4)
predicted4$prunepred <-ifelse(predicted4$prunepred == "1",0,1)
colnames(predicted4) <- c("ID","Term")

write.csv(predicted4,"D:/Great Learning/HackAll/predicted4.csv")



RFModel <- randomForest::randomForest(Term~.,data = trainData,type = "class", mtry = 3,
                                      nodesize = 10, ntree= 1200, importance = FALSE)
summary(RFModel)

rfpred <- predict(RFModel, newdata = trainData,type = "class")
caret::confusionMatrix(rfpred,trainData$Term)

rfpred.Test <- predict(RFModel,newdata = testData,type = "class")
caret::confusionMatrix(rfpred.Test,testData$Term)
names(testData)
test<- testData[,-19] 
test <- rbind(test[1,],TestData)
test<- test[-1,]
View(test)
rfpred.unseen <- predict(RFModel, newdata = unseen_test, type = "class")
View(rfpred.unseen)
rfFinal <-cbind(Test_data$ID,rfpred.unseen)
#View(predicted3)
rfFinal<-as.data.frame(rfFinal)
View(rfFinal)
rfFinal$rfpred.unseen

rfFinal$rfpred.unseen <-ifelse(rfFinal$rfpred.unseen == "1",0,1)
colnames(rfFinal) <- c("ID","Term")
View(rfFinal)
write.csv(rfFinal,"D:/Great Learning/HackAll/rffinal.csv")

library(randomForest)
names(trainData)
tRF <- tuneRF(x = trainData[,-19],
              y=trainData$Term,
              mtryStart = 3,
              ntreeTry=15000,
              stepFactor = 1.5,
              improve = 0.0001,
              trace=TRUE,
              plot = TRUE,
              doBest = TRUE,
              nodesize = 50,
              importance=TRUE
)

 ?tuneRF
summary(tRF)
summary(tRF$forest)

rfpred2 <- predict(tRF,trainData,type = "class")
caret::confusionMatrix(trainData$Term,rfpred2)
rfpredTest2 <- predict(tRF,testData, type = "class")
caret::confusionMatrix(testData$Term,rfpredTest2)

unseen_test <- rbind(testData[1,-19],completeDData)
unseen_test<-rbind(unseen_test[-1,])
rfFinal2 <- predict(tRF,newdata = unseen_test,type = "class")

rfFinal2.sol<- cbind(Test_data$ID,rfFinal2)
View(rfFinal2.sol)
rfFinal2.sol<- as.data.frame(rfFinal2.sol)
rfFinal2.sol$rfFinal2 <- ifelse(rfFinal2.sol$rfFinal2 ==1,0,1)
View(rfFinal2.sol)
colnames(rfFinal2.sol) <- c("ID","Term")
View(rfFinal2.sol)
write.csv(rfFinal2.sol,"D:/Great Learning/HackAll/rffinal2.1.csv")

#RF Using train
library(caret)
control <- trainControl(method = "cv",number = 10, repeats = 3)
metric <- "Accuracy"
mtry <- 5
tunegrid <- expand.grid(mtry=mtry)

rf.Train <- train(Term~.,data=trainData,method = "rf",metric = metric, tuneGrid = tunegrid,trControl=control)

summary(rf.Train)


#XGBoostinggg
str(trainData)
trainData$marital<- as.numeric(trainData$marital)
trainData$education<- as.numeric(trainData$education)
trainData$default<- as.numeric(trainData$default)
trainData$housing<- as.numeric(trainData$housing)
trainData$loan<- as.numeric(trainData$loan)
trainData$contact<- as.numeric(trainData$contact)
trainData$month<- as.numeric(trainData$month)
trainData$day_of_week<- as.numeric(trainData$day_of_week)
trainData$poutcome<- as.numeric(trainData$poutcome)
trainData$Term<- as.numeric(trainData$Term)
str(trainData)

testData$marital<- as.numeric(testData$marital)
testData$education<- as.numeric(testData$education)
testData$default<- as.numeric(testData$default)
testData$housing<- as.numeric(testData$housing)
testData$loan<- as.numeric(testData$loan)
testData$contact<- as.numeric(testData$contact)
testData$month<- as.numeric(testData$month)
testData$day_of_week<- as.numeric(testData$day_of_week)
testData$poutcome<- as.numeric(testData$poutcome)
testData$Term<- as.numeric(testData$Term)
str(trainData)
trainData$Term <- ifelse(trainData$Term =="1",0,1)
testData$Term <- ifelse(testData$Term =="1",0,1)

features_train<- as.matrix(trainData[,1:18])
label_train <- as.matrix(trainData[,19])

features_test<- as.matrix(testData[,1:18])

library(xgboost)
xgbModel <- xgboost(
  data = features_train,
  label = label_train,
  eta = 0.01,
  max_depth = 100,
  min_child_weight = 3,
  nrounds = 1000,
  nfold = 10,
  objective = "reg:logistic",
  verbose = 0,
  early_stopping_rounds = 10)

summary(xgbModel)

# Performing Prediction on Train data.
#str(label_train)
xgb.train.pred<- predict(xgbModel, newdata = features_train)
xgb.train.pred1 <- ifelse(xgb.train.pred<.4,0,1)
xgb.train.pred1<- as.factor(xgb.train.pred1)
trainData$Term <- as.factor(trainData$Term)
#str(trainData$Term)
#str(xgb.train.pred1)
#dim(trainData$Term)
#dim(p)

caret::confusionMatrix(xgb.train.pred1, trainData$Term)



xgb.test.pred  <- predict(xgbModel,newdata = features_test)
xgb.test.pred1 <- ifelse(xgb.test.pred<.4,0,1)
xgb.test.pred1<- as.factor(xgb.test.pred1)
testData$Term <- as.factor(testData$Term)
$str(testData)
caret::confusionMatrix(xgb.test.pred1, testData$Term)


unseen_test <- rbind(testData[1,-19],completeDData)
unseen_test<-rbind(unseen_test[-1,])
str(unseen_test)

unseen_test$marital<- as.numeric(unseen_test$marital)
unseen_test$education<- as.numeric(unseen_test$education)
unseen_test$default<- as.numeric(unseen_test$default)
unseen_test$housing<- as.numeric(unseen_test$housing)
unseen_test$loan<- as.numeric(unseen_test$loan)
unseen_test$contact<- as.numeric(unseen_test$contact)
unseen_test$month<- as.numeric(unseen_test$month)
unseen_test$day_of_week<- as.numeric(unseen_test$day_of_week)
unseen_test$poutcome<- as.numeric(unseen_test$poutcome)
str(unseen_test)
mat.given <- as.matrix(unseen_test)

xgbFinal <- predict(xgbModel,newdata = mat.given)
xgbFinal1 <- ifelse(xgbFinal<.5,0,1)

xgbFinal.sol<- cbind(Test_data$ID,xgbFinal1)
View(xgbFinal.sol)
xgbFinal.sol<- as.data.frame(xgbFinal.sol)
xgbFinal.sol$xgbFinal1<- ifelse(xgbFinal.sol$xgbFinal1 ==1,0,1)
View(xgbFinal.sol)
colnames(xgbFinal.sol) <- c("ID","Term")
View(xgbFinal.sol)
write.csv(xgbFinal.sol,"D:/Great Learning/HackAll/xgbFinal.sol.csv")

str(trainData)


xgbModel2 <- train(Term~.,trainData,
                 trControl = trainControl("cv", number = 15),method = "xgbTree")
summary(xgbModel2)

xgb2.train.pred <- predict(xgbModel2,trainData)
caret::confusionMatrix(trainData$Term,xgb2.train.pred)

xgb2.test.pred <- predict(xgbModel2,testData)
caret::confusionMatrix(xgb2.test.pred, testData$Term)

xgbFinal2 <- predict(xgbModel2, unseen_test)
xgbFinal2.sol<- cbind(Test_data$ID,xgbFinal2)
View(xgbFinal2.sol)
xgbFinal2.sol<- as.data.frame(xgbFinal2.sol)
xgbFinal2.sol$xgbFinal2<- ifelse(xgbFinal2.sol$xgbFinal2 ==1,0,1)
View(xgbFinal2.sol)
colnames(xgbFinal2.sol) <- c("ID","Term")
View(xgbFinal2.sol)
write.csv(xgbFinal2.sol,"D:/Great Learning/HackAll/xgbFinal2.sol.csv")




logitModel<- glm(Term~., data = trainData,family = "binomial")
summary(logitModel)
logit.pred.train <- predict(logitModel, trainData,type = "response")
logit.pred.train1 <- ifelse(logit.pred.train<.5,0,1)
logit.pred.train1<- as.factor(logit.pred.train1)
caret::confusionMatrix(trainData$Term,logit.pred.train1)

logit.pred.test <- predict(logitModel, newdata = testData,type = "response")
logit.pred.test1 <- ifelse(logit.pred.test<.5,0,1)
logit.pred.test1<- as.factor(logit.pred.test1)
caret::confusionMatrix(logit.pred.test1,testData$Term)
View(completeDData)
str(trainData)
given.logit.pred  <- predict(logitModel, newdata = unseen_test,type = "response")
given.logit.pred1 <- ifelse(given.logit.pred<.5,0,1)

logitFinal.sol<- cbind(Test_data$ID,given.logit.pred1)
View(logitFinal.sol)
logitFinal.sol<- as.data.frame(logitFinal.sol)
xgbFinal2.sol$xgbFinal2<- ifelse(xgbFinal2.sol$xgbFinal2 ==1,0,1)
View(logitFinal.sol)
colnames(logitFinal.sol) <- c("ID","Term")
View(logitFinal.sol)
write.csv(logitFinal.sol,"D:/Great Learning/HackAll/logitFinal.sol.csv")



nn.dev <- neuralnet(Term~.,
                    data = trainData, 
                    #hidden = c(4,2),
                    err.fct = "sse",
                    linear.output = FALSE,
                    lifesign = "full",
                    lifesign.step = 1,
                    threshold = .09,
                    stepmax = 2000
)
plot(nn.dev)
quantile(nn.dev$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

misClassTable = data.frame(trainData$Term, Predict.score = nn.dev$net.result[[1]] )
misClassTable$Predict.class = ifelse(misClassTable$Predict.score>0.6,1,0)
with(misClassTable, table(df_train$Target ,Predict.class))