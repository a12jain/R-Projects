
# Deploying the Libraries

library(readxl)
library(mice)
library(corrplot)
library(DMwR)
library(e1071)
library(car)
library(blorr)
library(MASS)
library(caret)
library(psych)
library(tidyverse)
library(DataExplorer)

# Setting up the Working Directory.

setwd("D:/Great Learning/Finance and Risk Analytics")

# Reading the Raw data.

raw_data <- read_xlsx("raw-data.xlsx")

# Transforming the names of the Variables

colnames(raw_data)<- make.names(colnames(raw_data))

# Creating Default variable basis of Net Worth Next Year

raw_data$Default <- ifelse(raw_data$Networth.Next.Year <0,1,0)

#Perfroming Exploratory Data Analysis

# Checking the first 10 rows of the dataset

head(raw_data,n=10)

# Checking the dimensions of the data

dim(raw_data)

# Understanding the structure of the dataset

str(raw_data)

# Understanding the basic summary of the dataset

summary(raw_data)


# Checking for the Missing Values in the dataset
colSums(is.na(raw_data))

# Removing the unnecessary variables and variables with missing value greater then 25%
names(raw_data)
data <- raw_data[,-c(1,2,18,19,22,25,32,34,42:47)]

# Converting datatypes of the variables
data$Equity.face.value <- as.numeric(data$Equity.face.value)
data$PE.on.BSE <- as.numeric(data$PE.on.BSE)
data$Default <- as.factor(data$Default)

# Imputing the missing Values.

data1 <- mice(data,m=5,maxit=10,meth='pmm',seed=500)
data1$imp
data_new <- complete(data1,1)
colSums(is.na(data_new))
str(data)

data_new <- na.omit(data_new)
dim(data_new)
names(data)

#Perfroming Univariate Analysis
dev.off()

plot_histogram(data_new[1:8])

boxplot(data_new[1:8],horizontal = TRUE)

plot_histogram(data_new[9:16])

boxplot(data_new[9:16],horizontal = TRUE)

plot_histogram(data_new[17:24])

boxplot(data_new[17:24],horizontal = TRUE)

plot_histogram(data_new[25:32])

boxplot(data_new[25:32],horizontal = TRUE)

plot_histogram(data_new[33:39])

boxplot(data_new[33:39],horizontal = TRUE)


#Performing Bi-Variate Analaysis

ggplot(data_new, aes(x=data_new$Total.assets)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,15000)

boxplot(data_new$Total.assets~data_new$Default, horizontal = TRUE)

ggplot(data_new, aes(x=data_new$Net.worth)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,10000)

boxplot(data_new$Net.worth~data_new$Default, horizontal = TRUE)

ggplot(data_new, aes(x=data_new$Total.income)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,10000)

boxplot(data_new$Total.income~data_new$Default, horizontal = TRUE)


ggplot(data_new, aes(x=data_new$Total.expenses)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,10000)

boxplot(data_new$Total.expenses~data_new$Default, horizontal = TRUE)


ggplot(data_new, aes(x=data_new$Cash.profit)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,2000)

boxplot(data_new$Cash.profit~data_new$Default, horizontal = TRUE)


ggplot(data_new, aes(x=data_new$Profit.after.tax)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,2000)

boxplot(data_new$Profit.after.tax~data_new$Default, horizontal = TRUE)


ggplot(data_new, aes(x=data_new$Sales)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,40000)

boxplot(data_new$Sales~data_new$Default, horizontal = TRUE)


ggplot(data_new, aes(x=data_new$Reserves.and.funds)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,5000)

boxplot(data_new$Reserves.and.funds~data_new$Default, horizontal = TRUE)


ggplot(data_new, aes(x=data_new$Current.liabilities...provisions)) +   
  geom_density(aes(fill =data_new$Default, alpha = 0.3)) +   
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) +    
  scale_fill_manual(values = c("darkturquoise","lightcoral","lightgreen"))+
  xlim(-5,5000)

boxplot(data_new$Current.liabilities...provisions~data_new$Default, horizontal = TRUE)


#Checking for the Multicollinearity

plot_correlation(data_new)

data <- data_new
dim(data)
corrplot(cor(data[,-39]),method = "number", type = "lower", number.cex = 0.7)

# Creating new variables using the standalone Variables

data$Borrowings.per.total.Capital <- data$Borrowings/data$Total.capital
data$Borrowings.per.total.asset <- data$Borrowings/data$Total.assets
data$capital.Employed.per.total.income <- data$Capital.employed/data$Total.income
data$cash.profit.per.total.asset <- data$Cash.profit/data$Total.assets
data$Change.in.stock.per.total.Income <- data$Change.in.stock/data$Total.income
data$Cumulative.retained.profits.per.sale <- data$Cumulative.retained.profits/data$Sales
data$Current.assets.per.total.asset <- data$Current.assets/data$Total.assets
data$Current.liabilities...provisions.per.total.asset <- data$Current.liabilities...provisions/data$Total.assets
data$Current.liabilities...provisions.per.current.asset <- data$Current.liabilities...provisions/data$Current.assets
data$Net.fixed.assets.per.total.asset <- data$Net.fixed.assets/data$Total.assets
data$Net.working.capital.per.total.asset <- data$Net.working.capital/data$Total.assets
data$Net.working.capital.per.total.capital <- data$Net.working.capital/data$Total.capital
data$PAT.per.Sales <- data$Profit.after.tax/data$Sales
data$PAT.per.total.asset <- data$Profit.after.tax/data$Total.assets
data$PBDITA.per.Sales <- data$PBDITA/data$Sales
data$PBT.per.Sales <- data$PBT/data$Sales
data$PBT.per.total.asset <- data$PBT/data$Total.assets
data$PBT.per.Total.Capital <- data$PBT/data$Total.capital
data$Sales.per.total.asset <- data$Sales/data$Total.assets
data$Net.working.capital.per.sales <- data$Net.working.capital/data$Sales
data$Shareholder.fund.per.total.asset <- data$Shareholders.funds/data$Total.assets
data$Shareholder.fund.per.total.capital <- data$Shareholders.funds/data$Total.capital
data$Total.asset.per.Current.Liability <- data$Total.assets/data$Current.liabilities...provisions
data$Total.Income.per.Shareholder.fund <- data$Total.income/data$Shareholders.funds
data$Total.income.per.total.asset <- data$Total.income/data$Total.assets
data$Total.income.per.Sale <- data$Total.income/data$Sales
data$Total.income.per.total.expense <- data$Total.income/data$Total.expenses
data$Total.liabilities.per.shareholder.fund <- data$Total.liabilities/data$Shareholders.funds



dataset <- data
dim(dataset)
names(dataset)

# Treating the Outliers by Capping and Flooring method

dataset2 <- dataset
names(dataset2)
a <- c(1:38,40:67)

for(val in a){
  qnt<- quantile(dataset2[,val],probs = c(0.25,0.75))
  cap<- quantile(dataset2[,val],probs = c(0.05,0.95))
  
  h= 1.5*IQR(dataset2[,val])
  dataset2[,val][dataset2[,val]>(qnt[2]+h)]<- cap[2]
  dataset2[,val][dataset2[,val]<(qnt[1]-h)]<- cap[1]
}


# Checking the Proportion on Default.

table(dataset2$Default)
prop.table(table(dataset2$Default))

# Balancing the dataset.
anyNA(dataset2)
bal2 <- SMOTE(dataset2$Default~., data = dataset2,k=5)
#bal2 <-ROSE(Default~., data=dataset2, seed=3)$data
table(bal2$Default)
prop.table(table(bal2$Default))

#Building the Model with the dataset after performing the EDA.

attach(bal2)

glmModel<- glm(bal2$Default~.,data = bal2, family = "binomial")
summary(glmModel)
vif(glmModel)

glmPred <- predict(glmModel,data= bal2)
glmPred1 <- as.factor(ifelse(glmPred <0.6,0,1))
caret::confusionMatrix(glmPred1,as.factor(bal2$Default),positive='1')

# Refining the Model using Step Wise AIC.

stepAIC(glmModel,direction = "both")

glmModel1 <- glm(bal2$Default~Total.assets+Profit.after.tax+Cash.profit+
                   PBT.as...of.total.income+Cash.profit.as...of.total.income+
                   PAT.as...of.net.worth+Total.capital+Borrowings+Current.liabilities...provisions+
                   Cumulative.retained.profits+Contingent.liabilities...Net.worth....  
                 +Net.working.capital+Debt.to.equity.ratio..times.+ Cash.to.current.liabilities..times.+
                   PE.on.BSE+Borrowings.per.total.asset+capital.Employed.per.total.income
                 +Change.in.stock.per.total.Income+Cumulative.retained.profits.per.sale+
                   Current.assets.per.total.asset+Current.liabilities...provisions.per.current.asset+
                   Net.fixed.assets.per.total.asset+Net.working.capital.per.total.asset+
                   Shareholder.fund.per.total.asset+Total.income.per.Sale+
                   Total.liabilities.per.shareholder.fund 
                 ,data = bal2,
                 family = "binomial")

summary(glmModel1)

exp(glmModel1$coefficients)
lr.Test <- lmtest::lrtest(glmModel)
lr.Test

rSq<- pscl::pR2(glmModel)
rSq

# Checking the Variable Inflation Factor

vif(glmModel1)

# Predicting probability on Original Dataset

pred <- predict(glmModel1,data = bal2, type = "response")
pred1 <- ifelse(pred<0.4,0,1)
pred1 <- as.factor(pred1)
actuals <- as.factor(bal2$Default)
summary(pred)
# Creating Confusion Matrix on Original dataset

caret::confusionMatrix(pred1,actuals,positive='1')


lr.Test <- lmtest::lrtest(glmModel)
lr.Test

rSq<- pscl::pR2(glmModel)
rSq


#########Importing the Validation dataset###############
unseen <- read_xlsx('validation_data.xlsx')

# Perfroming Exploratory data Analysis

summary(unseen)

str(unseen)

colnames(unseen)<- make.names(colnames(unseen))
unseen1 <- unseen
str(unseen)

# Convering the datatype of variables

unseen1$Creditors.turnover <- as.numeric(unseen1$Creditors.turnover)
unseen1$Debtors.turnover <- as.numeric(unseen1$Debtors.turnover)
unseen1$Finished.goods.turnover <- as.numeric(unseen1$Finished.goods.turnover)
unseen1$WIP.turnover <- as.numeric(unseen1$WIP.turnover)
unseen1$Raw.material.turnover <- as.numeric(unseen1$Raw.material.turnover)
unseen1$Shares.outstanding <- as.numeric(unseen1$Shares.outstanding)
unseen1$Equity.face.value <- as.numeric(unseen1$Equity.face.value)
unseen1$PE.on.BSE <- as.numeric(unseen1$PE.on.BSE)
names(unseen1)

# Dropping variables that are dropped in Original to bring it to the same level

data_unseen <- unseen1[,-c(1,18,19,22,25,32,34,42:47)]

# Imputing the missing values

unseen <- mice(data_unseen,m=5,meth = "pmm",maxit = 10,seed = 500)
unseen$imp
unseen <- complete(unseen,1)
colSums(is.na(unseen))
unseen<-na.omit(unseen)
anyNA(unseen)

# Creating new variables that are created in Original Dataset to create it of the same level

unseen$Borrowings.per.total.Capital <- unseen$Borrowings/unseen$Total.capital
unseen$Borrowings.per.total.asset <- unseen$Borrowings/unseen$Total.assets
unseen$capital.Employed.per.total.income <- unseen$Capital.employed/unseen$Total.income
unseen$cash.profit.per.total.asset <- unseen$Cash.profit/unseen$Total.assets
unseen$Change.in.stock.per.total.Income <- unseen$Change.in.stock/unseen$Total.income
unseen$Cumulative.retained.profits.per.sale <- unseen$Cumulative.retained.profits/unseen$Sales
unseen$Current.assets.per.total.asset <- unseen$Current.assets/unseen$Total.assets
unseen$Current.liabilities...provisions.per.total.asset <- unseen$Current.liabilities...provisions/unseen$Total.assets
unseen$Current.liabilities...provisions.per.current.asset <- unseen$Current.liabilities...provisions/unseen$Current.assets
unseen$Net.fixed.assets.per.total.asset <- unseen$Net.fixed.assets/unseen$Total.assets
unseen$Net.working.capital.per.total.asset <- unseen$Net.working.capital/unseen$Total.assets
unseen$Net.working.capital.per.total.capital <- unseen$Net.working.capital/unseen$Total.capital
unseen$PAT.per.Sales <- unseen$Profit.after.tax/unseen$Sales
unseen$PAT.per.total.asset <- unseen$Profit.after.tax/unseen$Total.assets
unseen$PAT.per.total.income <- unseen$Profit.after.tax/unseen$Total.income
unseen$PBDITA.per.Sales <- unseen$PBDITA/unseen$Sales
unseen$PBT.per.Sales <- unseen$PBT/unseen$Sales
unseen$PBT.per.total.asset <- unseen$PBT/unseen$Total.assets
unseen$PBT.per.Total.Capital <- unseen$PBT/unseen$Total.capital
unseen$Sales.per.total.asset <- unseen$Sales/unseen$Total.assets
unseen$Net.working.capital.per.sales <- unseen$Net.working.capital/unseen$Sales
unseen$Shareholder.fund.per.total.asset <- unseen$Shareholders.funds/unseen$Total.assets
unseen$Shareholder.fund.per.total.capital <- unseen$Shareholders.funds/unseen$Total.capital
unseen$Total.asset.per.Current.Liability <- unseen$Total.assets/unseen$Current.liabilities...provisions
unseen$Total.Income.per.Shareholder.fund <- unseen$Total.income/unseen$Shareholders.funds
unseen$Total.income.per.total.asset <- unseen$Total.income/unseen$Total.assets
unseen$Total.income.per.Sale <- unseen$Total.income/unseen$Sales
unseen$Total.income.per.total.expense <- unseen$Total.income/unseen$Total.expenses
unseen$Total.liabilities.per.shareholder.fund <- unseen$Total.liabilities/unseen$Shareholders.funds

# Chnaging the name of the Default Variable

colnames(unseen)[colnames(unseen) == "Default...1"] <- "Default"

# Predicting the probability on Validation dataset

pred_unseen <- predict(glmModel1,newdata = unseen, type = "response")
unseen$probs <- pred_unseen
summary(unseen)
pred_unseen1 <- as.factor(ifelse(pred_unseen<0.6,0,1))
unseen$Predicted <- pred_unseen1
default <- as.factor(unseen$Defaul)

# Creating Confusion Matrix on Validation dataset

caret::confusionMatrix(pred_unseen1,default,positive='1')


# Deciling the data on probability

prob <- seq (0,1, length = 11) 
prob 
qs_train<- quantile(unseen$probs,prob,na.rm = TRUE) 
unseen$Decile <- cut(unseen$probs,unique(qs_train), include.lowest = TRUE, right = FALSE) 

unseen.decile <- unseen %>% mutate(quartile = ntile(-unseen$probs, 10))

table(unseen.decile$Default) 
defaulter<- data.table::data.table(unseen.decile) 
defaulter
unseen1_decile <- defaulter[,list(`# Defaulter` <- sum(Default==1),
                                  Non_Default <- sum(Default==0),
                                  Total  <-length(Default)) , by= quartile][order(quartile)]  
colnames(unseen1_decile) <- c('Groups','Defaults','Non_Default','Total')
unseen1_decile

