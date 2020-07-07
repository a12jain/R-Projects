
install.packages("readr")
library(readxl)
library(DataExplorer)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
install.packages("randomForest")
library(randomForest)
# Setting the Working Directory.

setwd("D:/Great Learning/Project 3")

# Reading the Dataset
theraData <- read_xlsx("TheraBank.xlsx", col_names = TRUE)

# Performing Exploratory Data Analysis
head(theraData)

View(theraData)

names(theraData)

names(theraData) <- make.names(c("ID","Age (in years)","Experience (in years)","Income (in K/month)","ZIP Code","Family members",
             "CCAvg","Education","Mortgage","Personal Loan","Securities Account","CD Account","Online","CreditCard"))
theraData<- theraData[,-1]

str(theraData)

summary(theraData)

# Removing negative records from the Dataset.
theraData <- subset(theraData, theraData$Experience..in.years. >= 0)

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
theraData<- theraData[,-2]

# Converting the variables to Factors

theraData$Education <- as.factor(theraData$Education)
theraData$Personal.Loan<- as.factor(theraData$Personal.Loan)
theraData$`Securities Account` <- as.factor(theraData$`Securities Account`)
theraData$`CD Account` <- as.factor(theraData$`CD Account`)
theraData$Online <- as.factor(theraData$Online)
theraData$CreditCard <- as.factor(theraData$CreditCard)

str(theraData)

nrow(theraData)

# Splitting data into Train and Test with a split of 70, 30 respectively.
set.seed(1000)
index <- sample.split(theraData$Personal.Loan,SplitRatio = 0.7)

Data_Train <- subset(theraData, index ==TRUE)
Data_Test <- subset(theraData, index ==F)

# Building CART Model on Train Data

Model_Cart <- rpart(`Personal Loan`~.,data = Data_Train,method = "class")
Model_Cart

prp(Model_Cart)
rpart.plot(Model_Cart,tweak = 1.2)

Model_Cart$cptable

model_rf <- randomForest(Personal.Loan~.,)