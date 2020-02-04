############ Mini Project 2 for Regression Model Factor Analysis

#Installing the required Packages.

library(psych)

library(corrplot)

library(caTools)

install.packages("DataExplorer")

library(DataExplorer)

install.packages("devtools")

library(devtools)

install_github("vqv/ggbiplot")


library(ggbiplot)


# Setting up Working Directory.

setwd("D:/Great Learning/Project 2")

#Reading the file to R.

dataset <- read.csv("Factor-Hair-Revised.csv")
str(dataset)
head(dataset)
dataset <- dataset[,-1]

#Creating a replica od dataset for future use
dataset1 <- dataset

### Performing Exploratory Data Analysis

#Checking for the data load efficiency
head(dataset1, n= 10)

# Checking for the Structure of the Dataset
str(dataset)

# Checking for the summary of the Dataset
summary(dataset)

# Looking for the Null values in the Dataset
is.null(dataset)

### Plotting the dataset

#Plotting the dataset to identify the Outliers.
#Plot2
boxplot(dataset[,1:4],horizontal = TRUE)

#Plot2
boxplot(dataset[,5:8],horizontal = TRUE)

#Plot3
boxplot(dataset[,9:1],horizontal = TRUE)

###Multivariate Analysis with the Independent variables on
###X axis and DependentVariable on Y axis.

#Plot: ProdQual vs Satisfaction.
plot(dataset$ProdQual,dataset$Satisfaction,col = "Red" )

#Plot: Ecom vs Satisfaction.
plot(dataset$Ecom,dataset$Satisfaction,col = "Red")

#Plot: TechSup vs Satisfaction.
plot(dataset$TechSup,dataset$Satisfaction,col = "Red")

#Plot: CompRes vs Satisfaction.
plot(dataset$CompRes,dataset$Satisfaction,col = "Red")

#Plot: Advertising vs Satisfaction.
plot(dataset$Advertising,dataset$Satisfaction,col = "Red")

#Plot: ProdLine vs Satisfaction.
plot(dataset$ProdLine,dataset$Satisfaction,col = "Red")

#Plot: SalesFImage vs Satisfaction.
plot(dataset$SalesFImage,dataset$Satisfaction,col = "Red")

#Plot: ComPricing vs Satisfaction.
plot(dataset$ComPricing,dataset$Satisfaction,col = "Red")

#Plot: WartyClaim vs Satisfaction.
plot(dataset$WartyClaim,dataset$Satisfaction,col = "Red")

#Plot: OrdBilling vs Satisfaction.
plot(dataset$OrdBilling,dataset$Satisfaction,col = "Red")

#Plot: DelSpeed vs Satisfaction.
plot(dataset$DelSpeed,dataset$Satisfaction,col = "Red")

###Performing Univariate Analysis on the dataset.

plot_histogram(dataset)



### Performing Simple Linear Regression on Individual Variables

# Simple Linear Regression Model on Satisfaction and ProdQual
slr_prodqual <- lm(Satisfaction~ProdQual, data = dataset1)
summary(slr_prodqual)

# Simple Linear Regression Model on Satisfaction and ECOM
slr_ecom <- lm(Satisfaction~Ecom, data = dataset1)
summary(slr_ecom)

# Simple Linear Regression Model on Satisfaction and TechSup
slr_Techsup <- lm(Satisfaction~TechSup, data = dataset1)
summary(slr_Techsup)

# Simple Linear Regression Model on Satisfaction and CompRes
slr_compres <- lm(Satisfaction~CompRes, data = dataset1)
summary(slr_compres)

# Simple Linear Regression Model on Satisfaction and Advertising
slr_advertising <- lm(Satisfaction~Advertising, data = dataset1)
summary(slr_advertising)

# Simple Linear Regression Model on Satisfaction and Prodline
slr_prodline <- lm(Satisfaction~ProdLine, data = dataset1)
summary(slr_prodline)

# Simple Linear Regression Model on Satisfaction and SalesFImage
slr_salesfimage <- lm(Satisfaction~SalesFImage, data = dataset1)
summary(slr_salesfimage)

# Simple Linear Regression Model on Satisfaction and Compricing
slr_compricing <- lm(Satisfaction~ComPricing, data = dataset1)
summary(slr_compricing)

# Simple Linear Regression Model on Satisfaction and WartyClaims
slr_wartyclaim <- lm(Satisfaction~WartyClaim, data = dataset1)
summary(slr_wartyclaim)

# Simple Linear Regression Model on Satisfaction and OrdBilling
slr_ordbilling <- lm(Satisfaction~OrdBilling, data = dataset1)
summary(slr_ordbilling)

# Simple Linear Regression Model on Satisfaction and DelSpeed
slr_delspeed <- lm(Satisfaction~DelSpeed, data = dataset1)
summary(slr_delspeed)


#Identifying the Correlation between the Independent variables.
# Removing the Dependent Variable to check the multicolinearity
# between the Independent Variables


dataset<- dataset[,-12]
str(dataset)
#Creating the correlation Matrix
mat <- cor(dataset)
mat

###Plotting the Correlation Matrix

# Plot 1
corrplot.mixed(mat, lower = "number", upper = "pie")

# Plot 2
plot_correlation(mat, title = "Correlation Plot")

#Performing Bartlett test and KMO Test to check the validity of the Correlation
#Bartlett Test: If p-Value < 0.05, correlation is valid

cortest.bartlett(mat,n = 100)
#$p.value = 1.79337e-96

#KMO Test: If the MSA > 0.5, correlation is valid
KMO(mat)
#Overall MSA =  0.65

### Performing Eigen Test

a = eigen(mat)
eigen_val <- a$values

#Plotting Eigen Values
plot(eigen_val)
lines(eigen_val, col = "red")
abline(h = 1)

### Performing PCA.

#Using the Kizer Rule, We take the number of factors for which the eigen values are greater then 1.
#In this case, we have 4 values which are greater the 1, hence we take 4 factors for PCA
#With Rotate = "None"

pca_no_rtt <- principal(dataset, nfactors = 4, rotate = "none")
pca_no_rtt

#Creating the Score Matrix.
pca_score_no_rtt <- pca_no_rtt$scores
head(pca_score_no_rtt)

#Joining the Score Matrix with the Original dataset to get the Satisfaction variable
pca_reg_no_rtt <- cbind(dataset1[,12],pca_score_no_rtt)

#Renaming the Factored dataset.
colnames(pca_reg_no_rtt) <- c("Satisfaction", "Factor1", "Factor2", "Factor3", "Factor4")
head(pca_reg_no_rtt)

pca_reg_no_rtt<- as.data.frame(pca_reg_no_rtt)

set.seed(42)

#Dividing the dataset into Test and Train. 
index_pca_no_rtt <- sample.split(pca_reg_no_rtt$Satisfaction,SplitRatio = .70)
train_pca_no_rtt <- subset(pca_reg_no_rtt,index_pca_no_rtt ==TRUE)
test_pca_no_rtt <- subset(pca_reg_no_rtt, index_pca_no_rtt == FALSE)

#Building the Regression Model on the Train Data.
model_pca_no_rtt<- lm(Satisfaction~.,data = train_pca_no_rtt)
summary(model_pca_no_rtt)

#Validating the Model on Test Data.
pred_pca_no_rtt <- predict(model_pca_no_rtt,newdata = test_pca_no_rtt)
summary(pred_pca_no_rtt)

SST_no_rtt <- sum((test_pca_no_rtt$Satisfaction - mean(test_pca_no_rtt$Satisfaction))^2)
SSR_no_rtt <- sum((pred_pca_no_rtt - mean(test_pca_no_rtt$Satisfaction))^2)
SSE_no_rtt <- sum((test_pca_no_rtt$Satisfaction - pred_pca_no_rtt)^2)

calculated_Rsq_no_rtt <- 1-(SSE_no_rtt/SST_no_rtt)
calculated_Rsq_no_rtt

#Using Rotate = "Varimax".

pca_wt_rtt <- principal(dataset, nfactors = 4, rotate = "varimax")
pca_wt_rtt

#Creating the Score Matrix.
pca_score_wt_rtt <- pca_wt_rtt$scores
head(pca_score_wt_rtt)

#Joining the Score Matrix with Original Dataset to get the Dependent Dataset
pca_reg_wt_rtt <- cbind(dataset1[,12],pca_score_wt_rtt)

#Renaming the Factored Dataset
colnames(pca_reg_wt_rtt) <- c("Satisfaction", "Factor1", "Factor2", "Factor3", "Factor4")
head(pca_reg_wt_rtt)

pca_reg_wt_rtt<- as.data.frame(pca_reg_wt_rtt)

set.seed(42)
#Dividing the dataset into Test and Train.
index_pca_wt_rtt <- sample.split(pca_reg_wt_rtt$Satisfaction,SplitRatio = .70)
train_pca_wt_rtt <- subset(pca_reg_wt_rtt,index_pca_wt_rtt ==TRUE)
test_pca_wt_rtt <- subset(pca_reg_wt_rtt, index_pca_wt_rtt == FALSE)

#Building the Model on Train Dataset
model_pca_wt_rtt<- lm(Satisfaction~.,data = train_pca_wt_rtt)
summary(model_pca_wt_rtt)



#Validating the Model on Test Data.
pred_pca_wt_rtt <- predict(model_pca_wt_rtt,newdata = test_pca_wt_rtt)

summary(pred_pca_wt_rtt)
SST_wt_rtt <- sum((test_pca_wt_rtt$Satisfaction - mean(test_pca_wt_rtt$Satisfaction))^2)
SSR_wt_rtt <- sum((pred_pca_wt_rtt - mean(test_pca_wt_rtt$Satisfaction))^2)
SSE_wt_rtt <- sum((test_pca_wt_rtt$Satisfaction - pred_pca_wt_rtt)^2)

calculated_Rsq_wt_rtt <- 1-(SSE_wt_rtt/SST_wt_rtt)
calculated_Rsq_wt_rtt




### Performing Factor Analysis.

#Using the Kizer Rule, We take the number of factors for which the eigen values are greater then 1.
#In this case, we have 4 values which are greater the 1, hence we take 4 factors for PCA.
#With Rotate = None

fa_no_rtt <- fa(dataset,nfactors = 4, rotate = "none", fm="pa")
fa_no_rtt

#Plotting the Factor Analysis
fa.diagram(fa_no_rtt)

#Creating the Score Matrix
fa_score_no_rtt <- fa_no_rtt$scores
head(fa_score_no_rtt)

#Joining the Score Matrix with the Original dataset to get the Satisfaction variable
fa_reg_no_rtt <- cbind(dataset1[,12],fa_score_no_rtt)

#Renaming the Factored dataset
colnames(fa_reg_no_rtt) <- c("Satisfaction", "Factor1", "Factor2", "Factor3", "Factor4")
head(fa_reg_no_rtt)

fa_reg_no_rtt<- as.data.frame(fa_reg_no_rtt)

set.seed(42)

#Dividing the Dataset to Test and Train Data
index <- sample.split(fa_reg_no_rtt$Satisfaction,SplitRatio = .70)
train_fa_no_rtt <- subset(fa_reg_no_rtt,index ==TRUE)
test_fa_no_rtt <- subset(fa_reg_no_rtt, index == FALSE)

#Building the Regression Model on Train Data
model_fa_no_rtt<- lm(Satisfaction~.,data = train_fa_no_rtt)
summary(model_fa_no_rtt)

#Validating the Model on Test Data
pred_fa_no_rtt <- predict(model_fa_no_rtt,newdata = test_fa_no_rtt)
summary(pred_fa_no_rtt)

SST_fa_no_rtt <- sum((test_fa_no_rtt$Satisfaction - mean(test_pca_no_rtt$Satisfaction))^2)
SSR_fa_no_rtt <- sum((pred_fa_no_rtt - mean(test_fa_no_rtt$Satisfaction))^2)
SSE_fa_no_rtt <- sum((test_fa_no_rtt$Satisfaction - pred_fa_no_rtt)^2)

calculated_Rsq_fa_no_rtt <- 1-(SSE_fa_no_rtt/SST_fa_no_rtt)
calculated_Rsq_fa_no_rtt

#Using the Keiser Law.
#In this case, we have 4 values which are greater the 1, hence we take 4 factors for FA.
#With rotate = Varimax
fa_wt_rtt <- fa(dataset,nfactors =4, rotate = "varimax", fm="pa")
fa_wt_rtt

fa.diagram(fa_wt_rtt)

#Creating the Score Matrix
fa_score_wt_rtt <- fa_wt_rtt$scores
head(fa_score_wt_rtt)

#Joining the Score Matrix with the Original dataset to get the Satisfaction variable
fa_reg_wt_rtt <- cbind(dataset1[,12],fa_score_wt_rtt)

#Renaming the Factored Dataset 
colnames(fa_reg_wt_rtt) <- c("Satisfaction", "Factor1", "Factor2", "Factor3", "Factor4")
head(fa_reg_wt_rtt)

fa_reg_wt_rtt<- as.data.frame(fa_reg_wt_rtt)

#Splitting the data into Test and Train.
set.seed(42)
index <- sample.split(fa_reg_wt_rtt$Satisfaction,SplitRatio = .70)
train_fa_wt_rtt <- subset(fa_reg_wt_rtt,index ==TRUE)
test_fa_wt_rtt <- subset(fa_reg_wt_rtt, index == FALSE)

#Building the Model
model_fa_wt_rtt<- lm(Satisfaction~.,data = train_fa_wt_rtt)
summary(model_fa_wt_rtt)


#Validating the Model on the Test Data.
pred_fa_wt_rtt <- predict(model_fa_wt_rtt,newdata = test_fa_wt_rtt)
summary(pred_fa_wt_rtt)
SST_fa_wt_rtt <- sum((test_fa_wt_rtt$Satisfaction - mean(test_fa_wt_rtt$Satisfaction))^2)
SSR_fa_wt_rtt <- sum((pred_fa_wt_rtt - mean(test_fa_wt_rtt$Satisfaction))^2)
SSE_fa_wt_rtt <- sum((test_fa_wt_rtt$Satisfaction - pred_fa_wt_rtt)^2)

calculated_Rsq_fa_wt_rtt <- 1-(SSE_fa_wt_rtt/SST_fa_wt_rtt)
calculated_Rsq_fa_no_rtt



