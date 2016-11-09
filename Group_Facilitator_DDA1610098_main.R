# Set Working Directory

getwd()   									              # Check for the Current Directory
setwd("C://Users//IBM_ADMIN//Desktop")    # Update the Working Directory to "C:/Users/IBM_ADMIN/Downloads"
getwd()									                	# Validate if the directory got updated or not



# Install and Load the required packages

install.packages("class")
install.packages("caret")
install.packages("ROCR")
install.packages("car")
install.packages("Hmisc")
install.packages("e1071")

library(class)   							# for KNN function for K-NN model
library(caret)								# for ConfusionMatrix Function
library(ROCR)								  # for ROCR Curve and KS Statistics for Logistic Regression
library(car)								  # for VIF function for Logistic regression
library(Hmisc)								# for C-statistics for Logistic regression
library(e1071)								# for naiveBayes function for naive-Bayes Model


# Load the given files.

customer <- read.csv("customer_data.csv")	# Import Source File in 3 different Data Frame
churn <- read.csv("churn_data.csv")
internet <- read.csv("internet_data.csv")

# Collate the 3 files in a single file.

mergeData <- merge(customer,churn,by.x = "customerID", all = T)
mergeData <- merge(mergeData,internet,by.x = "customerID", all = T)

# Understand the structure of the collated file.

str(mergeData)
summary(mergeData)

# Make bar charts to find interesting relationships between variables.

ggplot(mergeData,aes(x=Contract,fill=Churn))+geom_bar(position = "fill")   # Churning tendency is higher for month to month Contract Customer

ggplot(mergeData,aes(x=PaperlessBilling,fill=Churn))+geom_bar(position = "dodge")   # Churning is little bit higher for Paperless Billing Customer

ggplot(mergeData,aes(x=TechSupport,fill=Churn))+geom_bar(position = "fill")   # Churning is higher for customerwithout Techsupport Service

ggplot(mergeData,aes(x=InternetService,fill=Churn))+geom_bar(position = "fill")  # Churning is higher for Customer getting internet service via fiber optic in compare to DSL

ggplot(mergeData,aes(x=StreamingTV,fill=Churn))+geom_bar(position = "fill")       # No Effect on This variables on Churning
ggplot(mergeData,aes(x=StreamingMovies,fill=Churn))+geom_bar(position = "fill")   # No Effect on This variables on Churning
ggplot(mergeData,aes(x=DeviceProtection,fill=Churn))+geom_bar(position = "fill")  # Not much impact
ggplot(mergeData,aes(x=OnlineBackup,fill=Churn))+geom_bar(position = "fill")      # Notmuch impact

# Make Box plots for numeric variables to look for outliers. 

boxplot(mergeData$tenure)          # No outlier present in Tenure field
boxplot(mergeData$MonthlyCharges)  # No outlier present in monthly Charges field
boxplot(mergeData$TotalCharges)    # No outlier present in Total Charges field

# Perform De-Duplication if required

mergeData <- unique(mergeData)


# Impute the missing values, and perform the outlier treatment (if required).

sum(is.na(mergeData))   # 11 missing value present

# to find the presense of missing value in various variable

sapply(mergeData,function(x) sum(is.na(x)))  # all missing value is in TotalCharges field

mergeData$TotalCharges[which(is.na(mergeData$TotalCharges))] <- mean(mergeData$TotalCharges,na.rm = T)

# To check if missing values has been imputed or not

sum(is.na(mergeData))   # No missing value present now

# Bring the variables in the correct format

# Check for outlier

boxplot.stats(mergeData$tenure)          # No outlier present in Tenure field
boxplot.stats(mergeData$MonthlyCharges)  # No outlier present in monthly Charges field
boxplot.stats(mergeData$TotalCharges)    # No outlier present in Total Charges field

# Converting 2 level Factor field (gender,Partner,Dependents,PhoneService,PaperlessBilling,Churn) to Numeric one


# gender to numeric ( 1 means "Male"    0 means "Female" )

levels(mergeData$gender) <- c(0,1)
mergeData$gender <- as.numeric(levels(mergeData$gender))[mergeData$gender]

# Partner to numeric ( 1 means "Male"    0 means "Female" )

levels(mergeData$Partner) <- c(0,1)
mergeData$Partner <- as.numeric(levels(mergeData$Partner))[mergeData$Partner]

# Dependents to numeric ( 1 means "Yes"    0 means "No" )

levels(mergeData$Dependents) <- c(0,1)
mergeData$Dependents <- as.numeric(levels(mergeData$Dependents))[mergeData$Dependents]

# PhoneService to numeric ( 1 means "Yes"    0 means "No" )

levels(mergeData$PhoneService) <- c(0,1)
mergeData$PhoneService <- as.numeric(levels(mergeData$PhoneService))[mergeData$PhoneService]

# PaperlessBilling to numeric ( 1 means "Yes"    0 means "No" )

levels(mergeData$PaperlessBilling) <- c(0,1)
mergeData$PaperlessBilling <- as.numeric(levels(mergeData$PaperlessBilling))[mergeData$PaperlessBilling]

# Churn to numeric ( 1 means "Yes"    0 means "No" )

levels(mergeData$Churn) <- c(0,1)
mergeData$Churn <- as.numeric(levels(mergeData$Churn))[mergeData$Churn]


str(mergeData)

# Converting remaining Factor type variables (Contract,PaymentMethod,MultipleLines,InternetService,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies) to dummy variables except CustomerID

dummy_Contract <- as.data.frame(model.matrix(~Contract,data = mergeData ))
dummy_Contract <- dummy_Contract[,-1]

dummy_PaymentMethod <- as.data.frame(model.matrix(~PaymentMethod,data = mergeData ))
dummy_PaymentMethod <- dummy_PaymentMethod[,-1]

dummy_MultipleLines <- as.data.frame(model.matrix(~MultipleLines,data = mergeData ))
dummy_MultipleLines <- dummy_MultipleLines[,-1]

dummy_InternetService <- as.data.frame(model.matrix(~InternetService,data = mergeData ))
dummy_InternetService <- dummy_InternetService[,-1]

dummy_OnlineSecurity <- as.data.frame(model.matrix(~OnlineSecurity,data = mergeData ))
dummy_OnlineSecurity <- dummy_OnlineSecurity[,-1]

dummy_OnlineBackup <- as.data.frame(model.matrix(~OnlineBackup,data = mergeData ))
dummy_OnlineBackup <- dummy_OnlineBackup[,-1]

dummy_DeviceProtection <- as.data.frame(model.matrix(~DeviceProtection,data = mergeData ))
dummy_DeviceProtection <- dummy_DeviceProtection[,-1]

dummy_TechSupport <- as.data.frame(model.matrix(~TechSupport,data = mergeData ))
dummy_TechSupport <- dummy_TechSupport[,-1]

dummy_StreamingTV <- as.data.frame(model.matrix(~StreamingTV,data = mergeData ))
dummy_StreamingTV <- dummy_StreamingTV[,-1]

dummy_StreamingMovies <- as.data.frame(model.matrix(~StreamingMovies,data = mergeData ))
dummy_StreamingMovies <- dummy_StreamingMovies[,-1]

# Converting integer type fields to numeric types

mergeData$SeniorCitizen <- as.numeric(mergeData$SeniorCitizen)
mergeData$tenure <- as.numeric(mergeData$tenure)

# Subsetting the numeric fields from the mergeData

mergeData_num <- mergeData[,c(2:7,11:13)]


# merging with the numeric dataset with dummy variable files

final_data <- cbind(mergeData_num,dummy_Contract,dummy_PaymentMethod,dummy_MultipleLines,dummy_InternetService,dummy_OnlineSecurity,dummy_OnlineBackup,dummy_DeviceProtection,dummy_TechSupport,dummy_StreamingTV,dummy_StreamingMovies)

# Normalising the Numerical Fields

final_data$MonthlyCharges <- scale(final_data$MonthlyCharges,center = T,scale = T)

final_data$TotalCharges <- scale(final_data$TotalCharges,center = T,scale = T)

final_data$tenure <- scale(final_data$tenure,center = T,scale = T)


# K-NN Model:

#==============================================================
# Model - K-NN:
#==============================================================

# Loading the Library required for this type of modeling

library(class)
library(caret)
library(ROCR)

# Creating Training and Testing Data

# Splitting into training and testing
set.seed(2)
s=sample(1:nrow(final_data),0.7*nrow(final_data))

# training data contains 70% of the data
data_train=final_data[s,]

#testing data contains 30% of the data
data_test=final_data[-s,]

# True class labels of training data
cl <- data_train[, 9]

#Training and testing data without the true labels
data_train <- data_train[,-9]
data_test1 <- data_test[, -9]

#KNN with 1NN

impknn1 <- knn(data_train,data_test1, cl, k = 1,prob = TRUE)
table(impknn1,data_test[,9])
confusionMatrix(impknn1, data_test[,9],positive = "1")

# Accuracy : 0.7317  Sensitivity : 0.4991  Specificity : 0.8153

#KNN - 3 Nearest neighbours

impknn3 <- knn(data_train,data_test1, cl, k = 3,prob = TRUE)
table(impknn3,data_test[,9])
confusionMatrix(impknn3, data_test[,9], positive ="1")

# Accuracy : 0.7496  Sensitivity : 0.4794  Specificity : 0.8468

#Using the train() command to find the best K.
model <- train(
  as.factor(Churn)~., 
  data=final_data,
  method='knn',
  tuneGrid=expand.grid(.k=1:50),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))

#Generating the plot of the model
model
plot(model)

# From Plot K = 9 shows the highest Average Accuracy

#KNN - 9 Nearest neighbours

impknn9 <- knn(data_train,data_test1, cl, k = 9,prob = TRUE)
table(impknn3,data_test[,9])
confusionMatrix(impknn9, data_test[,9], positive ="1")

# Accuracy : 0.7714  Sensitivity : 0.5206  Specificity : 0.8616


#calculating the values for ROC curve

# default probabilities coming from knn model are of winning class(of 0 or of 1 ), unlike most other models which give probability of 1.

# So, solution to get correct ROC and AUC is to first convert default prob to probability of 1.

attr(impknn9,"prob") <- ifelse(impknn9==1,attr(impknn9,"prob"),1 - attr(impknn9,"prob"))

pred <- prediction(attr(impknn9,"prob"), data_test[,"Churn"])
perf <- performance(pred,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

# calculating AUC
auc <- performance(pred,"auc")   # AUC = 0.7979


# Naive Bayes Model:

#==============================================================
# Model - Naive Bayes:
#==============================================================

# Data Preparation For naive Bayes method

# merging 3 data frame to 1 merge file based on Common Field CustomerID

mergeData_NB <- merge(customer,churn,by.x = "customerID", all = T)

mergeData_NB <- merge(mergeData_NB,internet,by.x = "customerID", all = T)

# Remove Duplicate Data from the merge dataset

mergeData_NB <- unique(mergeData_NB)

# Check for missing Values

sum(is.na(mergeData_NB))   # 11 missing value present

# to find the presense of missing value in various variable

sapply(mergeData_NB,function(x) sum(is.na(x)))  # all missing value is in TotalCharges field

mergeData_NB$TotalCharges[which(is.na(mergeData_NB$TotalCharges))] <- mean(mergeData_NB$TotalCharges,na.rm = T)

# To check if missing values has been imputed or not

sum(is.na(mergeData_NB))   # No missing value present now

# Check for outlier

boxplot.stats(mergeData_NB$tenure)          # No outlier present in Tenure field
boxplot.stats(mergeData_NB$MonthlyCharges)  # No outlier present in monthly Charges field
boxplot.stats(mergeData_NB$TotalCharges)    # No outlier present in Total Charges field

# Converting Numericals variables to Categorical via binning

# Variable: tenure

max(mergeData_NB$tenure) # 72
min(mergeData_NB$tenure) #  0

# Creaing 5 Bin for Tenure variable

mergeData_NB$tenure_bins<-cut(mergeData_NB$tenure, seq(0,75,15), right=FALSE, labels=c(1:5))


# Variable: MonthlyCharges

max(mergeData_NB$MonthlyCharges) # 118.75
min(mergeData_NB$MonthlyCharges) #  18.25

# Creaing 6 Bin for Tenure variable

mergeData_NB$MonthlyCharges_bins<-cut(mergeData_NB$MonthlyCharges, seq(0,120,20), right=FALSE, labels=c(1:6))


# Variable: TotalCharges

max(mergeData_NB$TotalCharges) # 8684.8
min(mergeData_NB$TotalCharges) #  18.8

# Creaing 9 Bin for Tenure variable

mergeData_NB$TotalCharges_bins<-cut(mergeData_NB$TotalCharges, seq(0,9000,1000), right=FALSE, labels=c(1:9))

# removing non required variables from the dataframe (customerID,tenure,MonthlyCharges,TotalCharges)

mergeData_NB <- mergeData_NB[,-c(1,6,11,12)]

# Converting "SeniorCitizen" variable to factor

mergeData_NB$SeniorCitizen <- as.factor(mergeData_NB$SeniorCitizen)

# Checking the Datatypes of the final data frame

str(mergeData_NB)   # All fields including target variable is factor type now so Naive Bayes Can be applied on the data frame


#forming training, testing
set.seed(2)
s=sample(1:nrow(mergeData_NB),0.7*nrow(mergeData_NB))
mergeData_NB_train=mergeData_NB[s,]
mergeData_NB_test=mergeData_NB[-s,]


# Now we will run Naive Bayes algorithm on this data: Using the e1071 package
model_NB <- naiveBayes(Churn ~. , data = mergeData_NB_train)


pred <- predict(model_NB, mergeData_NB_test)
table(pred, mergeData_NB_test$Churn)

confusionMatrix(pred, mergeData_NB_test$Churn,positive = "Yes")

# Accuracy : 0.7123  Sensitivity : 0.7907  Specificity : 0.6840 

predvec <- ifelse(pred=="Yes", 1, 0)
realvec <- ifelse(mergeData_NB_test$Churn=="Yes", 1, 0)

pr <- prediction(predvec, realvec)
prf <- performance(pr, "tpr", "fpr")
plot(prf)
auc<-performance(pr,"auc")@y.values[[1]]   # AUC = 0.7373


# Logistic Regression:

#==============================================================
# Model - MLR:
#==============================================================

# Model with all variables
initial_model = glm(Churn ~ ., data = train.data, family = "binomial")
summary(initial_model)

# Stepwise selection of variables
best_model = step(initial_model,direction = "both")
summary(best_model)

#VIF to remove multi-collinearity
library(car)
vif(best_model) 

# All variables which is having VIF value greater than 2 is significant so based on VIF value we can't remove any variable.

# elimniate "Dependents" based on P value

model_1 <- glm(formula = Churn ~ SeniorCitizen +  tenure + MonthlyCharges + 
                 TotalCharges + `ContractOne year` + `ContractTwo year` + 
                 `PaymentMethodElectronic check` + MultipleLinesYes + `InternetServiceFiber optic` + 
                 InternetServiceNo + OnlineSecurityYes + DeviceProtectionYes + 
                 TechSupportYes + StreamingTVYes + StreamingMoviesYes, family = "binomial", 
               data = train.data)

summary(model_1)

# elimniate "OnlineSecurityYes" based on P value

model_2 <- glm(formula = Churn ~ SeniorCitizen +  tenure + MonthlyCharges + 
                 TotalCharges + `ContractOne year` + `ContractTwo year` + 
                 `PaymentMethodElectronic check` + MultipleLinesYes + `InternetServiceFiber optic` + 
                 InternetServiceNo + DeviceProtectionYes + 
                 TechSupportYes + StreamingTVYes + StreamingMoviesYes, family = "binomial", 
               data = train.data)

summary(model_2)

# elimniate "OnlineSecurityYes" based on P value

model_3 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
                 TotalCharges + `ContractOne year` + `ContractTwo year` + 
                 `PaymentMethodElectronic check` + MultipleLinesYes + `InternetServiceFiber optic` + 
                 InternetServiceNo + DeviceProtectionYes + 
                 TechSupportYes + StreamingTVYes + StreamingMoviesYes, family = "binomial", 
               data = train.data)

summary(model_3)

# elimniate "OnlineSecurityYes" based on P value

model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
                 TotalCharges + `ContractOne year` + `ContractTwo year` + 
                 `PaymentMethodElectronic check` + MultipleLinesYes + `InternetServiceFiber optic` + 
                 InternetServiceNo + 
                 TechSupportYes + StreamingTVYes + StreamingMoviesYes, family = "binomial", 
               data = train.data)

summary(model_4)

# elimniate "TechSupportYes" based on P value

model_5 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
                 TotalCharges + `ContractOne year` + `ContractTwo year` + 
                 `PaymentMethodElectronic check` + MultipleLinesYes + `InternetServiceFiber optic` + 
                 InternetServiceNo 
               + StreamingTVYes + StreamingMoviesYes, family = "binomial", 
               data = train.data)

summary(model_5)

# All variables are significant now so this can be treated as final model.

# model_5 is our final model
model_final = model_5


## C-statistic
library(Hmisc)

train.data$predicted_prob = predict(model_final,  type = "response")
rcorr.cens(train.data$predicted_prob,train.data$Churn) # 1st argument is your vector of predicted probabilities, 2nd observed values of outcome variable

# C-statistic value for Training Data is 0.84

test.data$predicted_prob = predict(model_final, newdata = test.data,type = "response")
rcorr.cens(test.data$predicted_prob,test.data$Churn)

# C-statistic value for Test Data is 0.85


#KS-statistic

library(ROCR)

model_score <- prediction(train.data$predicted_prob,train.data$Churn)

model_perf <- performance(model_score, "tpr", "fpr")

ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)   # KS Statistics for Training data is 0.52

which(ks_table == ks)  #2048

decile_train <- 2048/4930  # 0.415

# Decile Number of Training Data: 5th Decile


model_score_test <- prediction(test.data$predicted_prob,test.data$Churn)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks_test = max(ks_table_test)   # KS Statistics for Testing data is 0.55

which(ks_table_test == ks_test) # 778

decile_test <- 778/2113  # 0.36

# Decile Number of Test Data: 4th Decile 

# ROC Curve and AUC Calculation

plot(model_perf,col = "red", lab = c(10,10,7))


# calculating AUC

auc <- performance(model_score_test,"auc")   # 0.8565




# SVM:

#==============================================================
# Model - SVM:
#==============================================================

# SVM Model Specific Data Preparation

# convert target variable to factor
final_data$Churn = factor(final_data$Churn)

# generate train and test data 
set.seed(1)
train.indices = sample(1:nrow(final_data), 0.7*nrow(final_data))

train.data = final_data[train.indices, ]
test.data = final_data[-train.indices, ]

# model 0 with cost = 0.1
model.svm.0 = svm(Churn ~., data = train.data, kernel = "linear", cost = 0.1, scale = F,probability=TRUE)  
summary(model.svm.0)


# model 1 with cost = 100
model.svm.1 = svm(Churn ~., data = train.data, kernel = "linear", cost = 100, scale = F,probability=TRUE)  
summary(model.svm.1)


# finding the optimal value of cost using cross-validation
# svm cross-validation is done using the tune function
# result: higher costs yield lower error rates

tune.svm = tune(svm, Churn ~., data = train.data, kernel = "linear", ranges = list(cost = c( 0.1,  1, 100)))
summary(tune.svm)

# the tune function also stores the best model obtained
# cost = 0.1 is the best model

best.mod <- model.svm.0
best.mod

# predicting test classes using the best model and analyzing the table
# best.model is the one with cost = 0.1

test.data$ypred = predict(best.mod, test.data)
table(predicted = ypred, truth = test.data$Churn)
confusionMatrix(ypred, test.data$Churn,positive = "1")

# Accuracy : 0.8135  Sensitivity : 0.5605  Specificity : 0.8970 


#predicting the test data
svmmodel.predict<-predict(best.mod,test.data[,-9],probability = T)
svmmodel.probs<-attr(svmmodel.predict,"probabilities")[,2]
svmmodel.class<-predict(model.svm.0,train.data,type="class")
svmmodel.labels<-test.data$Churn


#roc analysis for test data
svmmodel.prediction<-prediction(svmmodel.probs,svmmodel.labels)
svmmodel.performance<-performance(svmmodel.prediction,"tpr","fpr")
svmmodel.auc<-performance(svmmodel.prediction,"auc")@y.values[[1]]  # AUC = 0.8544

plot(svmmodel.performance,  main="ROC curves")
#==============================================================






