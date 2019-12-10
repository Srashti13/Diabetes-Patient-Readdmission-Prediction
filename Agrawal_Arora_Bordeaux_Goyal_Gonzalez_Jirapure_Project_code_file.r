#OR 568 final project code:
#Author: Srashti, Pablo, Anshul, Aishwarya, Cristina, Saksham

#Library Used:
library(dplyr)
library(AppliedPredictiveModeling)
library(forcats)
library(plyr)
library(ggplot2)

#Load  the dataset 
Diabetes_Data <- read.csv(file = 'diabetic_data.csv')

#Removing the duplicate records:
Unique_Diabetes <- Diabetes_Data[!duplicated(Diabetes_Data$patient_nbr),]

No_Death <- Unique_Diabetes[!(Unique_Diabetes$discharge_disposition_id==11 
                               | Unique_Diabetes$discharge_disposition_id==12 
                               | Unique_Diabetes$discharge_disposition_id==13 
                               | Unique_Diabetes$discharge_disposition_id==14 
                               | Unique_Diabetes$discharge_disposition_id==19 
                               | Unique_Diabetes$discharge_disposition_id==20 
                               | Unique_Diabetes$discharge_disposition_id==21),]

#Removed the missing values:
Preprocess_Diabetes <- No_Death[!(No_Death$race == '?' | No_Death$diag_1 =='?' | No_Death$diag_2=='?' | No_Death$diag_3=='?' | No_Death$gender == 'Unkown/Invalid'),]


#Removed the unwanted columns and the colums with highest missing values for the prediction:
LessV_Data <- subset(Preprocess_Diabetes, select = -c(encounter_id, patient_nbr, payer_code, medical_specialty))

#Removed 11 categorical predictors because they had primarily (almost) only one factor level and would not have a significant influence on the response.
diabetes <- subset(LessV_Data, select = -c(examide, citoglipton,metformin.rosiglitazone, acetohexamide, tolbutamide, miglitol,
                                             troglitazone, tolazamide, glipizide.metformin, glimepiride.pioglitazone, metformin.pioglitazone))

#Converting age intervals
diabetes$age <- ifelse(diabetes$age == "[0-10)",  5, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[10-20)", 15, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[20-30)", 25, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[30-40)", 35, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[40-50)", 45, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[50-60)", 55, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[60-70)", 65, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[70-80)", 75, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[80-90)", 85, diabetes$age);
diabetes$age <- ifelse(diabetes$age == "[90-100)", 95, diabetes$age);

#checking the datatype of each column
#data.frame(sapply(diabetes,class))

#Converting into the factor datatype:
diabetes$admission_type_id <- as.factor(diabetes$admission_type_id)
diabetes$admission_source_id <- as.factor(diabetes$admission_source_id)
diabetes$discharge_disposition_id <- as.factor(diabetes$discharge_disposition_id)

#Giving two labels: Home and other for discharge disposition
levels(diabetes$discharge_disposition_id)
diabetes$discharge_disposition_id <- fct_collapse(diabetes$discharge_disposition_id, Home = c("1"), 
                                                  Other = c("2","3","4","5","6","7","8","9","10","15","16",
                                                            "17","18","22","23","24","25","26","27","28","29"))
levels(diabetes$discharge_disposition_id)

#dividing admission source into 3 labels: 
levels(diabetes$admission_source_id)
diabetes$admission_source_id <- fct_collapse(diabetes$admission_source_id, "Emergency Room" = c("7"), 
                                             "Transfer/Referral" = c("1","2","3","4","5","6","10","18","22","25","26"),
                                             Other = c("8","9","11","13","14","17","20"))
levels(diabetes$admission_source_id)

#Dividing the target variable into two set:
diabetes$readmitted <- fct_collapse(diabetes$readmitted, "Readmitted" = c(">30","<30"), "Not Readmitted" = c("NO"))
levels(diabetes$readmitted)

#Assigning the value "Not admitted" as 0 and "Admitted" as 1
library(plyr)
diabetes$readmitted <- revalue(diabetes$readmitted,
                             c("Readmitted"="1", "Not Readmitted"="0"))

#Converting into numeric datatype since we have to use different classification models that will support numeric datatype.
#diabetes$readmitted <- as.numeric(as.character(diabetes$readmitted))

#data.frame(sapply(diabetes,class))

### diagnosis columns
#mapping for the levels were found in Strack et al..6

`%notin%` <- Negate(`%in%`)
# Group and Recode Primary Diagnoses Result (diag_1)
levels(diabetes$diag_1)[levels(diabetes$diag_1) %notin% as.factor(c(390:459, 785, 460:519, 786, 520:579, 787, seq(250,250.99, 0.01), 800:999, 710:739, 580:629, 788, 140:239))] <- "Other"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(390:459, 785))] <- "Circulatory"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(460:519, 786))] <- "Respiratory"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(520:579, 787))] <- "Digestive"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(seq(250,250.99, 0.01)))] <- "Diabetes"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(800:999))] <- "Injury"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(710:739))] <- "Musculoskeletal"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(580:629, 788))] <- "Genitourinary"
levels(diabetes$diag_1)[levels(diabetes$diag_1) %in% as.factor(c(140:239))] <- "Neoplasms"
levels(diabetes$diag_1)

# Group and Recode Secondary Diagnoses Result (diag_2)
levels(diabetes$diag_2)[levels(diabetes$diag_2) %notin% as.factor(c(390:459, 785, 460:519, 786, 520:579, 787, seq(250,250.99, 0.01), 800:999, 710:739, 580:629, 788, 140:239))] <- "Other"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(390:459, 785))] <- "Circulatory"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(460:519, 786))] <- "Respiratory"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(520:579, 787))] <- "Digestive"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(seq(250,250.99, 0.01)))] <- "Diabetes"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(800:999))] <- "Injury"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(710:739))] <- "Musculoskeletal"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(580:629, 788))] <- "Genitourinary"
levels(diabetes$diag_2)[levels(diabetes$diag_2) %in% as.factor(c(140:239))] <- "Neoplasms"
levels(diabetes$diag_2)

# Group and Recode Secondary Additional Diagnoses Result (diag_3)
levels(diabetes$diag_3)[levels(diabetes$diag_3) %notin% as.factor(c(390:459, 785, 460:519, 786, 520:579, 787, seq(250,250.99, 0.01), 800:999, 710:739, 580:629, 788, 140:239))] <- "Other"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(390:459, 785))] <- "Circulatory"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(460:519, 786))] <- "Respiratory"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(520:579, 787))] <- "Digestive"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(seq(250,250.99, 0.01)))] <- "Diabetes"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(800:999))] <- "Injury"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(710:739))] <- "Musculoskeletal"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(580:629, 788))] <- "Genitourinary"
levels(diabetes$diag_3)[levels(diabetes$diag_3) %in% as.factor(c(140:239))] <- "Neoplasms"
levels(diabetes$diag_3)

#Removing the weight column
diabetes_no_weight <- subset(diabetes, select = -c(weight))

#data.frame(sapply(diabetes_no_weight,class))

#Splitting the data into train and test set:
train_index <- sample(1:nrow(diabetes_no_weight), 0.7 * nrow(diabetes_no_weight))
test_index <- setdiff(1:nrow(diabetes_no_weight), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- diabetes_no_weight[train_index, -34]
y_train <- diabetes_no_weight[train_index, "readmitted"]

#Combined 70% training data
train_df <- diabetes_no_weight[train_index,]

X_test <- diabetes_no_weight[test_index, -34]
y_test <- diabetes_no_weight[test_index, "readmitted"]

# 30 % Test set 
test_df <- diabetes_no_weight[test_index,]

###################################################################################################
#1: Generalized Logistic Regression

#GlM Model
GLM_Model <- glm(readmitted~., data=train_df, family=binomial(link='logit'))

#Checking Information
summary(GLM_Model)

#For prediction
GLM_Model_Prediction <- predict(GLM_Model,test_df, type = "response")
plot(GLM_Model)

# Accuracy of Model
GLM_Pred <- ifelse(GLM_Model_Prediction > 0.5, 1, 0)
pred2 <- ifelse(GLM_Pred == 1, "TRUE", "FALSE")
#Number True and Number False
table(pred2)["TRUE"]

#Accuracy Equation
GLM_Accuracy <- ifelse(GLM_Pred == test_df$readmitted,1,0)

#table for accuracy
table(GLM_Accuracy)

#Calculating True vs. total
Accuracy_GLM <- (sum(GLM_Accuracy)/count(test_df))*100
Accuracy_GLM

#confusion matrix
confusionMatrix(as.factor(GLM_Pred),as.factor(test_df$readmitted), positive = "1")

#ROC curve for GLM
library(ROCR)
pred_glm <- prediction(as.numeric(as.character(GLM_Model_Prediction)), as.numeric(as.character(test_df$readmitted)))
perf_glm <- performance(pred_glm, "tpr", 'fpr')
plot(perf_glm, main = "ROC curve", colorize = T)
abline(0,1, col='gray60')

summary(perf_glm)

auc_ROCR <- performance(pred_glm, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

##############################################################################################################
#2: Classification tree Model
library(pROC)
library(rpart)
library(ROCR)

control <- rpart.control(minbucket=10, cp = 0.0001,maxsurrogate = 0, usesurrogate = 0, xval=10)

dat <- rpart(readmitted~., train_df, method = "class", control = control)
plotcp(dat)
printcp(dat)
datpr <- prune(dat, cp=0.00036)
plotcp(datpr)
summary(datpr)

pred = predict(datpr, test_df, type = "class")

confusionMatrix(as.factor(test_df$readmitted), pred)

varImp(datpr)

pred_ct <- prediction(as.numeric(as.character(pred)), as.numeric(as.character(test_df$readmitted)))
perf_ct <- performance(pred_ct, "tpr", 'fpr')
plot(perf_ct, main = "ROC curve", colorize = T)
abline(0,1, col='gray60')

##############################################################################################################
#3. Support Vector machine

library(e1071)
library(caret)
library(ROCR)
library(gplots)
library(kernlab)

Set.seed(12)

SVM_Model3 <- svm(readmitted~., data = train_df, kernel = "linear",
                  type = "C-classification", cross = 10, cost = 0.01, gamma = 1000)

# SVM Predication 
SVM_Model_Prediction <- predict(SVM_Model3, test_df)

# Summary for SVM Model and Confusion Matrix 
summary(SVM_Model3)# Accuracy: 61%

## ROC Curve

SVM_ROC2 <-roc(response=test_df$readmitted, predictor= factor(SVM_Model_Prediction,ordered = TRUE), plot=TRUE) 

SVM_ROC2

#################################################################################################################
# Changing target variable datatype to factor for Random forest, Boosted tree and Neural network Models

diabetes_no_weight$readmitted <- as.factor(as.character(diabetes_no_weight$readmitted))

train_index <- sample(1:nrow(diabetes_no_weight), 0.7 * nrow(diabetes_no_weight))
test_index <- setdiff(1:nrow(diabetes_no_weight), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- diabetes_no_weight[train_index, -34]
y_train <- diabetes_no_weight[train_index, "readmitted"]

#Combined 70% training data
train_df <- diabetes_no_weight[train_index,]

X_test <- diabetes_no_weight[test_index, -34]
y_test <- diabetes_no_weight[test_index, "readmitted"]

# 30 % Test set 
test_df <- diabetes_no_weight[test_index,]
#####################################################################################################

#4. Random Forest Model 

library(randomForest)
library(caret)
library(mlbench)


rForest <- randomForest(readmitted~., data=train_df, mtry=3, ntree = 500, nodesize=5,
                        importance = TRUE )

#check the Model
rForest 

#For predicting on test data
rf_yHat = predict(rForest, X_test)
head(rf_yHat)

#for confusion Matrix
confusionMatrix(rf_yHat, y_test)

# performance evaluation Accuracy and Kappa
rfPR = postResample(pred=rf_yHat, obs=y_test)

#predict Vs Observed Random Forest
plot(rf_yHat, y_test, main="Observed Vs Predicted", xlab= "Predicted ", ylab= "Observed ",pch=19)

#plots error vs # trees
plot(rForest, main = "Random Forest Model") 

#checking Important variables
importance(rForest); varImpPlot(rForest, sort = TRUE, main = "Random Forest Model")

#Roc curve for Random Forest
library(ROCR)
pred <- prediction(as.numeric(as.character(rf_yHat)), as.numeric(as.character(y_test)))
perf <- performance(pred, "tpr", 'fpr')
plot(perf, main = "ROC curve", colorize = T)
abline(0,1, col='gray60')

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

################################################################################################################

#5.Boosted Trees Model

library(caret)
library(corrplot)			# plot correlations
library(doParallel)		# parallel processing
library(gbm)				  # GBM Models

train_df$readmitted <- revalue(train_df$readmitted, c("1"="Yes", "0"="No"))

ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(100,1000),	        # Num trees to fit
                    shrinkage=c(0.01,0.2),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)

gbmFit<- train(readmitted~., data = train_df,
               method = "gbm",
               tuneGrid = grid,
               metric = "ROC",
               verbose = FALSE,
               trControl = ctrl)


gbmPred <- predict(gbmFit,type = "raw")
head(gbmPred)

confusionMatrix(train_df$readmitted,gbmPred)

library(ROCR)
gbmPred <- predict(gbmFit, test_df)
pred <- prediction(as.numeric(gbmPred), as.numeric(y_test))
perf <- performance(pred, "tpr", 'fpr')
plot(perf, main = "ROC curve", colorize = T)
abline(0,1, col='gray60')

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
###############################################################################################################

#6.1 Neural Network

#install.packages("quantmod")
library(nnet)       
library(caret)
library(quantmod)
head(train_df)
str(train_df)

##################### NNET MODEL #################################
model <- train(readmitted ~ . , train_df, method='nnet', linout=TRUE, trace = FALSE,
               #Grid of tuning parameters to try:
               tuneGrid=NULL ) 
summary(model)
############### Without Cross Validation#######

pred <- predict(model, test_df)
predNew = as.data.frame(pred)
categorise=function(x){
  return(ifelse(x>0.5,1,0))
}
predNew=apply(predNew,2,categorise)
head(predNew,10)
confusionMatrix(as.factor(test_df$readmitted),as.factor(predNew))

############### WITH CROSS VALIDATION###########
train_control <- trainControl(method = "cv", number = 10)
linearCrossMod <- train(readmitted ~ ., 
                        data=train_df, 
                        trControl = train_control,
                        method = "nnet",
                        family=binomial())

summary(linearCrossMod)

pred_cv <- predict(linearCrossMod, test_df)
predNew_cv = as.data.frame(pred_cv)
predNew_cv=apply(predNew_cv,2,categorise)
head(predNew_cv,10)
confusionMatrix(as.factor(predNew_cv),as.factor(test_df$readmitted))

############### Roc curve for NNET MODEL################
library(ROCR)

pred_roc <- prediction(as.numeric(as.character(pred_cv)), as.numeric(as.character(test_df$readmitted)))
perf <- performance(pred_roc, "tpr", 'fpr')
plot(perf, main = "ROC curve for Neural Net", colorize = T)
abline(0,1, col='gray60')

auc_ROCR <- performance(pred_roc, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

auc_ROCR

########################## 
# 6.2 avNNet MODEL 

###################################
model_av <- train(readmitted ~ . , train_df, method='avNNet', linout=TRUE, trace = FALSE,
                  #Grid of tuning parameters to try:
                  tuneGrid=NULL ) 
summary(model_av)

################ Without Cross Validation

pred_av <- predict(model_av, test_df)
predNew_av = as.data.frame(pred_av)

predNew_av=apply(predNew_av,2,categorise)
head(predNew_av,10)
confusionMatrix(as.factor(test_df$readmitted),as.factor(predNew_av))

################ WITH CROSS VALIDATION###########################

linearCrossMod_av <- train(readmitted ~ ., 
                           data=train_df, 
                           trControl = train_control,
                           method = "avNNet",
                           family=binomial())

summary(linearCrossMod_av)

pred_cv_av <- predict(linearCrossMod_av, test_df)
predNew_cv_av = as.data.frame(pred_cv_av)
predNew_cv_av=apply(predNew_cv_av,2,categorise)
head(predNew_cv_av,10)
confusionMatrix(as.factor(predNew_cv_av),as.factor(test_df$readmitted))

################ Roc curve for Neural Network###################
library(ROCR)

pred_roc_av <- prediction(as.numeric(as.character(pred_cv_av)), as.numeric(as.character(test_df$readmitted)))
perf_av <- performance(pred_roc_av, "tpr", 'fpr')
plot(perf_av, main = "ROC curve for Neural Net", colorize = T)
abline(0,1, col='gray60')

auc_ROCR_av <- performance(pred_roc_av, measure = "auc")
auc_ROCR_av <- auc_ROCR_av@y.values[[1]]

auc_ROCR_av



################################################################################################################
#Visualizations:
################################################################################################################

#Number of observations in each readmitted category.
Number_patients <- table(Diabetes_Data$readmitted)
plot(Number_patients,col ="lightblue", xlab = " Readmission Days ", main= " Frequency of Readmission", lwd =20,pch=18)

#Number of observations in each readmitted category after dichotomized into two classes.
Number_patients <- table(diabetes_no_weight$readmitted)
plot(Number_patients,col ="lightblue", xlab = " Readmission Days ", main= " Frequency of Readmission", lwd =20,pch=18)

#Count of gender
plot(diabetes_no_weight$gender, main = "Gender Distribution", col = "Blue") 

#Age distribution
plot(Diabetes_Data$age, main = "Age Distribution", col = "Blue", xlab = " Age Range", ylab = " Counts ")

#Correlation plot
library("PerformanceAnalytics")
library(corrplot)
library(corrgram)
M <- corrgram(diabetes_no_weight)
corrplot(diabetes_no_weight, order = "hclust", addrect = 2)
##################################################################################################################

#Considering weight as a predictor as this is an important predictor for diabitic patients:

#diabetes$with_weight <- diabetes$weight
#diabetes[diabetes$weight == "?", "weight"] <- NA

#Removed the missing values:
Prep_Diabetes <- diabetes[!(diabetes$weight == '?'),]

#Splitting the data into train and test set:
train_index_w <- sample(1:nrow(Prep_Diabetes), 0.7 * nrow(Prep_Diabetes))
test_index_w <- setdiff(1:nrow(Prep_Diabetes), train_index_w)

# Build X_train, y_train, X_test, y_test
X_train_w <- Prep_Diabetes[train_index_w, -35]
y_train_w <- Prep_Diabetes[train_index_w, "readmitted"]

#Combined 70% training data
train_df_w <- Prep_Diabetes[train_index_w,]

X_test_w <- Prep_Diabetes[test_index_w, -35]
y_test_w <- Prep_Diabetes[test_index_w, "readmitted"]

# 30 % Test set 
test_df_w <- Prep_Diabetes[test_index_w,]


rForest_w <- randomForest(readmitted~., data=train_df_w, mtry=3, ntree = 500,
                          importance = TRUE )

rf_yHat_w = predict(rForest_w, X_test_w)

#for confusion Matrix
confusionMatrix(rf_yHat_w, y_test_w)

#ROC curve
library(ROCR)
pred_w <- prediction(as.numeric(as.character(rf_yHat_w)), as.numeric(as.character(y_test_w)))
perf <- performance(pred_w, "tpr", 'fpr')
plot(perf, main = "ROC curve", colorize = T)
abline(0,1, col='gray60')

auc_ROCR_w <- performance(pred_w, measure = "auc")
auc_ROCR_w <- auc_ROCR@y.values[[1]]
auc_ROCR_w

#The roc value doesn't vary much when we add weight predictor, hence we decided to remove it from our analysis.

