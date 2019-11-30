#OR 568 final project code:
#Author: Srashti, Pablo, Anshul, Aishwarya, Cristina, Saksham

#Library Used:
library(dplyr)
library(AppliedPredictiveModeling)
library(forcats)
library(dplyr)
library(ggplot2)

#Load  the dataset 
Diabetes_Data <- read.csv(file = 'diabetic_data.csv')

#Removing the duplicate records:
Unique_Diabetes <- Diabetes_Data[!duplicated(Diabetes_Data$patient_nbr),]

#Plot1:Histrigram for Number of Patients Hospitalizations:
# (Need to add border,Change the scale of X)
Diabetes %>%
  group_by(patient_nbr) %>%
  filter(n() > 2) %>%
  count() %>%
  ggplot(aes(x =n, group = 1)) + 
  geom_histogram(binwidth = 1, fill = 'mediumpurple3') + 
  labs(title = "Hospitalizations Patients with 3+ Visits", x = " Plot1:Number of  Hospitalizations", y = "Number of Patients") +
  geom_text(binwidth = 1, stat = "bin", aes(y = ..count.., label = ifelse(..count.. / sum(..count..) < 0.01, NA, scales::percent(..count.. / sum(..count..)))), 
            position = position_dodge(width = 1), vjust = -0.5, family = "DecimaMonoPro", size = 2)


No_Death <- Unique_Diabetes[!(Unique_Diabetes$discharge_disposition_id==11 
                               | Unique_Diabetes$discharge_disposition_id==12 
                               | Unique_Diabetes$discharge_disposition_id==13 
                               | Unique_Diabetes$discharge_disposition_id==14 
                               | Unique_Diabetes$discharge_disposition_id==19 
                               | Unique_Diabetes$discharge_disposition_id==20 
                               | Unique_Diabetes$discharge_disposition_id==21),]

#Removed the missing values:
Preprocess_Diabetes <- No_Death[!(No_Death$race == '?' | No_Death$diag_1 =='?' | No_Death$diag_2=='?' | No_Death$diag_3=='?' | No_Death$gender == 'Unkown/Invalid'),]


#Removed the unwanted columns for the prediction:
LessV_Data <- subset(Preprocess_Diabetes, select = -c(encounter_id, patient_nbr, payer_code, medical_specialty))
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
diabetes$readmitted <- as.numeric(as.character(diabetes$readmitted))

data.frame(sapply(diabetes,class))

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




















#Data Summaries((Visualization)
#Plot2:Age Distribution 
plot(Diabetes$age, main = " Plot 2:Age Distribution") 

#Plot3:  Gender Distribution
plot(Diabetes$gender, main = "Plot 3:Gender Distribution") 

# Plot4: Readmission 
ggplot(Diabetes,aes(x=number_inpatient,fill=readmitted)) + geom_bar()






