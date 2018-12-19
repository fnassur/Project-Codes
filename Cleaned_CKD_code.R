#Load the libraries/packages

library(tidyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(mice)
library(class)
library(e1)
library(gbm)
library(rattle)
library(rpart)
library(pROC)



## Step 1  - Read in Data/Understand structure
data=read.csv("LogisticRegressionData.csv")
names(data)
class(data)
summary(data)
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status


#predicting missing data #ignore
predict_weight = which(is.na(data$Weight)==1)
predict_height = which(is.na(data$Height)==1)
predict_waist = which(is.na(data$Waist)==1)
predict_SBP = which(is.na(data$SBP)==1)
predict_DBP = which(is.na(data$DBP)==1)
predict_HDL = which(is.na(data$HDL)==1)
predict_LDL = which(is.na(data$LDL)==1)
predict_total = combine(predict_weight,predict_height,predict_waist,predict_SBP,predict_DBP,predict_HDL,predict_LDL)
predict_total = unique(predict_total)
#predict_data = data[-predict_total,]



#remove categorical variables
predict_data = data[,c(2,10,11,14,15,16,17,18)]
names(predict_data)


#####################################
# Predictive mean matching 
#pmm stands for predictive mean matching, default method of mice() for imputation of continous incomplete variables;
#for each missing value, pmm finds a set of observed values with the closest predicted mean as the missing one 
#and imputes the missing values by a random draw from that set. 
#Therefore, pmm is restricted to the observed values, and might do fine even for categorical data (though not recommended).
#install mice
#install.packages("mice")


md.pattern(predict_data)
impute_data <- mice(predict_data,m=5, method = "pmm",maxit = 25,seed = 500)
completedata <- complete(impute_data,5)
data$Age <- completedata$Age
data$Weight <- completedata$Weight
data$Height <- completedata$Height
data$BMI <- data$Weight/(data$Height/100)^2  #BMI formula
data$Obese <- ifelse(data$BMI>29,1,0)
data$Waist <- completedata$Waist
data$SBP <- completedata$SBP
data$DBP <- completedata$DBP
data$HDL <- completedata$HDL
data$LDL <- completedata$LDL
data$Total.Chol <- data$LDL + data$HDL


## Function to fill missing data
fill_missing <- function(col_name, comp_df, data_df){
  # browser()
  comp_df[,col_name] <- data_df[,col_name]
  missing <- which(is.na(comp_df[,col_name])==1)
  train_df <- comp_df[-missing,]
  test_df <- comp_df[missing,]
  train_df[,col_name] <- factor(train_df[,col_name])
  # train_col <- train_df[,col_name]
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  knn_fit <- train(train_df[,-which(names(train_df)%in%col_name)],train_df[,col_name], method = "knn",
                   trControl=trctrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10)
  pred_df <- predict(knn_fit, newdata = test_df)
  pred_df #both 0
  comp_df[missing,col_name] <- pred_df
  return(comp_df)  
  
}

## Fill In Categorical Missing Data
fill_names <- c("Diabetes","Anemia","Activity","Stroke","Educ","CVD","CHF","Hypertension","Insured","Fam.CVD","Unmarried","PoorVision","Income")
for (i in 1:length(fill_names)) {
  completedata <- fill_missing(fill_names[i],completedata, data)
  print(paste0("Done with ",fill_names[i]))
}


#Add on remaining columns to completedata
completedata$Racegrp <- data$Racegrp
completedata$CareSource <- data$CareSource
completedata$Female <- data$Female
completedata$BMI <- data$BMI
completedata$Obese <- data$Obese
completedata$Total.Chol <- data$Total.Chol
completedata$Fam.Diabetes <- data$Fam.Diabetes
data_new=model.matrix(~-1+Racegrp+CareSource,data=completedata)
completedata = completedata[,-22] #removed racegrp
completedata = completedata[,-22] #removed caresource
completedata = cbind(completedata,data_new) #combine race and caresource as binary variables


#splitting data into test and train   #already done since we have 2819 NA -> testing set

completedata_ckd <- cbind(completedata,data$CKD)
data_train <- completedata_ckd[-which(is.na(completedata_ckd$`data$CKD`)==1),]
data_test <-  completedata_ckd[which(is.na(completedata_ckd$`data$CKD`)==1),]

data_train$`data$CKD` <- factor(data_train$`data$CKD`)
colnames(data_train)[35] <- "CKD"
colnames(data_test)[35] <- "CKD"

#Decision tree
colnames(completedata)
model <- rpart(CKD~.,data = data_train)
model

summary(model) #identify relevant variables and try to use it in decision tree
plot(model,margin=0.001)
text(model,use.n = TRUE,pretty = TRUE,cex = 0.8)
fancyRpartPlot(model)
#predict
pred_ckd <- predict(model,newdata = data_train, type = "prob")
summary(pred_ckd)
auc(data_train$CKD, pred_ckd[,2]) #0.77 auc not good enough
table(Predicted = pred_ckd[,2]>0.136,Actualvalue = data_train$CKD) #set CKD probability threshold to 0.136

#Gradient boosted machine    #Not the best model however we get valuable insight to variables that give us relative importance
#can this be used as screening tool?!

fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 7)
fit <- train(CKD~., data = data_train, method = "gbm",trControl = fitcontrol, verbose = FALSE )
summary(fit)
print(fit)
predictions_train <- predict(fit, newdata = data_train, type="prob")
summary(predictions_train)


predictions_test <- predict(fit, newdata = data_test, type = "prob")
summary(predictions_test)

get_predict <- ifelse(predictions_train$`1`>0.1363801,1,0)
table(Actualvalue = data_train$CKD, predictedvalue = get_predict)

# Plot ROC curve and identify cutoff/threshold
rocrpred_gbm <- prediction(predictions_train$`1`,data_train$CKD)
rocrperf_gbm <- performance(rocrpred_gbm,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate
pred.acc_gbm <- performance(rocrpred_gbm,"acc")
#pred.acc_gbm
plot(rocrperf_gbm, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
#plot(rocrperf_gbm, add = TRUE)
abline(v=0.115)

cutoff.list.acc <- unlist(pred.acc_gbm @ x.values[[1]])
optimal.cutoff.acc<-cutoff.list.acc[which.max(pred.acc_gbm @ y.values[[1]])]
optimal.cutoff.acc  #gives your optimal cut off value?
cutoffs_gbm <- data.frame(cut=rocrperf_gbm@alpha.values[[1]], fpr=rocrperf_gbm@x.values[[1]], 
                          tpr=rocrperf_gbm@y.values[[1]])

head(cutoffs_gbm)
cutoffs_gbm <- cutoffs_gbm[order(cutoffs_gbm$tpr, decreasing=TRUE),]
head(subset(cutoffs_gbm, fpr < 0.19))

##Assess models   COMPARING GRADIENT BOOSTING vs LOGISTIC REG (0.8985 vs 0.9007) #pretty close 
#I believe gradient boosting provides us with a better insight however, logistic regression is a better predictor numerically

auc_gbm <- performance(rocrpred_gbm, measure = "auc")
auc_gbm <- auc_gbm@y.values[[1]]
auc_gbm

auc(data_train$CKD,predictions_train$`1`) #AUC = 0.8963

#logistic regression using backward elimination

res_train <- glm(CKD~.,family = "binomial", data=data_train)
summary(res_train)

model3=step(res_train,direction="backward")
summary(model3)

glm.fitted <- fitted(model3)
glm.fitted

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(model3))


res_test_ckd <- predict(res_train,data_test,type = "response") #with all variables
summary(res_test_ckd)

res_test_ckd <- as.data.frame(res_test_ckd)
res_test_ckd$GBM <- predictions_test$`1`

mod_predict_train <- predict(model3,newdata = data_train)

table(Actualvalue = data_train$CKD, predictedvalue = mod_predict_train>0.5)

rocrpred <- prediction(mod_predict_train,data_train$CKD)
rocrperf <- performance(rocrpred,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate

pred.acc <- performance(rocrpred,"acc")
pred.acc

max(rocrperf@y.values[[1]])
cutoff.list.acc <- unlist(pred.acc @ x.values[[1]])
optimal.cutoff.acc<-cutoff.list.acc[which.max(pred.acc @ y.values[[1]])]
optimal.cutoff.acc  #gives your optimal cut off value
cutoffs <- data.frame(cut=rocrperf@alpha.values[[1]], fpr=rocrperf@x.values[[1]], 
                      tpr=rocrperf@y.values[[1]])

head(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
auc(model3)
table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.119) #final answer .119 

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.2)

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.07)


auc_glm <- performance(rocrpred, measure = "auc")
auc_glm <- auc_glm@y.values[[1]]
auc_glm #0.899 
auc_gbm #0.896

#slightly better auc in logistic regression

# Output
mod_predict_train <- predict(model3,data_train, type = "response") #after elimating using backward
summary(mod_predict_train)

mod_predict_test <- predict(model3, data_test, type = "response") #probabilities of testing set
summary(mod_predict_test)

mod_predict_test <- as.data.frame(mod_predict_test)
mod_predict_test$value <- ifelse(mod_predict_test[,1]>.119,1,0)

colnames(mod_predict_test) <- c("Predicted probabilities","Model outcome")
write.csv(mod_predict_test,file = "Results.csv")
rm(mod_predict_test)

