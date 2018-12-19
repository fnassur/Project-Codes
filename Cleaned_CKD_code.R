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
#6000 rows training set
#id <- sample(2,nrow(completedata),prob = c(0.7,0.3), replace = TRUE)
#data_train <- completedata[id==1,]
#data_test <- completedata[id==2,]
#data_train$CKD <- data[id==1,"CKD"]
#data_test$CKD <- data[id==2,"CKD"]
#data_test <- data_test[,-34]

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

rocrpred_gbm <- prediction(predictions_train$`1`,data_train$CKD)
rocrperf_gbm <- performance(rocrpred_gbm,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate

pred.acc_gbm <- performance(rocrpred_gbm,"acc")
#pred.acc_gbm

plot(rocrperf_gbm, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
#plot(rocrperf_gbm, add = TRUE)
abline(v=0.115)

max(rocrperf_gbm@y.values[[1]])

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

auc(data_train$CKD,predictions_train$`1`)
auc(data_train$CKD, mod_predict_train)


#auc2 <- performance(rocrpred, measure = "auc")
#auc2 <- auc2@y.values[[1]]
#auc2

#trellis.par.set(caretTheme())
#plot(fit, scales = list(x = list(log = 2)))

##########################
# RANDOM STUFF
mod_fit <- gbm(CKD ~. ,  data=data_train,distribution = "gaussian",n.trees = 10000,shrinkage = 0.01,interaction.depth = 4)

summary(mod_fit)

# BAGGING
library(ipred)
library(caret)
library(rpart)
bag_fit <- bagging(CKD~.,data = data_train, control=rpart.control(minsplit=1,xval = 25))

summary(bag_fit)

bag_trainpred <- predict(bag_fit,data_train)
summary(bag_trainpred)

#Random forest
library(randomForest)
id <- sample(2,nrow(data_train),prob = c(0.7,0.3), replace = TRUE)
forest_train <- data_train[id==1,]
forest_test <- data_train[id==2,]


forest_mod <- randomForest(CKD~.,data = forest_train)
summary(forest_mod)

forest_trainpred <- predict(forest_mod,forest_test[,1:33])
summary(forest_trainpred)

#K means on CKD patients
library(flexclust)
ckdppl <- data_train[which(data_train$CKD==1),]
ckdppl <- ckdppl[,c("Age","BMI","LDL","HDL","Diabetes","CVD","Female","SBP","DBP")]
ckdcluster <- kmeans(ckdppl,9)
ckdcluster$centers
ckdcluster

set.seed(123)
tss<-rep(0,10)
for (k in 1:10){tss[k]=kmeans(ckdppl,k)$tot.withinss}
plot(1:10,tss)


##################################################
#Clustering to identify similar people #IGNORE
library(flexclust)

tss<-rep(0,20)
for (k in 1:20){tss[k]=kmeans(completedata,k)$tot.withinss}
plot(1:20,tss)

cl = kmeans(as.matrix(completedata),5)
plot(as.matrix(completedata), col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)  


text(data,rownames(data),col=cl$cluster)
#################
#point system


point_system <- lm(formula = CKD ~ Age + Diabetes + Anemia + CVD + Hypertension, data = data_train)
trial <- data.frame( Age =70,Diabetes = 1,Anemia = 1,CVD = 1,Hypertension = 1)

predict(newdata = trial,point_system)
summary(point_system)

rocrpred <- prediction(pointpredict,data_train$CKD)
rocrperf_point <- performance(rocrpred,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate



plot(rocrperf_point, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))

pointdata <- data_train
pointdata$agescore <- ifelse(pointdata$Age>70,4,ifelse(pointdata$Age>60,3,ifelse(pointdata$Age>50,2,0)))
pointdata$CVDpoint <- ifelse(pointdata$CVD + pointdata$Fam.CVD==2,1,ifelse(pointdata$CVD+pointdata$Fam.CVD==1,0.5,0))
pointdata$diabetespoint <- ifelse(pointdata$Diabetes + pointdata$Fam.Diabetes ==2,1,ifelse(pointdata$Diabetes+pointdata$Fam.Diabetes==1,0.5,0))

pointdata$total <- rowSums(pointdata[,c("agescore","Anemia","Hypertension","Diabetes","CVDpoint")])

write.csv(pointdata,"pointscore.csv")
############## TRIAL #######

trialmod <- data_train
trialmod$agescore <- 0.04*trialmod$Age
trialmod$dbscore <- 0.45*trialmod$Diabetes
trialmod$ascore <- 1.12*trialmod$Anemia
trialmod$cvdscore <- 1.27*trialmod$CVD
trialmod$hscore <- .27*trialmod$Hypertension
trialmod$total <- rowSums(trialmod[,c("agescore","dbscore","ascore","cvdscore","hscore")])

write.csv(trialmod,"trialcore.csv")

########### TRIAL TESTING LOL   #########

trialmod <- data_test
#trialmod$agescore <- 0.04*trialmod$Age
trialmod$agescore <- ifelse(trialmod$Age>70,4,ifelse(trialmod$Age>60,3,ifelse(trialmod$Age>50,3.5,3)))
trialmod$dbscore <- 0.45*trialmod$Diabetes
trialmod$ascore <- 1.12*trialmod$Anemia
trialmod$cvdscore <- 1.27*trialmod$CVD
trialmod$hscore <- .27*trialmod$Hypertension
trialmod$total <- rowSums(trialmod[,c("agescore","dbscore","ascore","cvdscore","hscore")])
#write.csv(trialmod,"trialcore.csv")

trialmod$final <- ifelse(trialmod$total>=3.65,1,0)

########### Linear categorical  #######
trial2mod <- data_train
trial2mod$age50 <- ifelse(trial2mod$Age<60&trial2mod$Age>49,1,0)
trial2mod$age60 <- ifelse(trial2mod$Age<70&trial2mod$Age>59,1,0)
trial2mod$age70 <- ifelse(trial2mod$Age>=70,1,0)
trial2model <- glm(CKD~age50+age60+age70+Diabetes+Anemia+CVD+Hypertension,data = trial2mod,family = "binomial")
summary(trial2model)
trial2coeff <- coefficients(trial2model)/log(2) 
trial2coeff

trial2mod$agepoint <- ifelse(trial2mod$age50==1,2,ifelse(trial2mod$age60==1,3,ifelse(trial2mod$age70==1,5,0)))
trial2mod$dpoint <- ifelse(trial2mod$Diabetes==1,0.5,0)
trial2mod$apoint <- ifelse(trial2mod$Anemia==1,2,0)
trial2mod$cvdpoint <- ifelse(trial2mod$CVD==1,1,0)
trial2mod$hpoint <- ifelse(trial2mod$Hypertension==1,1,0)
trial2mod$totalscore <- rowSums(trial2mod[,c("agepoint","dpoint","apoint","cvdpoint","hpoint")])

summary(trial2predict)
write.csv(trial2mod$totalscore,file = "trial2.csv")
################ test data  ##################
trial2mod <- data_test
trial2mod$age50 <- ifelse(trial2mod$Age<60&trial2mod$Age>49,1,0)
trial2mod$age60 <- ifelse(trial2mod$Age<70&trial2mod$Age>59,1,0)
trial2mod$age70 <- ifelse(trial2mod$Age>=70,1,0)
#trial2model <- glm(age50+age60+age70+Diabetes+Anemia+CVD+Hypertension,data = trial2mod,family = "binomial")
#summary(trial2model)
#trial2coeff <- coefficients(trial2model)/log(2) 
#trial2coeff

trial2mod$agepoint <- ifelse(trial2mod$age50==1,2,ifelse(trial2mod$age60==1,3,ifelse(trial2mod$age70==1,5,0)))
trial2mod$dpoint <- ifelse(trial2mod$Diabetes==1,0.5,0)
trial2mod$apoint <- ifelse(trial2mod$Anemia==1,2,0)
trial2mod$cvdpoint <- ifelse(trial2mod$CVD==1,1,0)
trial2mod$hpoint <- ifelse(trial2mod$Hypertension==1,1,0)
trial2mod$totalscore <- rowSums(trial2mod[,c("agepoint","dpoint","apoint","cvdpoint","hpoint")])


###### Linear TEST DATA  #######
trial2mod <- data_test
trial2mod$age50 <- ifelse(trial2mod$Age<60&trial2mod$Age>49,1,0)
trial2mod$age60 <- ifelse(trial2mod$Age<70&trial2mod$Age>59,1,0)
trial2mod$age70 <- ifelse(trial2mod$Age>=70,1,0)
#trial2model <- glm(CKD~age50+age60+age70+Diabetes+Anemia+CVD+Hypertension,data = trial2mod,family = "binomial")
summary(trial2model)
trial2predict <- predict(newdata = trial2mod,trial2model) 

score_value <- ifelse(trial2predict>=-2.3,1,0)

####################
## For TEST DATA
pointdata <- data_test
pointdata$agescore <- ifelse(pointdata$Age>70,4,ifelse(pointdata$Age>60,3,ifelse(pointdata$Age>50,2,1)))
pointdata$CVDpoint <- ifelse(pointdata$CVD + pointdata$Fam.CVD==2,1,ifelse(pointdata$CVD+pointdata$Fam.CVD==1,0.5,0))
pointdata$diabetespoint <- ifelse(pointdata$Diabetes + pointdata$Fam.Diabetes ==2,1,ifelse(pointdata$Diabetes+pointdata$Fam.Diabetes==1,0.5,0))

pointdata$total <- rowSums(pointdata[,c("agescore","Anemia","Hypertension","Diabetes","CVDpoint")])

pointdata$value <- ifelse(pointdata$total>=4,1,0)

#########################################
#log regression  #same as class (we will end up with this model)

#install.packages('ROCR')
library(ROCR)


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
logit2prop(coef(model3))


res_test_ckd <- predict(res_train,data_test,type = "response") #with all variables
summary(res_test_ckd)

res_test_ckd <- as.data.frame(res_test_ckd)
res_test_ckd$GBM <- predictions_test$`1`



#res_train_ckd <- predict(res_train,training_data,type = "response")

table(Actualvalue = data_train$CKD, predictedvalue = mod_predict>0.5)

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

head(subset(cutoffs, fpr<0.2))

head(subset(cutoffs, fpr<0.7))

head(subset(cutoffs, fpr<0.120))
library(caret)
auc(model3)
#table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.16) 



#table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.17112)

#recall <- 
#precision <- 
#fmeasure <- 

## PPV curve #check

#old answer 0.1154

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.119) #final answer .119 

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.2)

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.07)


auc_glm <- performance(rocrpred, measure = "auc")
auc_glm <- auc_glm@y.values[[1]]
auc_glm
auc_gbm


####    OUTPUT    ###################

mod_predict_train <- predict(model3,data_train, type = "response") #after elimating using backward
summary(mod_predict_train)

mod_predict_test <- predict(model3, data_test, type = "response") #probabilities of testing set
summary(mod_predict_test)


mod_predict_test <- as.data.frame(mod_predict_test)


mod_predict_test$value <- ifelse(mod_predict_test[,1]>.119,1,0)
#mod_predict_test$pointsystem <- pointdata$value
trialscore <- ifelse(trial2mod$totalscore>4,1,0)

mod_predict_test$pointsystem <- trialscore

colnames(mod_predict_test) <- c("Predicted probabilities","Model outcome","Screening tool outcome")

write.csv(mod_predict_test,file = "Group4.csv")
rm(mod_predict_test)

##########

plot(rocrpref, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
abline(h= 0.72198,v=0.119)
#spl <- smooth.spline(rocrperf@y.values$num~rocrperf@x.values$num)

#recheck again
table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict>0.5315)
###########################################
install.packages("kableExtra")
library(kableExtra)
library(knitr)
install.packages("formattable")
library(formattable)
fancytable <- table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.119)


formattable(fancytable)
kable(fancytable)

fancytable[1:2,1:2] %>%
  kable("html") %>%
  kable_styling()

##########################################################################


#### IGNORE BELOW  ######


###################################################################################
install.packages('pROC')
library(pROC)
pROC(data_train$CKD,phat3)
model=roc(data_in$CKD~phat3,percent=TRUE,plot=TRUE)






## Step 2  - Missing Data
summary(data_in)
dim(data_in)
?na.omit
data_in=na.omit(data_in)
dim(data_in)

# Table comparison of data
table1 = c(mean(data$Age,na.rm = TRUE),
           mean(data$Hypertension,na.rm = TRUE),
           mean(data$Diabetes,na.rm = TRUE),
           mean(data$CVD,na.rm = TRUE),
           mean(data$Fam.Hypertension,na.rm = TRUE),
           mean(data$Fam.Diabetes,na.rm = TRUE),
           mean(data$Fam.CVD,na.rm = TRUE))

table2 = c(mean(data_in$Age),
           mean(data_in$Hypertension),
           mean(data_in$Diabetes),
           mean(data_in$CVD),
           mean(data_in$Fam.Hypertension),
           mean(data_in$Fam.Diabetes),
           mean(data_in$Fam.CVD))

table3 = cbind(table1,table2)
rownames(table3) <- c("Age","Hypertension","Diabetes","CVD","Fam.Hypertension","Fam.Diabetes","Fam.CVD")
colnames(table3) <- c("Average including missing data","Average exculding missing data")

## Step 3 and 4 - Correlation
cor(data_in)
summary(data_in)
data_new=model.matrix(~-1+Racegrp+CareSource,data=data_in)
summary(data_new)
data_in=data_in[,-c(4,8)]
data_in=cbind(data_in,data_new)
cor(data_in)
names(data_in)
data_in=data_in[,-33]
cor(data_in)

cor_data = data_in[,c(2,23,24,25,26,28,29)]

cor(data_in$CKD,cor_data)
## any highly correlated - large relation to PCA


## Step 5 - Run a Regression
model=lm(CKD~.,data=data_in)
summary(model)

summary(predict(model))

########################################
## Decision tree  #for later
library(rpart)
colnames(data)
#split in data into two parts  #no need to do this split has been provided based on CKD
#id <- sample(2,nrow(data_in),prob = c(0.7,0.3), replace = TRUE)
#data_in_train <- data_in[id==1,]
#data_in_test <- data_in[id==2,]
#tree_mod <- rpart(CKD~.,data = data_in_train)
#plot(tree_mod, margin = 0.2)
#text(tree_mod,use.n = TRUE,pretty = TRUE,cex = 0.8)


########################
##prediction ignore
library(caret)
#predictckd <- predict(tree_mod,newdata = data_in_test,type = "vector")
#predictckd

#glmpredictckd <- glm(CKD~.,data_in_train,family = "binomial")
#glmres <- predict(glmpredictckd,data_in_test,type = "response")
#table(Actualvalue = data_in_test$CKD, Predictedvalue = glmres>0.6)



