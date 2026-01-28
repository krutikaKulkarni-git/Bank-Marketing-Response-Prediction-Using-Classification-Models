~1
library(tidyverse)

#read the data, and examine summary statistics
setwd("file path")
bankData=read_csv2('bank-full.csv')   #read_csv2 is used since the data has ';' as delimiter

#look at the variables 
glimpse(bankData)

#get summary stats of the variables
summary(bankData)

#Convert the chr variables to factor
bData <- bankData %>% mutate_if(is.character,as.factor)%>%view()

str(bData)

#Are there any missing values ?
anyNA(bData)
colnames(bData)[colSums(is.na(bData))>0]

#get summary statistics on the variables
summary(bData)



#Data exploration


#what is the proportion of yes,no values in the dependent variable y?
bData %>% group_by(y) %>% summarize(n())
#or bData %>% group_by(y) %>% tally()

#To calculate the proportion of examples in each class                                  
bData %>% group_by(y) %>% summarise(n=n()) %>% mutate(proportion=n/sum(n))

#summarize all numeric variables, grouped by dependent(target) variable
bData %>% group_by(y) %>% summarize_if(is.numeric, mean)

bData %>% group_by(job, y) %>% summarize( n=n())
#for each type of job, gives the count of yes,no values of y

#If we then want to see the proportions 
bData %>% group_by(job, y) %>% summarize( n=n()) %>% mutate(freq=n/sum(n)) %>% view() 

#Check what you get with the group_by variables listed in different order
bData %>% group_by(y, job) %>% summarize( n=n()) %>% mutate(freq=n/sum(n)) %>% view()


#Look at other variables
bData %>% group_by(poutcome, y) %>% tally()
#what do you observe?  Might this variable be useful in predicting y?


#Look at the age variable 
boxplot(bData$age)
#or better
ggplot(bData, aes(age,  color=y) ) + geom_boxplot()
#try a density plot
ggplot(bData, aes(age,  color=y) ) + geom_density()


bData$ageGroup <- cut(bData$age, breaks = c(0, 30, 40, 50, 60, 100))

bData %>% group_by(ageGroup, y) %>% tally()
bData %>% group_by(ageGroup, y) %>% tally() %>% mutate(propResp=n/sum(n))

#can plot the response rate by age group
tmp <-bData %>% group_by(ageGroup, y) %>% tally() %>% mutate(propResp=n/sum(n)) 
ggplot(tmp, aes(y=propResp, x=ageGroup, fill=y))+geom_bar(stat = 'identity')
#or
ggplot(tmp, aes(y=propResp, x=ageGroup, fill=y))+geom_bar(stat = 'identity', position = position_dodge())

#Look at duration of calls 
ggplot(bData, aes(duration,  color=y) ) + geom_boxplot()
ggplot(bData, aes(duration,  color=y) ) + geom_density()


#Look also at number of calls (campaign)
summary(bData$campaign)
ggplot(bData, aes(campaign,  color=y) ) + geom_boxplot()

#examine duration and number of calls relationship, and by response(y=yes/no)
ggplot(bData, aes(duration, campaign, color=y))+geom_point()

~2
#Predicting response (y)

#select variables to be used for developing the predictive model
mData <- bData %>% select(-c('contact', 'day', 'month', 'duration', 'campaign', 'pdays', 'previous', 'poutcome'))

#Also remove ageGroup which you created for data exploration
mData <- mData %>% select(-c('ageGroup'))



~2#a
#Decision trees using the rpart package


library(rpart)

#develop a rpart decision tree model
rpDT1 <- rpart(y ~ ., data=mData, method="class")

#print the model -- text form
print(rpDT1)


#to correct for class-imbalance, use the prior parameter
rpDT2 = rpart(y ~ ., data=mData, method="class", parms=list(prior=c(.5,.5)))
# levels are used here to check whats the yes and no share in prior 

#Display/plot the tree
plot(rpDT2, uniform=TRUE,  main="Decision Tree for Bank marketing response")
text(rpDT2, use.n=TRUE, all=TRUE, cex=.7)


#Nicer way to display the tree using the rpart.plot package
library(rpart.plot)

rpart.plot::prp(rpDT2, type=2, extra=1)
# more information on such plots are in "Plotting rpart trees with the rpart.plot package" (http://www.milbo.org/rpart-plot/prp.pdf)


#Details on the DT model

summary(rpDT2)



#Variable importance as given by a decision tree model
rpDT1$variable.importance


mincp_i <- which.min(rpDT1$cptable[, 'xerror'])  #the row (index) corresponding to the min xerror

#The optimal xerror is the min_xError + xstd
optError <- rpDT1$cptable[mincp_i, "xerror"] + rpDT1$cptable[mincp_i, "xstd"]

#the row(index) of the xerror value which is closest to optError
optCP_i <- which.min(abs( rpDT1$cptable[,"xerror"] - optError))

#finally, get the best CP value corresponding to optCP_i
optCP <- rpDT1$cptable[optCP_i, "CP"]


#Now we can prune the tree based on this best CP value
rpDT1_p <- prune(rpDT1, cp = optCP)

#view the plot of the pruned tree
plot(rpDT1_p, uniform=TRUE,  main="Decision Tree for Bank Marketing")
text(rpDT1_p, use.n=TRUE, all=TRUE, cex=.7)

#obtain the predictions from the DT
predDT1<-predict(rpDT1_p, mData, type='class')

#confusion matrix using the table command
table(actuals=bData$y, preds=predDT1)

#split the data into training and test(validation) sets - 70% for training, rest for validation
nr=nrow(mData)
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) #get a random 70%sample of row-indices
mdTrn=mData[trnIndex,]   #training data with the randomly selected row-indices
mdTst = mData[-trnIndex,]  #test data with the other row-indices

dim(mdTrn) 
dim(mdTst)


#develop a tree on the training data
rpDT2=rpart(y ~ ., data=mdTrn, method="class",  control = rpart.control(cp = 0.0), parms=list(prior=c(.5,.5)) )


#Obtain the model's predictions on the training data
predTrn=predict(rpDT2, mdTrn, type='class')
#Confusion table
table(pred = predTrn, true=mdTrn$y)
#Accuracy
mean(predTrn==mdTrn$y)

table(pred=predict(rpDT2,mdTst, type="class"), true=mdTst$y)


mincp_i <- which.min(rpDT2$cptable[, 'xerror'])  #the row (index) corresponding to the min xerror

#The optimal xerror is the min_xError + xstd
optError <- rpDT2$cptable[mincp_i, "xerror"] + rpDT2$cptable[mincp_i, "xstd"]

#the row(index) of the xerror value which is closest to optError
optCP_i <- which.min(abs( rpDT2$cptable[,"xerror"] - optError))

#finally, get the best CP value corresponding to optCP_i
optCP <- rpDT2$cptable[optCP_i, "CP"]

#Now we can prune the tree based on this best CP value
rpDT2_p <- prune(rpDT2, cp = optCP)

#Lift curve

#get the 'scores' from applying the model to the data
predTrnProb=predict(rpDT2_p, mdTrn, type='prob')
head(predTrnProb)

#Create a data-frame with only the model scores and the actual class  (OUTCOME) values
trnSc <- mdTrn %>%  select("y")   # selects the OUTCOME column into trnSc
trnSc$score<-predTrnProb[, 2]

#take a look at trnSc
head(trnSc)

#sort by score
trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]

trnSc$cumResponse<-cumsum(trnSc$y == "yes")

#take a look at the first 10 row in trnSc
trnSc[1:10,]

#Plot the cumDefault values (y-axis) by numCases (x-axis)
plot( trnSc$cumResponse, type = "l", xlab='#cases', ylab='#default')
abline(0,max(trnSc$cumResponse)/nrow(trnSc), col="blue")  #diagonal line



#On the tst data
predTstProb=predict(rpDT2_p, mdTst, type='prob')
tstSc <- mdTst %>%  select("y")
tstSc$score<-predTstProb[, 2]
tstSc<-tstSc[order(tstSc$score, decreasing=TRUE),]
tstSc$cumResponse<-cumsum(tstSc$y == "yes")

plot( tstSc$cumResponse, type = "l", xlab='#cases', ylab='#default')
abline(0,max(tstSc$cumResponse)/nrow(tstSc), col="blue")  #diagonal line

#Calculate the decile lift table.

trnSc["bucket"]<- ntile(-trnSc[,"score"], 10)  
# this creates a new column with group number for each row

#group the data by the 'buckets', and obtain summary statistics 
dLifts <- trnSc %>% group_by(bucket) %>% summarize(count=n(), numResponse=sum(y=="yes"), 
                                                   respRate=numResponse/count,  cumRespRate=cumsum(numResponse)/cumsum(count),
                                                   lift = cumRespRate/(sum(trnSc$y=="yes")/nrow(trnSc)) ) 

#look at the table
dLifts


#you can do various plots, for example
plot(dLifts$bucket, dLifts$lift, xlab="deciles", ylab="Cumulative Decile Lift", type="l")
barplot(dLifts$numResponse, main="numDefaults by decile", xlab="deciles")



#Do the above analyses for the test data
tstSc["bucket"]<- ntile(-tstSc[,"score"], 10)  
# this creates a new column with group number for each row

#group the data by the 'buckets', and obtain summary statistics 
dLifts <- tstSc %>% group_by(bucket) %>% summarize(count=n(), numResponse=sum(y=="yes"), 
                                                   respRate=numResponse/count,  cumRespRate=cumsum(numResponse)/cumsum(count),
                                                   lift = cumRespRate/(sum(trnSc$y=="yes")/nrow(trnSc)) ) 

#look at the table
dLifts



#(there are different packages to give us the lift, etc., but it is useful to be able to do customized calculations)


#ROC curves (using the ROCR package)
library('ROCR')

#obtain the scores from the model for the class of interest, here, the prob('default')
scoreTst=predict(rpDT2_p, mdTst, type="prob")[,'yes']  
#same as predProbTst

#now apply the prediction function from ROCR to get a prediction object
rocPredTst = prediction(scoreTst, mdTst$y, label.ordering = c('no', 'yes'))  

#obtain performance using the function from ROCR, then plot
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst)
abline(0,1)

#Other performance from ROCR

#AUC value
aucPerf=performance(rocPredTst, "auc")
aucPerf@y.values

#Accuracy 
accPerf <-performance(rocPredTst, "acc")
plot(accPerf)

#optimal threshold for max overall accuracy
accPerf@x.values[[1]][which.max(accPerf@y.values[[1]])]


#optimal cost with different costs for fp and fn
costPerf = performance(rocPredTst, "cost", cost.fp = 1, cost.fn = 3)
costPerf@x.values[[1]][which.min(costPerf@y.values[[1]])]


#Lift curve
liftPerf <-performance(rocPredTst, "lift", "rpp")
plot(liftPerf, main="Lift chart")

~2#b
#C5.0 decision trees
library(C50)

#build a tree model
c5DT1 <- C5.0(y ~ ., data=mdTrn, control=C5.0Control(minCases=10))

#model details
summary(c5DT1)


#Can try to use costs to try overcome class imbalance in data
costMatrix <- matrix(c(
  0,   1,
  10,  0),
  2, 2, byrow=TRUE)
rownames(costMatrix) <- colnames(costMatrix) <- c("yes", "no")

costMatrix 

c5DT1 <- C5.0(y ~ ., data=mdTrn, control=C5.0Control(minCases=10), costs=costMatrix)


#performance
predTrn <- predict(c5DT1, mdTrn)
table( pred = predTrn, true=mdTrn$y)
mean(predTrn==mdTrn$y)

predTst <- predict(c5DT1, mdTst)
table( pred = predTst, true=mdTst$y)
mean(predTst==mdTst$y)

#variable importance
C5imp(c5DT1)

#tree summary
summary(c5DT1)


#Rules - DT simplified to a set of rules
c5rules1 <- C5.0(y ~ ., data=mdTrn, control=C5.0Control(minCases=10), rules=TRUE)
summary(c5rules1)

#Or try with costs
c5rules1 <- C5.0(y ~ ., data=mdTrn, control=C5.0Control(minCases=10), rules=TRUE, costs=costMatrix)
summary(c5rules1)


#performance
predTrn <- predict(c5rules1, mdTrn)
table( pred = predTrn, true=mdTrn$y)
mean(predTrn==mdTrn$y)

predTst <- predict(c5rules1, mdTst)
table( pred = predTst, true=mdTst$y)
mean(predTst==mdTst$y)


#variable importance
C5imp(c5rules1)

#tree summary
summary(c5rules1)

~2#c
{r}
rfNews()
library('randomForest')
library('ROCR')

#for reproducible results, set a specific value for the random number seed
set.seed(576)

#for ntree=200
rfModel_1 = randomForest(y ~ ., data=mdTrn, ntree=200, importance=TRUE )

importance(rfModel_1) %>% view()
varImpPlot(rfModel_1)


#Classification performance
CTHRESH = 0.5

#For training data
rfPred<-predict(rfModel_1, mdTrn, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTrn$y)
mean(pred==mdTrn$y)
#mean=0.8847952

#For test data
rfPred<-predict(rfModel_1,mdTst, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTst$y)
mean(pred==mdTst$y)
#mean=0.8840227


#for ntree=500
rfModel_2 = randomForest(y ~ ., data=mdTrn, ntree=500, importance=TRUE )

importance(rfModel_2) %>% view()
varImpPlot(rfModel_2)


#Classification performance
CTHRESH = 0.5

#For training data
rfPred<-predict(rfModel_2, mdTrn, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTrn$y)
mean(pred==mdTrn$y)
#mean=0.8849532

#For test data
rfPred<-predict(rfModel_2,mdTst, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTst$y)
mean(pred==mdTst$y)
#mean=0.8840964

#for ntree=1000
rfModel_3 = randomForest(y ~ ., data=mdTrn, ntree=1000, importance=TRUE )

importance(rfModel_3) %>% view()
varImpPlot(rfModel_3)


#Classification performance
CTHRESH = 0.5

#For training data
rfPred<-predict(rfModel_3, mdTrn, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTrn$y)
mean(pred==mdTrn$y)
#mean=0.8847005

#For test data
rfPred<-predict(rfModel_3,mdTst, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTst$y)
mean(pred==mdTst$y)
#mean=0.8840227

#comparing the test data of various models
scoreTst_1=predict(rfModel_1, mdTst, type="prob")[,'yes']
rocPredTst_1 = prediction(scoreTst_1, mdTst$y, label.ordering = c('no', 'yes'))
perfROCTst1 = performance(rocPredTst_1, "tpr", "fpr")


scoreTst_2=predict(rfModel_2, mdTst, type="prob")[,'yes']
rocPredTst_2 = prediction(scoreTst_2, mdTst$y, label.ordering = c('no', 'yes'))
perfROCTst2 = performance(rocPredTst_2, "tpr", "fpr")

scoreTst_3=predict(rfModel_3, mdTst, type="prob")[,'yes']
rocPredTst_3 = prediction(scoreTst_3, mdTst$y, label.ordering = c('no', 'yes'))
perfROCTst3 = performance(rocPredTst_3, "tpr", "fpr")

plot(perfROCTst1, col='red', maim="")
plot(perfROCTst2, col='blue', add=TRUE)
plot(perfROCTst3, col='Green', add=TRUE)

#comparing the training and test data performance
#ROC and AUC on ntree=200 for training data
scoreTst_Tst1=predict(rfModel_1, mdTst, type="prob")[,'yes']
rocPredTst_2 = prediction(scoreTst_2, mdTst$y, label.ordering = c('no', 'yes'))
perfROCTst2 = performance(rocPredTst_2, "tpr", "fpr")


#AUC value
aucPerf_1=performance(rocPredTrn_1, "auc")
aucPerf_1@y.values
#0.9327534

#Accuracy 
accPerf_1 <-performance(rocPredTrn_1, "acc")

#ROC and AUC on ntree=200 for testing data
scoreTst_2=predict(rfModel_1, mdTst, type="prob")[,'yes']
rocPredTst_2 = prediction(scoreTst_2, mdTst$y, label.ordering = c('no', 'yes'))
perfROCTst2 = performance(rocPredTst_2, "tpr", "fpr")

#AUC value
aucPerf_2=performance(rocPredTst_2, "auc")
aucPerf_2@y.values
#0.6725233

#Accuracy 
accPerf_2 <-performance(rocPredTst_2, "acc")




plot(accPerf_1, col='red', main="")
plot(accPerf_2, col='blue', add=TRUE)



plot(perfROCTst1, col="red", main="")
plot(perfROCTst2, col="blue", add=TRUE)




#Looking at the confusion matrix, do you think a different value of THRESH will be better?
#Try with THRESH=0.1 ?  (why?  relation to class imbalance?)

CTHRESH = 0.1

#For training data
rfPred<-predict(rfModel_1, mdTrn, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTrn$y)
mean(pred==mdTrn$y)
#mean=0.9167404

#For test data
rfPred<-predict(rfModel_1,mdTst, type="prob")
pred = ifelse(rfPred[, 'yes'] >= CTHRESH, 'yes', 'no')
table( pred = pred, true=mdTst$y)
mean(pred==mdTst$y)
#mean=0.8638944

#ROC curve for the randomForest model
perf_rfTst=performance(prediction(predict(rfModel,mdTst, type="prob")[,2], mdTst$y), "tpr", "fpr")
plot(perf_rfTst)


#Do a lift analyses
#Lift curve
liftPerf1 <-performance(rocPredTrn_1, "lift", "rpp")
plot(liftPerf1, main="Lift chart1 Training Data")

#Lift curve
liftPerf2 <-performance(rocPredTst_2, "lift", "rpp")
plot(liftPerf2, main="Lift chart2 Test Data")

~2#d
{r}
install.packages('GBM')
library('gbm')

#gbm looks for 0,1 values in the dependent variable -- obtained here using unclass()
gbm_M1 <- gbm(formula= unclass(y)-1 ~., data=mdTrn,distribution = "bernoulli", n.trees=1000, shrinkage=0.025, interaction.depth = 4, bag.fraction=0.5, cv.folds = 5,  n.cores=NULL)  


#Look at the resulting model
gbm_M1
#what is the best iteration?  

#variable importance
summary(gbm_M1)

#plot of cv performance by iterations
bestIter<-gbm.perf(gbm_M1, method='cv')
#bestIter gives the best iteration value, which we can use for obtaining predictions

#performance of the gbm model on training and test data

predicted_probabilities<- predict(gbm_M1, newdata = mdTrn, n.tree= bestIter, type="response")
head(scores_gbmM1)

#0.11925057 0.07681795 0.06329522 0.05825239 0.04848237 0.10306441
#these are the scores for the '1' class

# Convert probabilities into class labels (0/1)
threshold <- 0.5
predicted_labels <- ifelse(predicted_probabilities > threshold, "yes", "no")
mean(predicted_labels==mdTrn$y)
#0.8864067



predicted_probabilities <- predict(gbm_M1, newdata = mdTst, n.trees = 100, type = "response")

# Convert probabilities into class labels (0/1)
threshold <- 0.5
predicted_labels <- ifelse(predicted_probabilities > threshold, "yes", "no")
mean(predicted_labels==mdTst$y)
#0.8838752

#Obtain various performance metrics as earlier

#ROC curve on Training and Test Data
scores_gbmM1<- predict(gbm_M1, newdata = mdTrn, n.tree= bestIter, type="response")
pred_gbmM1Trn <- prediction( scores_gbmM1, mdTrn$y, label.ordering = c("no", "yes"))
rocPerf_gbmM1Trn <-performance(pred_gbmM1Trn, "tpr","fpr")
plot(rocPerf_gbmM1Trn)
abline(a=0, b= 1)

scores_gbmM1<- predict(gbm_M1, newdata = mdTst, n.tree= bestIter, type="response")
pred_gbmM1Tst <- prediction( scores_gbmM1, mdTst$y, label.ordering = c("no", "yes"))
rocPerf_gbmM1Tst <-performance(pred_gbmM1Tst, "tpr","fpr")
plot(rocPerf_gbmM1Tst)
abline(a=0, b= 1)


#AUC value
aucPerf_gbmM1Trn=performance(pred_gbmM1Trn, "auc")
aucPerf_gbmM1Trn@y.values
#0.7182762

aucPerf_gbmM1Tst=performance(pred_gbmM1Tst, "auc")
aucPerf_gbmM1Tst@y.values
#0.7048169

#Accuracy 
accPerf_2Trn <-performance(pred_gbmM1Trn, "acc")
plot(accPerf_2Trn, main="")
accPerf_2Tst <-performance(pred_gbmM1Tst, "acc")
plot(accPerf_2Tst, main="")

#Do a lift analyses
#Lift curve
liftPerf1Trn <-performance(pred_gbmM1Trn, "lift", "rpp")
plot(liftPerf1Trn, main="Lift chart1 Training Data")

#Lift curve
liftPerf2Tst <-performance(pred_gbmM1Tst, "lift", "rpp")
plot(liftPerf2Tst, main="Lift chart2 Test Data")

~2#e
library(naivebayes)
#plotting for training
nbM1<-naive_bayes(y ~ ., data = mdTrn) 
nbM1
plot(nbM1)

#plotting for test
nbM1_test <- naive_bayes(y ~ ., data = mdTst) 
nbM1_test
plot(nbM1_test)

#Obtain predictions for training
nbPred = predict(nbM1, mdTrn, type='prob')
head(nbPred)

#Obtain predictions for test
nbPredTest = predict(nbM1_test, mdTst, type='prob')
head(nbPred)
#so the second column of values gives the prob for "yes" ?

#confusion matrix for training
THRESH=0.5
pred=ifelse(nbPred[, 2] > 0.5, "yes", "no")
table(pred=nbPred[, 2] > THRESH, actual=mdTrn$y)
mean(pred == mdTst$y)

##confusion matrix for test

THRESH=0.5
pred_test=ifelse(nbPredTest[, 2] > 0.5, "yes", "no")
table(pred_test=nbPredTest[, 2] > THRESH, actual=mdTst$y)
mean(pred_test== mdTst$y)



#confusion matrix for test
THRESH=0.5
table(pred=nbPredTest[, 2] > THRESH, actual=mdTst$y) 

#Try other thresholds
#Draw the ROC curve a before


#Develop a naive-Bayes model with useKernel=True  (what does this do?)
nbM2<-naive_bayes(y ~ ., data = mdTrn, usekernel = T) 
nbM2_test<-naive_bayes(y ~ ., data = mdTst, usekernel = T) 
plot(nbM2)

library(e1071)
library(naivebayes)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ROCR)
#ROC FOR TRAINING

nbrocPredTrn <- prediction(predict(nbM2,mdTrn, type="prob")[,2], mdTrn$y)

perf_nbTrn_kernel=performance(prediction(predict(nbM2,mdTrn, type="prob")[,2], mdTrn$y), "tpr", "fpr")
plot(perf_nbTrn_kernel)
abline(0,1)

#ROC FOR TEST
nbrocPredTst <- prediction(predict(nbM2_test,mdTst, type="prob")[,2], mdTst$y)
perf_nbTst_kernel=performance(prediction(predict(nbM2_test,mdTst, type="prob")[,2], mdTst$y), "tpr", "fpr")
plot(perf_nbTst_kernel)
abline(0,1)

#Auc for training 
aucPerf = performance(nbrocPredTrn, "auc")
aucPerf@y.values
#Auc for Test
aucPerf = performance(nbrocPredTst, "auc")
aucPerf@y.values

#Accuracy for training data
accPerf <- performance(nbrocPredTrn, "acc")
plot(accPerf)

#Accuracy for testing data
accPerf <- performance(nbrocPredTst, "acc")
plot(accPerf)

#lift curve for testing data
nbliftTst <- performance(nbrocPredTst , "lift","rpp")
plot(nbliftTst, main="lift chart")

#lift curve for training data
nbliftTrn <- performance(nbrocPredTrn , "lift","rpp")
plot(nbliftTrn, main="lift chart")
#Evaluate performance

# Load necessary libraries
library(naivebayes)
library(ROCR)  # For ROC and performance evaluation

# Train the Naïve Bayes model with Kernel Density Estimation (KDE) on training data
nbM2_kernel <- naive_bayes(y ~ ., data = mdTrn, usekernel = TRUE)

# For testing, train on test data
nbM2_kernel_tst <- naive_bayes(y ~ ., data = mdTst, usekernel = TRUE)

# Evaluate performance with KDE for training data
nbrocPredTrn_kernel = prediction(predict(nbM2_kernel, mdTrn, type = "prob")[, 2], mdTrn$y)

library(naivebayes)
#Plots to check the continuous variables are 
plot(density(mdTrn$age), main = "Age Density Plot")
plot(density(mdTrn$balance), main = "Balance Plot")

nbM1_trn <-naive_bayes(y ~ ., data = mdTrn) 

nbM1_trn
plot(nbM1_trn)

nbM1_tst <-naive_bayes(y ~ ., data = mdTst)
nbM1_tst
plot(nbM1_tst)
#Obtain predictions for training data
nbPredTrn = predict(nbM1_trn, mdTrn, type='prob')
head(nbPredTrn)
#Obtain predictions for test data
nbPredtst = predict(nbM1_tst, mdTst, type='prob')
head(nbPredtst)
#so the second column of values gives the prob for "yes" ?

THRESH=0.5
pred_nbTrn =ifelse(nbPredTrn[, 2] > 0.5, "yes", "no")
table(pred=nbPredTrn[, 2] > 0.5, actual=mdTrn$y)
mean(pred_nbTrn == mdTrn$y)

pred_nbTst =ifelse(nbPredtst[, 2] > 0.5, "yes", "no")
table(pred=nbPredtst[, 2] > 0.5, actual=mdTst$y)
mean(pred_nbTst == mdTst$y)

#Develop a naive-Bayes model with useKernel=True  (what does this do?)
#For Training Data
nbM2_kernel <-naive_bayes(y ~ ., data = mdTrn, usekernel = T) 
nbM2_kernel_tst <-naive_bayes(y ~ ., data = mdTst, usekernel = T) 
plot(nbM2)

nbPredTrn_kernel = predict(nbM2_kernel, mdTrn, type='prob')

pred_nbTrn_kernel =ifelse(nbPredTrn_kernel[, 2] > 0.7896977, "yes", "no")
table(pred=nbPredTrn_kernel[, 2] > 0.5, actual=mdTrn$y)
mean(pred_nbTrn_kernel == mdTrn$y)
#For Test Data
nbPredTst_kernel = predict(nbM2_kernel_tst, mdTst, type='prob')

pred_nbTst_kernel =ifelse(nbPredTst_kernel[, 2] > 0.8452133, "yes", "no")
table(pred=nbPredTst_kernel[, 2] > 0.5, actual=mdTst$y)
mean(pred_nbTst_kernel == mdTst$y)


#Try other thresholds
#Draw the ROC curve a before



#Evaluate performance with KDE
# for training Data
nbrocPredTrn_kernel = prediction(predict(nbM2_kernel,mdTrn, type="prob")[,2], mdTrn$y)

perf_nbTrn_kernel=performance(prediction(predict(nbM2_kernel,mdTrn, type="prob")[,2], mdTrn$y), "tpr", "fpr")
plot(perf_nbTrn_kernel)
abline(0,1) 

#AUC value
aucPerf=performance(nbrocPredTrn_kernel, "auc")
aucPerf@y.values

#Accuracy 
accPerf <-performance(nbrocPredTrn_kernel, "acc")
plot(accPerf)

#optimal threshold for max overall accuracy
accPerf@x.values[[1]][which.max(accPerf@y.values[[1]])]


#optimal cost with different costs for fp and fn
costPerf = performance(nbrocPredTrn_kernel, "cost", cost.fp = 1, cost.fn = 3)
costPerf@x.values[[1]][which.min(costPerf@y.values[[1]])]



#Lift curve
nbliftPerfTrn <-performance(nbrocPredTrn_kernel, "lift", "rpp")
plot(nbliftPerfTrn, main="Lift chart")

# for Test Data
nbrocPredTst_kernel = prediction(predict(nbM2_kernel_tst,mdTst, type="prob")[,2], mdTst$y)

perf_nbTst_kernel=performance(prediction(predict(nbM2_kernel_tst,mdTst, type="prob")[,2], mdTst$y), "tpr", "fpr")
plot(perf_nbTst_kernel)
abline(0,1) 

#AUC value
aucPerf=performance(nbrocPredTst_kernel, "auc")
aucPerf@y.values

#Accuracy 
accPerf <-performance(nbrocPredTst_kernel, "acc")
plot(accPerf)

#optimal threshold for max overall accuracy
accPerf@x.values[[1]][which.max(accPerf@y.values[[1]])]


#optimal cost with different costs for fp and fn
costPerf = performance(nbrocPredTst_kernel, "cost", cost.fp = 1, cost.fn = 3)
costPerf@x.values[[1]][which.min(costPerf@y.values[[1]])]



#Lift curve
nbliftPerfTst <-performance(nbrocPredTst_kernel, "lift", "rpp")
plot(nbliftPerfTst, main="Lift chart")
```

~2#f

# this is just a template with some fake values
library(dplyr)


df <- data.frame(
  id = 1:3,  
  test_accuracy = runif(3, min = 0.8, max = 1),  # use predict()
  precision = runif(3, min = 0.7, max = 1)  # F1-score: 2PR/P+R, P=tp/(tp+fp), R=tp/(tp+fn)
)

print(df)

#naive-Bayes model
nbPred <- predict(nbM1, mdTst, type='prob')[, "yes"]
nbROCPred <- prediction( nbPred, mdTst$y, label.ordering = c("no", "yes") )
nbPerfROC <- performance(nbROCPred, "tpr", "fpr")
plot(nbPerfROC, col='black') 

#rpart DT model
dtrPred <- predict(rpDT2_p, mdTst, type="prob")[,'yes']  
dtrROCPred <- prediction( dtrPred, mdTst$y, label.ordering = c("no", "yes") )
dtrPerfROC <- performance(dtrROCPred, "tpr", "fpr")
plot(dtrPerfROC, add=TRUE, col='blue') 

#random forest model
rfPred <-predict(rfModel_1,mdTst, type="prob")[, 'yes']
rfROCPred <- prediction( rfPred, mdTst$y, label.ordering = c("no", "yes") )
rfPerfROC <- performance(rfROCPred, "tpr", "fpr")
plot(rfPerfROC, add=TRUE, col='green') 

#gbm model
gbmPred <- predict(gbm_M1, newdata = mdTst, n.tree= bestIter, type="response")
gbmROCPred <- prediction( gbmPred, mdTst$y, label.ordering = c("no", "yes") )
gbmPerfROC <- performance(gbmROCPred, "tpr", "fpr")
plot(gbmPerfROC, add=TRUE, col='red')

#Add legend
legend('bottomright', c('nB', 'rpartDT', 'rf', 'gbm'), lty=1, col=c('black', 'blue', 'green', 'red'))
abline(0,1)  #add the diagonal reference line
plot(liftrp, col='black',main='LIFT CHART')
plot(liftPerf2, col='red', add=TRUE)
plot(liftPerf2Tst, col='blue', add = TRUE )
plot(nbliftPerfTst, col='green', add = TRUE )
legend('bottomright', c('nB', 'rpartDT', 'rf', 'gbm'), lty=1, col=c('green', 'black', 'red', 'blue'))
abline(0,1)  #add the diagonal reference line




