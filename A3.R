#excel file has many worksheets

library(XLConnectJars)
library(XLConnect)
workbook<-"IMB579-XLS-ENG.xlsx"
manipulator<-readWorksheet(loadWorkbook(workbook),sheet=2)
nonmanipulator<-readWorksheet(loadWorkbook(workbook),sheet=3)

complete.data<-readWorksheet(loadWorkbook(workbook),sheet=4)
sample.data<-readWorksheet(loadWorkbook(workbook),sheet=5) #220 companies 39 variables



#manipulator EDA
summary(manipulator)
plot(manipulator$DSRI,main='DSRI distribution',col='red')
plot(manipulator$GMI)
plot(manipulator$AQI)
plot(manipulator$SGI)
plot(manipulator$DEPI,main='DEPI distribution',col='blue')
plot(manipulator$SGAI)
plot(manipulator$ACCR,main='ACCR distribution',col='pink')
plot(manipulator$LEVI)

#non-manipulator EDA

#check any correlations between c.manipulator and all variables



#running a logistic regression model first to find out statistical significance of each indicator
library(ISLR)
attach(complete.data)
beneish.model<-glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,data=complete.data,family=binomial)
summary(beneish.model)
beneish.model1<-step(object=beneish.model,trace=0) #stepwise by AIC 
summary(beneish.model1)
anova(beneish.model1 ,test="Chisq")
coefficients(beneish.model1)


#Q3 sample data logistic regression
attach(sample.data)
bm<-glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+ACCR,data=sample.data,family=binomial)
summary(bm)
plot(bm)

#alternative 1- separate the sample data in to 2 parts 

set.seed(1234)
index<-sample(2,nrow(sample.data),replace=TRUE,prob=c(0.7,0.3))
train<-sample.data[index==1,] #group 1 with 70% of the data for training data
test<-sample.data[index==2,]
bm.1<-glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+ACCR,data=train,family=binomial)
summary(bm.1)
probs1<-predict(bm.1,test,type="response")
pred.1<-ifelse(probs1>=0.5,1,0)
pred.1<-factor(pred.1,levels=c(0,1),order='TRUE')
consufusion.1<-table(test$C.MANIPULATOR,pred.1)
consufusion.1


#visualize the result
roc.small<-roc(test$C.MANIPULATOR,probs1)
x<-1-roc.small$specificities
y<-roc.small$sensitivities
auc.small<-roc.small$auc

ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='pink')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc.small,digit=2)))+labs(x='False positive rate',y='sensitivity',title='ROC curve')

#5 
#random sampling first
non_manipulator<-nonmanipulator[sample(1:1200,156,replace=FALSE),]#randomly sampling for 156
#change the row name
names(manipulator)[11]<-'C.MANIPULATOR'
names(manipulator)[10]<-'Manipulator'
#change company id for better version
non_manipulator$Company.ID<-c(40:195)
#join this to the manipulator
new.data<-rbind(manipulator,non_manipulator)

#Q6
#packages for cross validation
library(lattice)
library(caret)
#cross validation
folds<-createFolds(new.data$Company.ID,k=10)#10 fold is optimum
str(folds)
#take a look on one of the regression model 
manipulate.train<-new.data[-folds$Fold09,]
manipulate.test<-new.data[folds$Fold09,]
detection.model<-glm(C.MANIPULATOR~DSRI+GMI+AQI+SGI+ACCR,data=manipulate.train,family=binomial)
summary(detection.model)


#use kappa function to evaluate
library(lpSolve)
library(irr)
str(cross_validate)
mean(unlist(cross_validate))

#7 CART 
library(rpart)
library(rpart.plot)
detection<-rpart(Manipulator~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,data=new.data,method='class')#by using new dataset
detection.1<-rpart(Manipulator~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,data=train,method='class')#by using the sample one
manipulator.pred<-predict(detection.1,test,type='class') #without type, output is probability
rpart.plot(detection,main='manipulator detection',type=3,extra=100)
rpart.plot(detection.1,type=3,extra=101)
better<-prune(detection.1, cp= detection.1$cptable[which.min(detection.1$cptable[,"xerror"]),"CP"])  
rpart.plot(better,type=3,extra=101)
detection.1
detection
summary(detection)
summary(detection.1)
detection.1$cptable
#confusion matrix






#Q8 use complete.data for logistic regression

set.seed(1234)
index.complete<-sample(2,nrow(complete.data),replace=TRUE,prob=c(0.7,0.3))
train.data<-complete.data[index.complete==1,] #group 1 with 70% of the data for training data
test.data<-complete.data[index.complete==2,]
bm.complete<-glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+ACCR,data=train.data,family=binomial)
summary(bm.complete)
prob<-predict(bm.complete,test.data,type="response")
pred<-ifelse(prob>=0.5,1,0)
pred<-factor(pred,levels=c(0,1),order='TRUE')
confusion<-table(test.data$C.MANIPULATOR,pred)
confusion



#alternative method 
#confusion matrix
library(gmodels)
confusion.matrix<-CrossTable(test.data$C.MANIPULATOR,pred)

#ROC curve
library(pROC)
library(ROCR)
#pred.2<-prediction(predictions=as.matrix(pred),labels=test.data$C.MANIPULATOR))
roc.curve<-roc(test.data$C.MANIPULATOR,prob)
x<-1-roc.curve$specificities
y<-roc.curve$sensitivities
library(ggplot2)
#plotting ROC curve
#plot(y~x,col='red',lty=3,lwd=3)
#abline(a=0,b=1,color='black')
#calculate auc value
#auc<-roc.curve$auc
#converting class to a vector
#auc<-unlist(slot)


auc<-roc.curve$auc


ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='red')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc,digit=2)))+labs(x='False positive rate',y='sensitivity',title='ROC curve')
 

#Q9
#random forest
library(randomForest)
set.seed(300)
rf<-randomForest(Manipulator~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,data=sample.data)

