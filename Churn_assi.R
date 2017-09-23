setwd("C:\\Users\\MYPC\\Desktop\\acadgild")
data_set<-read.csv("Churn.csv")
str(data_set)
summary(data_set)
head(data_set)
library(ggplot2)
colnames(data_set)
##number of people who have churned
table(data_set$Churn)

##STate that have max number of users still renewing and state that have max users who have churned
table(data_set$State,data_set$Churn)
qplot(State,data = data_set,facets = Churn~.)

colnames(data_set)
table(data_set$Phone)

qplot(Eve.Charge,data = data_set,fill=Churn=="1")
qplot(Account.Length,data = data_set,facets = Churn~.)
## It implies that phone numbers having custServ calls 5 or more than 5, they are more likly to churn. 
qplot(CustServ.Calls,data=data_set,fill=Churn=="1")
##Checking Intl.Calls vs Chrun- as phones numbers are making international calls <5 are most likly to churn.
qplot(Intl.Calls,data = data_set,facets = Churn~.)

data_set$Area.Code[data_set$State=="TX"]
## Splitting data for training and testing
library(caTools)
set.seed(144)
sample<-sample.split(data_set$Churn,0.8)
sample
train_data<-subset(data_set,sample=="TRUE")
test_data<-subset(data_set,sample=="FALSE")
str(train_data)
str(test_data)
colnames(train_data)
train_data<-train_data[,-c(19:21)]
str(train_data)
##checking multicollinearity
x<-cor(train_data)
corrplot::corrplot(cor(train_data),method = "circle")
##VMail.Message~VMail.Plan , Day.Mins~Day.Charge , Eve.Mins~Eve.Charge , Night.Charge~Night.Mins,
##Intl.Mins~Intl.Charge 
list(x>0.7)


##Making model with all independent variables.
model1<-glm(Churn~.,data = train_data,family = "binomial")
summary(model1)
##note: according to summary, customer service calls increase the prob of churn which can also be depicted by seeing graph.

##Making model using step wise selection of IV.
step(glm(Churn~.,data = train_data,family = "binomial"),direction = "backward")

##Making model after eliminating IV having multicollinearity and which are not in step wise selection.
model2<-glm(Churn~Account.Length+VMail.Message+Eve.Charge+Night.Mins+Intl.Charge+Intl.Calls+Day.Mins+CustServ.Calls,data = train_data,family = "binomial")
summary(model2)

##By comparing the above models on the basis of AIC value and other factors, model_final is best model.
model_final<-glm(formula = Churn ~ VMail.Message + Eve.Mins + Intl.Mins + 
                   CustServ.Calls + Int.l.Plan + VMail.Plan + Day.Charge + Night.Charge + 
                   Intl.Calls, family = "binomial", data = train_data)
summary(model_final)
predict_test<-predict(model_final,newdata = test_data,type = "response")
predict_test
##confusion matrix
table(test_data$Churn,predict_test>=0.5)
##accuracy of the model
(553+19)/(553+17+78+19)
## accuracy of Base line model
table(test_data$Churn)
570/(570+97)
## accuracy of our model and base line model is same so our model is not significant model.


hist(predict_test)
##above histogram showed that our data is skewed and accuracy is not right criteria to judge a model
##making ROCR curve
library(ROCR)
rocr_prediction<-prediction(predict_test,test_data$Churn)
perf<-performance(rocr_prediction,"tpr","fpr")
rocr_curve<-plot(perf)
str(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs

##AUC
auc<-performance(rocr_prediction,"auc")
auc<-unlist(slot(auc,"y.values"))
round(auc,3)
legend(.6,.2,round(auc,3),title = "AUC")


##We have to decrease FPR

##In my oppinion the thrushold of 0.18 is best suitable for this model.
table(test_data$Churn,predict_test>0.18)
