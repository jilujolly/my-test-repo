setwd("C:\Users\Jils\Documents")
#Reading the data file

Data1<-read.csv("UniversalBank.csv")
str(Data1)
# Converting the target variable to categorical

Data1$Personal.Loan<-as.factor(Data1$Personal.Loan)
str(Data1)

# Dropping ID column as it is a unique value and is not a value added feature

Data1$ID<-NULL
summary(Data1)

#Plots multiple
hist(Data1$Income)
hist(Data1$Mortgage) # since the values are mosly 0, this can be converted to a categorical value

Data1$Mortgage<-ifelse(Data1$Mortgage>0,1,0)
View(Data1)

#Correlation Plot

cor(Data1[,-c(1,10)])

install.packages("corrplot")
library("corrplot")

corrplot(cor(Data1[,-c(1,10)]),method = "number")

#Train-Test Split

set.seed(123)
rows=1:nrow(Data1)
trainRows=sample(rows,0.7*(nrow(Data1)))
traindata=Data1[trainRows,]
testdata=Data1[-trainRows,]

#model building
model<-glm(Personal.Loan~.-ID-Experience-ZIP.Code,traindata,family = binomial(link ="logit"))
summary(model)

#predictions
testdata$preds=predict(model,testdata,type="response")
str(testdata)

testdata$preds=ifelse(testdata$preds>0.5,1,0)
View(testdata)

#Metric calculation
table(testdata$Personal.Loan,testdata$preds,dnn = c("Actual","Prediction"))

