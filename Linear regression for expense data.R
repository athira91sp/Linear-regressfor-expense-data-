# model for expenses dataset
getwd()
setwd("E:\\Jigsaw\\linear regression")
getwd()
exp<-read.csv("expenses.csv")

#Data exploration
summary(exp)
dim(exp)
str(exp)
View(exp)
#NA
ind<-which(is.na(exp$bmi))
ind
exp$bmi[ind]<-mean(exp$bmi,na.rm = T)
View(exp$bmi[ind])

#Data Tabulation
t1<-table(exp$bmi)
View(t1)
t2<-table(exp$age)
View(t2)

#summary
summary(exp)

# Check for outliers in the dependent variable
hist(exp$age)
x<-boxplot(exp$age)
x
out<-x$out
out

#Remove outlier
index<-which(exp$age %in% x$out) # %in% used to pass more than 1 value
index

#imputing mean values
exp$age[index]<-mean(exp$age,na.rm=T)
View(exp$age[index])

#considering dependent variable
boxplot(exp$charges)


#univariate visualisation
#seen from summary

#assgn 0 for female and 1 for male
exp$sex1<-ifelse(exp$sex=='male',1,0)
View(exp)
exp$smoker1<-ifelse(exp$smoker=='yes',1,0)
View(exp)

#assgn dummy variables in region
exp$south<-ifelse(exp$region %in% c("southwest","southeast"),1,0)
View(exp)
#building model-simple linear regression
model1<-lm(charges~age,data=exp) #lm =linear modelling
model1
summary(model1) #pvalue=2.2e-16 <alpha,R square -age has only 9% effect on the charges

model2<-lm(charges~age+bmi+smoker1+sex1+south+children,exp)
model2
summary(model2)

model3<-lm(charges~age+bmi+smoker1+south+children,exp)
model3
summary(model3)

model4<-lm(charges~age+bmi+smoker1+children,exp)
model4
summary(model4)

#predited values
predsales1<-predict(model4,data=exp)
class(predsales1)
head(predsales1)
exp$predd<-predsales1

library(car)
vif(model4) #vif values less than 4,so it not multicollinear

exp$resi1<-resid(model4)
View(exp)

plot(exp$resi1) #no funnel, homoscadastic

#hist
hist(exp$resi1)      #slightly right skewed,mean>median
summary(exp$resi1)
#MAPE
exp$mape<-(abs(exp$resi1)/exp$charges)*100
mean(exp$mape) # mape>5,model not good       

#fitchart
library(ggplot2)
qplot(exp$charges,exp$predd)

dat<-data.frame(act=exp$charges,est=exp$predd)

rnum<-as.numeric(row.names(dat))
head(rnum)
p<-ggplot(dat,aes(x=rnum,y=act))
p+geom_line(color="blue")+geom_line(data=dat,aes(y=est),color="green")




