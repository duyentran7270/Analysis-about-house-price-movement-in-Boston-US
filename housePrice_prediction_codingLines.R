rm(list=ls())

library(ggplot2)
library(dplyr)
library(corrplot)

#Check each dataset characteristics/nature: 
data0<-read.csv('Housing_data.csv',header=TRUE,sep=',')
str(data0)

    ## PCCR variable
data0$PCCR
class(data0$PCCR) 
mean(data0$PCCR)
min(data0$PCCR)
max(data0$PCCR)
median(data0$PCCR)

plot(data0$PCCR,ylab='PCCR',main='Per capita crime rate by town',pch=16,col='blue')
abline(a=mean(data0$PCCR),b=0,lty=2,col='red')
abline(a=40,b=0,lty=2,col='brown')

hist(data0$PCCR,main='Histogram of PCCR',xlab='PCCR')

boxplot(data0$PCCR,ylab='PCCR',medcol='red',boxlty=0,outpch=16,main='Boxplot of PCCR')
abline(a=40,b=0,lty=2,col='red')

par(mfrow=c(1,3))
  plot(data0$PCCR,ylab='PCCR',main='Per capita crime rate by town',pch=16,col='blue')
    abline(a=mean(data0$PCCR),b=0,lty=2,col='red')
  hist(data0$PCCR)
  boxplot(data0$PCCR,ylab='PCCR',medcol='red',boxlty=0,outpch=16)

ggplot(data0,aes(x=PCCR))+
  geom_histogram()+
  ggtitle('Per capita crime rate by town')

par(mfrow=c(1,2))  
ggplot(data0,(aes(x=c(1:506),y=PCCR)))+
  geom_line()+
  ggtitle('Per capita crime rate by town')

ggplot(data0,aes(x=PCCR))+
  geom_density()+
  ggtitle('Per capita crime rate by town')
    
    ## PRLZ 
data0$PRLZ
class(data0$PRLZ) 
mean(data0$PRLZ)
median(data0$PRLZ)
min(data0$PRLZ)
max(data0$PRLZ)

par(mfrow=c(1,3)) 
plot(data0$PRLZ,ylab='PRLZ',main='PRLZ Scatter plot',pch=16,col='blue')
abline(a=mean(data0$PRLZ),b=0,lty=2,col='red')

hist(data0$PRLZ,main='Histogram of PRLZ',xlab='PRLZ')

boxplot(data0$PRLZ,ylab='PRLZ',medcol='red',boxlty=0,outpch=16,main='Boxplot of PRLZ')
abline(a=mean(data0$PRLZ),b=0,lty=2,col='red')

ggplot(data0,aes(x=PRLZ))+
  geom_density()+
  ggtitle('PRLZ density graph')

    ## INDUS 
data0$INDUS
class(data0$INDUS) 
mean(data0$INDUS)
median(data0$INDUS)
min(data0$INDUS)
max(data0$INDUS)

plot(data0$INDUS,ylab='INDUS',main='Scatter plot of INDUS',pch=16,col='blue')
abline(a=mean(data0$INDUS),b=0,lty=2,col='red')

hist(data0$INDUS,main='Histogram of INDUS',xlab='INDUS',ylim=c(0,200),xlim=c(0,30),breaks=15)

boxplot(data0$INDUS,ylab='INDUS',medcol='red',boxlty=0,outpch=16,main='Boxplot of INDUS')

    ## NOX 
data0$NOX
class(data0$NOX) 
mean(data0$NOX)
median(data0$NOX)
min(data0$NOX)
max(data0$NOX)

plot(data0$NOX,ylab='NOX',main='Scatter plot of NOX',pch=16,col='blue')
abline(a=mean(data0$NOX),b=0,lty=2,col='red')

hist(data0$NOX,main='Histogram of NOX',xlab='NOX')
boxplot(data0$NOX,ylab='NOX',medcol='red',boxlty=0,outpch=16,main='Boxplot of NOX')

ggplot(data0,aes(x=NOX))+
  geom_density()+
  ggtitle('Density Graph of NOX')
  
    ##AVR
data0$AVR
class(data0$AVR) 
mean(data0$AVR)
median(data0$AVR)
min(data0$AVR)
max(data0$AVR)

par(mfrow=c(1,3))
plot(data0$AVR,ylab='AVR',main='Scatter Plot of AVR',pch=16,col='blue')
  abline(a=mean(data0$AVR),b=0,col='red')
hist(data0$AVR,main='Histogram of AVR',xlab='AVR')
boxplot(data0$AVR,ylab='AVR',medcol='red',boxlty=0,outpch=16,main='Boxplot of AVR')

ggplot(data0,(aes(x=c(1:506),y=AVR)))+
  geom_line()+
  ggtitle('Line graph of AVR')

ggplot(data0,aes(x=AVR))+
  geom_density()+
  ggtitle('Density graph of AVR')
    
    ##AGE
data0$AGE
class(data0$AGE) 
mean(data0$AGE)
median(data0$AGE)
min(data0$AGE)
max(data0$AGE)

par(mfrow=c(1,3))
plot(data0$AGE,ylab='AGE',main='Scatter Plot of AGE',pch=16,col='blue')
  abline(a=mean(data0$AGE),b=0,col='red')
hist(data0$AGE,main='Histogram of AGE',xlab='AGE',ylim=c(1,200))
boxplot(data0$AGE,ylab='AGE',medcol='red',boxlty=0,outpch=16,main='Boxplot of AGE')

ggplot(data0,aes(x=AGE))+
  geom_density()+
  ggtitle('Density graph of AGE')

    ##DIS 
data0$DIS
class(data0$DIS) 
mean(data0$DIS)
median(data0$DIS)
min(data0$DIS)
max(data0$DIS)

par(mfrow=c(1,3))
plot(data0$DIS,ylab='DIS',main='Scatter Plot of DIS',pch=16,col='blue')
abline(a=mean(data0$DIS),b=0,col='red')
hist(data0$DIS,main='Histogram of DIS',xlab='DIS')
boxplot(data0$DIS,ylab='DIS',medcol='red',boxlty=0,outpch=16,main='Boxplot of DIS')

ggplot(data0,aes(x=DIS))+
  geom_density()+
  ggtitle('Density graph of DIS')

    ##RAD 
data0$RAD
class(data0$RAD) 
mean(data0$RAD)
median(data0$RAD)
min(data0$RAD)
max(data0$RAD)

plot(data0$RAD,ylab='RAD',main='Scatter Plot of RAD',pch=16,col='blue')
  abline(a=mean(data0$RAD),b=0,col='red')
hist(data0$RAD,main='Histogram of RAD',xlab='RAD')
boxplot(data0$RAD,ylab='RAD',medcol='red',boxlty=0,outpch=16,main='Boxplot of RAD')

    ##TAX 
data0$TAX
class(data0$TAX) 
mean(data0$TAX)
median(data0$TAX)
min(data0$TAX)
max(data0$TAX)

plot(data0$TAX,ylab='TAX',main='Scatter Plot of TAX',pch=16,col='blue')
abline(a=mean(data0$TAX),b=0,col='red')
hist(data0$TAX,main='Histogram of TAX',xlab='TAX')
boxplot(data0$TAX,ylab='TAX',medcol='red',boxlty=0,outpch=16,main='Boxplot of TAX')

    ## MEDV 
data0$MEDV
class(data0$MEDV) 
mean(data0$MEDV)
median(data0$MEDV)
min(data0$MEDV)
max(data0$MEDV)

plot(data0$MEDV,ylab='MEDV',main='Scatter Plot of MEDV',pch=16,col='blue')
abline(a=mean(data0$MEDV),b=0,col='red')
hist(data0$MEDV,main='Histogram of MEDV',xlab='MEDV')
boxplot(data0$MEDV,ylab='MEDV',medcol='red',boxlty=0,outpch=16,main='Boxplot of MEDV')

ggplot(data0,aes(x=MEDV))+
  geom_density()+
  ggtitle('Density graph of MEDV')

#Correlation analysis:  
cor(data0)

corrplot(cor(data0),'number')
corrplot(cor(data0),'ellipse')

#Regression model implementation: (model0)
model0<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,data=data0)
summary(model0)
coef(model0)

#Adjusted model
#No Intercept (model0.1)
model0.1<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX+0,data=data0)
summary(model0.1)
    ##-> Change significantly
# No INDUS (model1)
model1<-lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0,data=data0)
summary(model1)
    ##-> Not much different => No impact. pp-value of all theta is less than 0.1 now. 

#Generate prediction values: 
ypred0=coef(model0)[1]+coef(model0)[2]*data0$PCCR+coef(model0)[3]*data0$PRLZ+coef(model0)[4]*data0$INDUS+
  coef(model0)[5]*data0$NOX+coef(model0)[6]*data0$AVR+coef(model0)[7]*data0$AGE+coef(model0)[8]*data0$DIS+
  coef(model0)[9]*data0$RAD+coef(model0)[10]*data0$TAX

ypred1=coef(model1)[1]*data0$PCCR+coef(model1)[2]*data0$PRLZ+coef(model1)[3]*data0$NOX+
  coef(model1)[4]*data0$AVR+coef(model1)[5]*data0$AGE+coef(model1)[6]*data0$DIS+
  coef(model1)[7]*data0$RAD+coef(model1)[8]*data0$TAX

ggplot(data0,aes(x=AVR,y=ypred0))+
  geom_line(col='blue')+
  geom_line(aes(x=AVR,y=ypred1),col='red')

ggplot(data0,aes(x=AVR,y=MEDV))+
  geom_point()+
  geom_line(aes(x=AVR,y=ypred0),col='blue')+
  geom_line(aes(x=AVR,y=ypred1),col='red')

# Check the residuals
residuals(model1)
plot(residuals(model1),pch=16,col='blue',ylim=c(-30,50))
abline(a=-3*sd(residuals(model1)),b=0,col='red')
abline(a=3*sd(residuals(model1)),b=0,col='red')

plot(density(residuals(model1)))

