#Assignments SLR
###################1st assignment##############################
calories<-read.csv(file.choose())

View(calories)
attach(calories)
plot(calories$Weight.gained..grams.,calories$Calories.Consumed)
cor(Calories.Consumed,Weight.gained..grams.)
reg<-lm(Calories.Consumed~Weight.gained..grams.)
summary(reg)

pred<-predict(reg)
summary(reg$residuals)
sqrt(sum(reg$residuals)^2/nrow(calories))#RMSE
sqrt(mean(reg$residuals)^2)
confint(reg,level=0.95)
predict(reg,interval="prediction")


ggplot(data=calories,aes(x=Weight.gained..grams.,y=Calories.Consumed)) +
  geom_point(color='red') + 
  geom_line(color='blue',data=calories,aes(x=Weight.gained..grams.,y=Calories.Consumed))
?ggplot2

#Residual standard error: 251.5 on 12 degrees of freedom
#Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
#F-statistic: 104.3 on 1 and 12 DF,  p-value: 2.856e-07

#Multiple R-squared:  0.8968 > 0.86
#Multiple R-squared value graterthan the 0.86 this is best model no need to for the transformation


####################2nd Assisgnment################################
#2) Delivery_time -> Predict delivery time using sorting time
delivery_time<-read.csv(file.choose())
View(delivery_time)
attach(delivery_time)
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,Sorting.Time)
mod<-lm(Sorting.Time ~ Delivery.Time)
summary(mod)
pred<-predict(mod)
summary(mod$residuals)
sqrt(sum(mod$residuals)^2/nrow(delivery_time))
sqrt(mean(mod$residuals)^2)
confint(mod,level=0.95)
predict(mod,interval="prediction")

#Residual standard error: 1.47 on 19 degrees of freedom
#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 
#F-statistic:  40.8 on 1 and 19 DF,  p-value: 3.983e-06
#Multiple R-squared:  0.6823<0.85 value is less go for transformations



#logthemic model

#x=log(deliverytime),y=sorting time
plot(log(Delivery.Time),Sorting.Time)
cor(log(Delivery.Time),Sorting.Time)
mod1<-lm(Sorting.Time~log(Delivery.Time))
summary(mod1)
predict(mod1)
mod1$residuals
sqrt(sum(mod1$residuals^2)/nrow(delivery_time))#RMSE
sqrt(mean(mod1$residuals^2))
confint(mod1,level = 0.95)
predict(mod1,interval = "confidence")

#Residual standard error: 1.402 on 19 degrees of freedom
#Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 
#F-statistic: 46.73 on 1 and 19 DF,  p-value: 1.593e-06
#Multiple R-squared:  0.7109<0.85 value is less go for exponentia

#Exponential model

#x=delivery time y=log(sorting time)
plot(Delivery.Time,log(Sorting.Time))
cor(Delivery.Time,log(Sorting.Time))
mod2<-lm(log(Sorting.Time)~Delivery.Time)
summary(mod2)
mod2$residuals

sqrt(mean(mod2$residuals^2))
logat<-predict(mod2)     
at<-exp(logat)
error<-delivery_time$Sorting.Time-at
error
sqrt(sum(mod2$residuals^2)/nrow((delivery_time)))#RMSE
confint(mod2,level = 0.95)
predict(mod2,interval = "confidence")
#Multiple R-squared:  0.6954,	Adjusted R-squared:  0.6794 
#go for quardratic model

#polynominal model with 2 degree(quadratic model)


plot(Delivery.Time,Sorting.Time)
plot(Delivery.Time*Delivery.Time,Sorting.Time)
plot(Delivery.Time*Delivery.Time,log(Sorting.Time))

cor(Delivery.Time*Delivery.Time,Sorting.Time)
cor(Delivery.Time*Delivery.Time,log(Sorting.Time))
mod3<-lm(log(Sorting.Time)~Delivery.Time + I(Delivery.Time*Delivery.Time))
summary(mod3)
mod3$residuals
logpol<-predict((mod3))
expy<-exp(logpol)  
err<-delivery_time$Sorting.Time - expy
err
sqrt(sum(mod3$residuals^2)/nrow(delivery_time))
confint(mod3,level=0.95)
predict(mod3,interval = "confidence")

#Multiple R-squared:  0.7937,	Adjusted R-squared:  0.7708 
#multiple r -squared value is near by 0.8 this is the best model

######################3rd Assignment#########################
##Emp_data -> Build a prediction model for Churn_out_rate 

empdata<-read.csv(file.choose())
View(empdata)
attach(empdata)
plot(Salary_hike,Churn_out_rate)
cor(Salary_hike,Churn_out_rate)
emp1<-lm(Churn_out_rate ~ Salary_hike)
summary(emp1)
pred<-predict(emp1)
summary(emp1$residuals)
sqrt(sum(emp1$residuals)^2/nrow(empdata))
sqrt(mean(emp1$residuals)^2)
confint(emp1,level=0.95)
predict(emp1,interval="prediction")

#Residual standard error: 4.469 on 8 degrees of freedom
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 
#F-statistic:  39.4 on 1 and 8 DF,  p-value: 0.0002386

#logthemic model

#x=log(salary hike),y=Churn_out_rate
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
emp2<-lm(Churn_out_rate~log(Salary_hike))
summary(emp2)
predict(emp2)
emp2$residuals
sqrt(sum(emp2$residuals^2)/nrow(empdata))#RMSE
sqrt(mean(emp2$residuals^2))
confint(emp2,level = 0.95)
predict(emp2,interval = "confidence")

#Residual standard error: 4.233 on 8 degrees of freedom
#Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 
#F-statistic: 44.85 on 1 and 8 DF,  p-value: 0.0001532
#MR-squred value is near by 0.86.this is the best model


###Salary_hike -> Build a prediction model for Salary_hike

sal_hike<-read.csv(file.choose())
View(sal_hike)
attach(sal_hike)
plot(YearsExperience,Salary)
cor(YearsExperience,Salary)
sal1<-lm(Salary ~ YearsExperience)
summary(sal1)
pred<-predict(sal1)
summary(sal1$residuals)
sqrt(sum(sal1$residuals)^2/nrow(sal_hike))
sqrt(mean(sal1$residuals)^2)
confint(sal1,level=0.95)
predict(sal1,interval="prediction")

#Residual standard error: 5788 on 28 degrees of freedom
#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554 
#F-statistic: 622.5 on 1 and 28 DF,  p-value: < 2.2e-16

#multiple R-squared value is 0.957>0.85 this is best model
