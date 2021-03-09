#in this data set there is one dependent variable Price and
#multiple independent variables
nyc <- read.csv("nyc.csv")
View(nyc)

plot(nyc,main="Pairwise scatter plot")

#lm(dependent var~indep.var1+indep.var2)
#nycmod_1 <- lm(price~.,data= nyc) if u want to use all attributes
nycmod_1 <- lm(Price~Food+Decor+Service+East,data= nyc)
summary(nycmod_1)

#by looking at P value in summary it is clear that service does not have any impact on price
#so we will make model excluding service
nycmod_2 <- lm(Price~Food+Decor+East,data= nyc)
summary(nycmod_2)

#from the plot we understood that there is realtion betn food & service
#so now we will exclude service and include food in the model
nycmod_3 <- lm(Price~Service+Decor+East,data= nyc)
summary(nycmod_3)
#here R2 value has decreased so we can say that food is much more neatly explaining the 
#variation in the price than service

#Now we are checking if linear model is good or not
#by checking if estimate B1 is significant
alpha = 0.05 #95% confidence level
n = 168 #no of smamples
p = 1 #no of independent variable
critical_value = qt(p = 1 - (alpha/2),df=n-p-1) #here df is degree of freedom
#this will output critical value which is 1.974358 in this case

#now we have to found confidence level of b1^ upper bound and lower bound
#b1^-(critical value * std deviation associated)
#b1^+(critical value * std deviation associated)
summary(nycmod_2)
#for food
1.5363 - (critical_value * 0.2632)
1.5363 + (critical_value * 0.2632)
#the interval doesn't contain '0' So its significant

#for decor
1.9094 - (critical_value * 0.19)
1.9094 + (critical_value * 0.19)
#the interval doesn't contain '0' So its significant

#for East
2.067 - (critical_value * 0.9318)
1.9094 + (critical_value * 0.19)
#the interval doesn't contain '0' So its significant
#Therefore all the considered predictors do affect price

#handling the outliers
plot(nyc_mod$fitted.values,rstandard(nyc_mod),
     main="Residual plot",
     xlab = "predicted price",
     ylab = "standardized residuals")
abline(h=2,lty=2)
abline(h=-2,lty=2)

identify(nycmod_2$fitted.values,rstandard(nycmod_2))
#now remove the outliers one by one
nyc_new <- nyc[-56,]
nyc_mod <- lm(Price~Food+Decor+East,data=nyc_new)
summary(nyc_mod)
#after eliminating one outlier R2 is 0.6471

