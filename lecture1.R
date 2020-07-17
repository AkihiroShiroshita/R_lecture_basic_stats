#######################
###Linear regression###
#######################
###Regression methods using R basic functions
df <- read.csv("stats_data.csv", head = T) #just for simplicity. Don't do this actually. 
#####Simple linear regression#################################
install.packages("multcomp")
install.packages("lmtest")
install.packages("sandwich")
library(multcomp)
library(lmtest)
library(sandwich)
plot(log(df$hospitalterm) ~ df$age)
lines(df$age, fit$fitted.values)
##Estimation
fit = lm(log(hospitalterm) ~ age, data = df)
summary(fit)
##Prediction of the mean vs Prediction of a new observation
##The prediction interval depends on the normally assumption.
predict(fit, newdata = data.frame(age = 50), interval = "confidence")
predict(fit, newdata = data.frame(age = 50), interval = "prediction")
##Model checking
plot(fit$fitted, fit$residuals)
abline(0,0)
qqnorm(fit$residuals)
qqline(fit$residuals)
dfb <- dfbeta(fit)
index = order(abs(dfb[,2]), decreasing=T)
cbind(dfb[index[1:15],], df$hospitalterm[index[1:15]], df$age[index[1:15]])
fit_r <- coeftest(fit, vcov = sandwich)
fit_r
