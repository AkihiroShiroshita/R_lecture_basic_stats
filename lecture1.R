#######################
###Linear regression###
#######################
###Regression methods using R basic functions###
df <- read.csv("stats_data.csv", head = T) #just for simplicity. Don't do this actually. 
df <- na.omit(df)
attach(df)
install.packages("multcomp")
install.packages("lmtest")
install.packages("sandwich")
library(multcomp)
library(lmtest)
library(sandwich)
hist(hospitalterm)
hist(log(hospitalterm))
#####Simple linear regression######
plot(log(hospitalterm) ~ age)
lines(age, fit$fitted.values)
##Estimation
fit <- lm(log(hospitalterm) ~ age)
summary(fit)
summary(fit)$r.squared
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
cbind(dfb[index[1:15],], hospitalterm[index[1:15]], age[index[1:15]])
fit_r <- coeftest(fit, vcov = sandwich)
fit_r
#####Multiple linear regression#####
##Estimation
par(mfrow = c(1,1))
plot(age[gender == 1], log(hospitalterm)[gender == 1],xlim=c(40,100), ylim =c(0,10),  
     pch = 1, col = 75, xlab = "Age", ylab = "Length of hospital stay")
points(age[gender == 2], log(hospitalterm)[gender == 2], pch = 1, col = 34)
fit2 <- lm(log(hospitalterm) ~ age + gender)
summary(fit2)
plot(fit2$fitted, fit2$residuals, xlab = "Fitted values", ylab = "Residuals")
abline(0,0)
qqnorm(fit2$residuals)
qqline(fit2$residuals)
dfb2 <- dfbeta(fit2)
index = order(abs(dfb2[,2]), decreasing = T)
cbind(dfb2[index[1:15],], age[index[1:15]], gender[index[1:15]])
index = order(abs(dfb2[,3]), decreasing = T)
cbind(dfb2[index[1:15],], age[index[1:15]], gender[index[1:15]])
anova(fit, fit2)
fit.r2 <- coeftest(fit2, vcov = sandwich)
fit.r2
##Prediction
predict(fit2, newdata = data.frame(age = 50, gender = 1), interval = "confidence")
predict(fit2, newdata = data.frame(age = 50, gender = 1), interval = "prediction")
##Interaction
fit3 <- lm(log(hospitalterm) ~ age * gender) #":", only interaction term
summary(fit3)
anova(fit2, fit3)
#####One-way ANOVA######
tapply(hospitalterm, factor(hospital), mean)
tapply(hospitalterm, factor(hospital), sd)
plot.design(hospitalterm ~ factor(hospital))
fit4 <- lm(hospitalterm ~ factor(hospital))
summary(fit4)
anova(fit4)
fit4_m <- lm(hospitalterm ~ -1 + factor(hospital)) 
summary(fit4_m)
anova(fit4_m)
