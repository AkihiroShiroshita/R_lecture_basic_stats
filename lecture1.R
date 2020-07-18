#######################
###Lecture 1###########
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
##Equal variance
tapply(hospitalterm, factor(hospital), mean)
tapply(hospitalterm, factor(hospital), sd)
plot.design(hospitalterm ~ factor(hospital))
fit4 <- lm(log(hospitalterm) ~ factor(hospital))
summary(fit4)
anova(fit4)
fit4_m <- lm(log(hospitalterm) ~ -1 + factor(hospital)) 
summary(fit4_m)
anova(fit4_m)
##Unequal variance 
#Welch's ANOVA
#Kruskal-Wallis test
oneway.test(log(hospitalterm) ~ factor(hospital))
coeftest(fit4, vcov = sandwich)
kruskal.test(log(hospitalterm) ~ factor(hospital))
##Multiple comparison
fit5 <- lm(log(hospitalterm) ~ -1 + factor(hospital))
m <- contrMat(table(hospital), type="Tukey") #define matrix of contrasts
m
mc <- glht(fit5, linfct = m) #general linear hypothesis
summary(mc, test=adjusted("fdr")) #false discovery rate: less conservative
#####Two-way ANOVA#####
par(mfrow = c(1,1))
plot.design(hospitalterm ~ factor(hospital) + factor(gender))
#without interaction
fit6 <- lm(log(hospitalterm) ~ factor(hospital) + factor(gender))
summary(fit6)
fit7 <- lm(log(hospitalterm) ~ factor(gender))
anova(fit6, fit7)
#with interaction
fit8 <- lm(log(hospitalterm) ~ factor(hospital) * factor(gender))
summary(fit8)
anova(fit6, fit8)
#then, calculate the main effects
######ANCOVA#####
fit9 <- lm(log(hospitalterm) ~ factor(hospital))
fit10 <- lm(log(hospitalterm) ~ factor(hospital) + age)
anova(fit9, fit10)
fit11 <- lm(log(hospitalterm) ~ factor(hospital) * age)
anova(fit9, fit10)
#test of coincident lines
anova(fit9, fit11)
#test of parallel lines
anova(fit10, fit11)
##Prediction
predict(fit10, new = data.frame(age = mean(age), hospital = 1), interval = "prediction")
predict(fit10, new = data.frame(age = mean(age), hospital = 2), interval = "prediction")
predict(fit10, new = data.frame(age = mean(age), hospital = 3), interval = "prediction")
predict(fit10, new = data.frame(age = mean(age), hospital = 4), interval = "prediction")
predict(fit10, new = data.frame(age = mean(age), hospital = 5), interval = "prediction")
#####Logistic regression#####
table(hospital, death)
##Without regression
chisq.test(hospital, death)
##logistic regression
#logit = log(odds)
fit11 <- glm(death ~ age, family = "binomial")
summary(fit11)
exp(fit11$coef)
exp(confint(fit11))
fit12 <- glm(death ~ age + factor(hospital), family = "binomial")
summary(fit12)
exp(fit12$coef)
exp(confint(fit12))
##likelihood ratio test
lrtest(fit11, fit12)
##Model checking

##Modified Poisson regression
#relative risk
fit13 <-glm(death ~ age + factor(hospital), family = "poisson")  
coeftest(fit13, vcov = sandwich)
exp(fit13$coef)
##Linear regression
#risk difference
fit14 <- glm(death ~ age + factor(hospital), family = "gaussian")
coeftest(fit14, vcov = sandwich)
