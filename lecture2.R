#######################
###Lecture 2###########
###Logistic regression#
#######################
###More advanced topics about logistic regression###
installed.packages("rms")
library(rms)
df2 <- read_csv("stats_data_2.csv", 
               locale = locale(encoding = "SHIFT-JIS"), 
               col_types = cols(
                 id = col_double(),
                 age = col_double(),
                 gender = col_double(),
                 steroid = col_double(),
                 hospitalterm = col_double(),
                 marker1 = col_double(),
                 marker2 = col_double(),
                 marker3 = col_factor(),
                 marker4 = col_factor(),
                 death = col_factor(),
                 hospital = col_factor()
               ),
               guess_max = 1500, #default: 1000
               na = c("NA", "NULL", ".", "999", "#VALUE!", "MA", "ND", "NE"))
df2 <- df2 %>% mutate_all(.funs = ~{as.numeric(.)})
df2 <- df2 %>%
  drop_na()
detach(df)
attach(df2) 
###Logistic regression with continuous predictors###
f1 <- lrm(death ~ age + gender + marker1, data = df2) #using rms package
print(f1) #check c-statistics, Dxy: Somner's = 2(AUC-0.5) 
f2 <- lrm(death ~ age + gender + marker1 + marker2, data = df2)
print(f2)
###Logistic regression with binary predictors###
f3 <- lrm(death ~ age + gender + marker3 + marker4, data = df2) 
print(f3) 
###Calibaration###
##Mean calibration
mean(df2$death) #not as a factor
fitted.risks <- predict(f3, df2, type="fitted")
mean(fitted.risks)
##Logistic calibration
f3 <- lrm(death ~ age + gender + marker3 + marker4, data = df2, x = TRUE, y = TRUE) 
set.seed(1234) #for bootstrapping
validate(f3, B=400) #check index.corrected -> high risks are overestimated and low risks are underestimated
##Calibration curve
val.prob(predict(f3, df2, type = "fitted"), df2$death, m=50)
#Is Hosmer-Lemeshow test is necessary? 
##Strong calibraition is quite important for limited numbers of predictors and they are all categorical.
###Discrimination
#ROC curve:AUC, no information about prevalence, tp or fp -> no way to determine an optimal cut-off
#IDI:Change in mean risk difference for two nested models 
#Above average risk difference: Net benefit metrics
##A risk model for recommended treatments


12:55 R demo

