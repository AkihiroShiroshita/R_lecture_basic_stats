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
###A risk model for recommended treatments
baseline.model <- decision_curve(lk ~ age + gender + marker3, #fitting a logistic model
                                 data = df2,
                                 study.design = "cohort",
                                 bootstraps = 50)
plot_decision_curve(baseline.model,  curve.names = "baseline model")
summary(baseline.model)
#Net Benefit rather than standardized Net Benefit
plot_decision_curve(baseline.model, standardize=F, curve.names = "baseline model")
#Examine the potential for the new biomarker to improve Net Benefit
full.model <- decision_curve(lk ~ age + gender + marker3 + marker4, data = df2, bootstraps = 50)
plot_decision_curve( list(baseline.model, full.model),  curve.names = c("Baseline model", "Full model"))


# If the standard is biopsy, then "opt-out" decision curve is more useful
baseline.model.optout <- decision_curve(Cancer~Age + Female + Smokes + Marker1, #fitting a logistic model
                                        data = dcaData,
                                        study.design = "cohort",
                                        policy = "opt-out",
                                        bootstraps = 50)
plot_decision_curve(baseline.model.optout, curve.names = "baseline model", xlim=c(0, .15), ylim=c(0, 0.6))


# in the Opt-Out formulation, examine the evidence for the new marker
full.model.optout <- decision_curve(Cancer~Age + Female + Smokes + Marker1 + Marker2, policy="opt-out", data = dcaData, bootstraps = 50)
plot_decision_curve( list(baseline.model.optout, full.model.optout), curve.names = c("baseline", "full"), xlim=c(0, .15), legend.position="topleft")
# Plot ROC alternative for full model that includes biomarkers
plot_roc_components(full.model, col = c("black", "red"))
# Clinical Impact Plot for full model that includes biomarkers
plot_clinical_impact(full.model, xlim = c(0, .2), col = c("black", "blue"))