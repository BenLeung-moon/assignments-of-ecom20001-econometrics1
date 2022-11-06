#*******************************************************************************
#Assignment 2 Code
#By: Haohong Liang, Yanjun Tao, Yifan Zhao
#*******************************************************************************

#*******************************************************************************
# SET WORKING DIRECTIORY AND LOAD DATA

## Set the working directory for the assignment file
setwd("C:\Users\Ecom1\Ass2\Assignment3")

## Load Stargazer package for summary statistics and regression tables
library(stargazer)
library(AER)
library(ggplot2)

## Load the dataset from a comma separate value
data = read.csv(file = "as2_sleep.csv")

## List the variables in the dataset named data
names(data)

## Dimension of the dataset
# 700 observations
# 13 variables: 
# id, sleep, nap, totwrk, educ, age, gdhlth, smsa, union, selfe  
# marr, yrsmarr, yngkid
dim(data)

# Variable descriptions|
# id: individual identifier
# sleep: number of minutes sleep (overnight) per week
# naps: number of minutes napping per week
# totwrk: number of minutes working per week
# educ: number of years of educational attainment
# age: age
# gdhlth: equals 1 if self-reported health is “Excellent??? or “Good???, 0 otherwise
# smsa: equals 1 if lives in a US urban area (SMSA), 0 otherwise
# union: equals 1 if part of a union, 0 otherwise
# selfe: equals 1 if self-employed, 0 otherwise
# marr: equals 1 if married, 0 otherwise
# yrsmarr: number of years married
# yngkid: equals 1 if they have a young child less than 3 at home, 0 otherwise
# male: equals 1 if male, 0 otherwise


#*******************************************************************************
# QUESTION1:
## Summary of all variables
stargazer(data,
          summary.stat = c("n", "mean", "sd", "min", "max"),  
          type="text", 
          title="Summary Descriptive Statistics",
          out="sumstats.txt")

#*******************************************************************************
# QUESTION2:
# Construct regressions
reg_sleepVSeduc = lm(sleep~educ, data = data)
reg_sleepVSage = lm(sleep~age, data = data)
reg_educVSage = lm(educ~age, data = data)

# constrcut scatter plot for sleep vs educ
png("plot_with_educ_and_sleep.png")
plot(data$educ, data$sleep,
     main = "Relationship Between Years of Education and Sleep Time",
     xlab = "Number of Years of Educational Attainment",
     ylab = "Number of Minutes Sleep (overnight) Per Week",
     col = "cornflowerblue",
     pch=20)
abline(reg_sleepVSeduc, col="coral", lwd=3)
dev.off()

# construct scatter plot for sleep vs age
png("plot_with_age_and_sleep.png")
plot(data$age, data$sleep,
     main = "Relationship Between Age and Sleep Time",
     xlab = "Year of Age",
     ylab = "Number of Minutes Sleep (overnight) Per Week",
     col = "cornflowerblue",
     pch=20)
abline(reg_sleepVSage, col="springgreen4", lwd=3)
dev.off()

# construct scatter plot for educ and age
png("plot_with_educ_and_age.png")
plot(data$age, data$educ,
     main = "Relationship Between Age Year and Years of Education",
     xlab = "Year of Age",
     ylab = "Number of Years of Educational Attainment",
     col = "cornflowerblue",
     pch=20)
abline(reg_educVSage, col="tan3", lwd=3)
dev.off()


#*******************************************************************************
# QUESTION3:
# construct regression of sleep vs educ and age
reg_sleep_vs_educ_age = lm(sleep~educ+age, data = data)
summary(reg_sleep_vs_educ_age)
summary(reg_sleepVSeduc)
educ_with_ovb = coef(summary(reg_sleepVSeduc))[, "Estimate"][2]
educ_without_ovb =coef(summary(reg_sleep_vs_educ_age))[, "Estimate"][2]


#*******************************************************************************
# QUESTION4
# construct regressions and save each robust standard errors
# Single linear regression with no other controls
reg1 = lm(sleep~educ, data = data)
cov1=vcovHC(reg1, type = "HC1")           
se1=sqrt(diag(cov1))

# Controlling for age
reg2 = lm(sleep~educ+age, data = data)
cov2=vcovHC(reg2, type = "HC1")           
se2=sqrt(diag(cov2))

# controlling for self-reported health
reg3 = lm(sleep~educ+age+gdhlth, data = data)
cov3=vcovHC(reg3, type = "HC1")           
se3=sqrt(diag(cov3))

# controlling US urban area, part of union and self-employed
reg4 = lm(sleep~educ+age+gdhlth+smsa+union+selfe, data = data)
cov4=vcovHC(reg4, type = "HC1")           
se4=sqrt(diag(cov4))

# controlling married, number of married year and have a young child less than 3 at home
reg5 = lm(sleep~educ+age+gdhlth+smsa+union+selfe+marr+yrsmarr+yngkid, data = data)
cov5=vcovHC(reg5, type = "HC1")           
se5=sqrt(diag(cov5))

# output text file
stargazer(reg1, reg2, reg3, reg4, reg5, type="text",
          se=list(se1, se2, se3, se4, se5),
          digits=3, 
          dep.var.labels=c("Number of Minutes Sleep (overnight) Per Week"),
          covariate.labels=
            c("Number of Years of Educational Attainment",
              "Age",
              "Self-Reported Health",
              "Lives in a US Urban Area",
              "Part of a Union",
              "Self-Employed",
              "Married",
              "Years of Married",
              "Have a Young Child Less Than 3 at Home"),
          out="Q4_reg_output.txt")


#*******************************************************************************
# QUESTION5
# (D)
#3-units change in sleep when educ increase by 3
predicted_change_educ_increase_three = 3 * coef(summary(reg5))[,"Estimate"][2]
CI99_lower_bound = 3 * (coef(summary(reg5))[,"Estimate"][2] 
                        - 2.58*se5[2])
CI99_upper_bound = 3 * (coef(summary(reg5))[,"Estimate"][2] 
                        + 2.58*se5[2])
sprintf("99%% confidence interval for 3-unit change is [%.3f, %.3f]", 
        CI99_lower_bound, CI99_upper_bound)

# (E)
# overall regression F statistics
waldtest(reg5, vcov = vcovHC(reg5, "HC1"))


#*******************************************************************************
# QUESTION6
# construct two regression whose interest of variable is sleep and nap
# and save each robust standard errors
reg_sleep = lm(sleep~educ+age+gdhlth+smsa+union+selfe+marr+yrsmarr+yngkid, data = data)
cov_sleep=vcovHC(reg_sleep, type = "HC1")           
se_sleep=sqrt(diag(cov_sleep))

reg_nap = lm(nap~educ+age+gdhlth+smsa+union+selfe+marr+yrsmarr+yngkid, data = data)
cov_nap=vcovHC(reg_nap, type = "HC1")           
se_nap=sqrt(diag(cov_nap))

# output report txt file
stargazer(reg_sleep,reg_nap, type="text",
          se=list(se_sleep, se_nap),
          digits=3, 
          dep.var.labels=c("Sleep", "Nap"),
          covariate.labels=
            c("Number of Years of Educational Attainment",
              "Age",
              "Self-Reported Health",
              "Lives in a US Urban Area",
              "Part of a Union",
              "Self-Employed",
              "Married",
              "Years of Married",
              "Have a Young Child Less Than 3 at Home"),
          out="Q6_reg_output.txt")


# calculate F statistics of two regression
# reg_sleep
linearHypothesis(reg_sleep, c("educ=0"), vcov = vcovHC(reg_sleep, type = "HC1"))

# reg_nap
linearHypothesis(reg_nap, c("educ=0"), vcov = vcovHC(reg_nap, type = "HC1"))


#*******************************************************************************
# QUESTION7
# construct regressions and save each robust standard errors
# entire sample regression
reg_Q7_entire = lm(sleep~totwrk+educ+age+gdhlth+smsa+union+selfe+marr+yrsmarr+yngkid, 
            data = data)
cov_Q7_entire=vcovHC(reg_Q7_entire, type = "HC1")           
se_Q7_entire=sqrt(diag(cov_Q7_entire))

# males only sample regression
reg_Q7_male_only = lm(sleep~totwrk+educ+age+gdhlth+smsa+union+selfe+marr+yrsmarr+yngkid, 
                   data = data[which(data$male==1),])
cov_Q7_male_only=vcovHC(reg_Q7_male_only, type = "HC1")           
se_Q7_male_only=sqrt(diag(cov_Q7_male_only))

# females only sample regression
reg_Q7_female_only = lm(sleep~totwrk+educ+age+gdhlth+smsa+union+selfe+marr+yrsmarr+yngkid, 
                      data = data[which(data$male==0),])
cov_Q7_female_only=vcovHC(reg_Q7_female_only, type = "HC1")           
se_Q7_female_only=sqrt(diag(cov_Q7_female_only))

# output text file
stargazer(reg_Q7_entire, reg_Q7_male_only, reg_Q7_female_only, type="text",
          se=list(se_Q7_entire, se_Q7_male_only, se_Q7_female_only),
          digits=3, 
          dep.var.labels=c("Number of Minutes Sleep (overnight) Per Week for Entire Sample, Male, and Female"),
          covariate.labels=
            c("Number of Minutes Working Per Week",
              "Number of Years of Educational Attainment",
              "Age",
              "Self-Reported Health",
              "Lives in a US Urban Area",
              "Part of a Union",
              "Self-Employed",
              "Married",
              "Years of Married"),
          out="Q7_reg_output.txt")


#*******************************************************************************
# QUESTION8
## (A)
# calculate F statistics of Reg(1) about totwrk=0
linearHypothesis(reg_Q7_entire, 
                 c("totwrk=0"), 
                 vcov = vcovHC(reg_Q7_entire, type = "HC1"))
# predicated cahnge in sleep from working one more hour per day for five days
increment_num_of_min_working = 5 * 1 * 60
predicted_change_on_sleep_with_work_increment = 
  coef(summary(reg_Q7_entire))[,"Estimate"][2] * increment_num_of_min_working
sprintf("predicted change is %.3f"
        , predicted_change_on_sleep_with_work_increment)

## (B)
# F statistics of Reg(1) about marr=yngkid
linearHypothesis(reg_Q7_entire, 
                 c("marr=yngkid"), 
                 vcov = vcovHC(reg_Q7_entire, type = "HC1"))
## (C)
# calculate F statistics of Reg(2) and Reg(3) about totwrk 
# and predicted change in sleep from working one more hour per day for five days
# within a given week

#Reg(2)
linearHypothesis(reg_Q7_male_only, 
                 c("totwrk=0"), 
                 vcov = vcovHC(reg_Q7_male_only, type = "HC1"))
predicted_change_in_male = coef(summary(reg_Q7_male_only))[,"Estimate"][2] * increment_num_of_min_working

#Reg(3)
linearHypothesis(reg_Q7_female_only, 
                 c("totwrk=0"), 
                 vcov = vcovHC(reg_Q7_female_only, type = "HC1"))
predicted_change_in_female = coef(summary(reg_Q7_female_only))[,"Estimate"][2] * increment_num_of_min_working


data$log_totwrk = log(1+data$totwrk)
data$age_power_2 = data$age*data$age
data$age_power_3 = data$age*data$age*data$age
data$age_power_4 = data$age*data$age*data$age*data$age

reg1 = lm(log_totwrk~age, data = data)
cov_reg1=vcovHC(reg1, type = "HC1")           
se_reg1=sqrt(diag(cov_reg1))

reg2 = lm(log_totwrk~age_power_2, data = data)
cov_reg2=vcovHC(reg2, type = "HC1")           
se_reg2=sqrt(diag(cov_reg2))

reg3 = lm(log_totwrk~age_power_3, data = data)
cov_reg3=vcovHC(reg3, type = "HC1")           
se_reg3=sqrt(diag(cov_reg3))

reg4 = lm(log_totwrk~age_power_4, data = data)
cov_reg4=vcovHC(reg4, type = "HC1")           
se_reg4=sqrt(diag(cov_reg4))

stargazer(reg1, reg2, reg3, reg4, type="text",
          se=list(se_reg1, se_reg2, se_reg3, se_reg4),
          digits=3 )


reg5 = lm(log_totwrk~age+age_power_2+age_power_3+age_power_4, data = data)
cov_reg5=vcovHC(reg5, type = "HC1")
se_reg5=sqrt(diag(cov_reg5))
stargazer(reg5, type="text",se=list(se_reg5), digits = 3)
