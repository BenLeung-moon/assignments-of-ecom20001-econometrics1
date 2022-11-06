#*******************************************************************************
#Assignment 1 Code
#By: Haohong Liang, Yanjun Tao, Yifan Zhao
#*******************************************************************************

#*******************************************************************************
# SET WORKING DIRECTIORY AND LOAD DATA

## Set the working directory for the assignment file
setwd("C:\Users\Ecom1\Ass1\Assignment1")

## Load Stargazer package for summary statistics and regression tables
library(stargazer)


## Load the dataset from a comma separate value
data = read.csv(file = "as1_beer.csv")

## List the variables in the dataset named data
names(data)

## Dimension of the dataset
# 1,134 observations (states in a certain year)
# 5 variables: state, year, beercons, beertax, cigtax
dim(data)

# Variable descriptions|
#state: US state
#year: sample year
#beercons: per capita beer sales in terms of gallons of ethanol equivalent
#beertax: beer tax in terms of inflation-adjusted (real) dollars per gallon
#cigtax: cigarette tax in terms of inflation-adjusted (real) dollars per pack


#*******************************************************************************
# QUESTION 1:
## Summary of beercons, beertax, and cigtax
#3 and 5 is the index from beercons to cigtax
stargazer(data[3:5],
          summary.stat = c("n", "mean", "sd", "min", "max"),  
          type="text", 
          title="Descriptive Statistics of beercons, beertax, And cigtax",
          out="sumstats.txt")


#Using histogram to find extremes
#beercons
png("density_of_beercons.png")
hist(data$beercons,
     main = "Density of Per Capita Beer Sales",
     xlab = "Per Capita Beer Sales in Gallons",
     xlim = range(0.5:3),
     ylab = "Frequency",
     col = "orange",
     pch = 16)
dev.off()

#beertax
png("density_of_beertax.png")
hist(data$beertax,
     main = "Density of Beer Tax in Real Dollar per Gallons",
     xlab = "Real Dollar per Gallons",
     ylab = "Frequency",
     col = "orange",
     pch = 16)
dev.off()

#cigtax
png("density_of_cigtax.png")
hist(data$cigtax,
     main = "Density of Cigarette Tax in Real Dollar per Pack",
     xlab = "Real Dollar per Pack",
     ylab = "Frequency",
     col = "orange",
     pch = 16)
dev.off()


#*******************************************************************************
#QUESTION2:

##Compute the 95% confidence intervals
#beercons
#t.test(data$beercons, conf.level = 0.95)
mu_beercons = mean(data$beercons)                # Sample mean of beercons
nobs_beercons = length(data$beercons)            # Number of observations
sd_beercons = sd(data$beercons)                  # Sample standard deviation of beercons
se_beercons = sd_beercons/sqrt(nobs_beercons)    # Standard error of the sample mean
CI95_low_beercons = mu_beercons-1.96*se_beercons # Lower bound of the 95% CI
CI95_high_beercons = mu_beercons+1.96*se_beercons# Upper bound of the 95% CI

#beertax
#t.test(data$beertax, conf.level = 0.95)
mu_beertax = mean(data$beertax)               # Sample mean of beertax
nobs_beertax = length(data$beertax)           # Number of observations
sd_beertax = sd(data$beertax)                 # Sample standard deviation of beertax
se_beertax = sd_beertax/sqrt(nobs_beertax)    # Standard error of the sample mean
CI95_low_beertax = mu_beertax-1.96*se_beertax # Lower bound of the 95% CI
CI95_high_beertax = mu_beertax+1.96*se_beertax# Upper bound of the 95% CI


#cigtax
#t.test(data$cigtax, conf.level = 0.95)
mu_cigtax = mean(data$cigtax)                 # Sample mean of cigtax
nobs_cigtax = length(data$cigtax)             # Number of observations
sd_cigtax = sd(data$cigtax)                   # Sample standard deviation of cigtax
se_cigtax = sd_cigtax/sqrt(nobs_cigtax)       # Standard error of the sample mean
CI95_low_cigtax = mu_cigtax-1.96*se_cigtax    # Lower bound of the 95% CI
CI95_high_cigtax = mu_cigtax+1.96*se_cigtax   # Upper bound of the 95% CI


#*******************************************************************************
#QUESTION3:

#Detect each states whether is a high tax state
#New Variable hightax, if a state is hightax, hightax=1, otherwise, 0.

#Create high tax threshold variable
taxMedian = median(data$beertax)

#Detecting high tax states
data$hightax[data$beertax >= taxMedian] = 1
data$hightax[data$beertax < taxMedian] = 0

#Density graphs for hightax=1 and hightax=0
png("Consumption in Different Tax Policy.png")
plot(density(data$beercons[data$hightax==0]), col="blue",
     lty=1,
     main="Beer Consumption in Different Tax Policy", 
     xlab="Per Capita Beer Sales in Gallons")
lines(density(data$beercons[data$hightax==1]), 
      col="red",
      lty=1)
legend("topright", legend=c("High Beer Tax State", "Low Beer Tax State"), 
       col=c("red","blue"), lty=c(1,1))
dev.off()


#statistics in different tax policy
mean(data$beercons[data$hightax==1])
mean(data$beercons[data$hightax==0])


#*******************************************************************************
#QUESTION4
#Create Variables to Store Data
highTax = data$beercons[data$hightax==1]
lowTax = data$beercons[data$hightax==0]
highTaxMean = mean(data$beercons[data$hightax==1])
lowTaxMean = mean(data$beercons[data$hightax==0])

#Conduct T test
t.test(highTax, lowTax)

#difference means
meanDiffereceLowAndHigh = lowTaxMean - highTaxMean

#Calculate change in percent from low tax to high tax
percentChangeInHighTax = (highTaxMean - lowTaxMean) / lowTaxMean
#round to three decimal                              
percentChangeInHighTax = round(percentChangeInHighTax, digits = 3)
#output
paste("the per cent change in the conditional mean from hightax = 0 to hightax = 1 is"
      , percentChangeInHighTax) 


#*******************************************************************************
#QUESTION5

#construct linear regression between beertax and beercons
beer_reg1 = lm(beercons~beertax, data = data)

#Construct the scatter plot and abline between beertax and beercons
png("plot_with_beertax_and_beercons.png")
plot(data$beertax, data$beercons,
     main = "Relationship Between Beer Tax and Consumption",
     xlab = "Beer Tax in Real Dollar per Gallons",
     ylab = "Per Capita Beer Sales in Gallons",
     col = "orange",
     pch=20)
abline(beer_reg1, col="red", lwd=3)
dev.off()

#correlation efficient between beertax and beercons
corr_effi_cons_tax = round(cor(x = data$beercons, y = data$beertax)
                           , digit = 3)

paste("The correlation coefficient between beertax and beercons is",
      corr_effi_cons_tax)


#*******************************************************************************
#QUESTION6

#Construct linear regression
reg1 = lm(beercons~beertax, data = data)
reg2 = lm(beercons~cigtax, data = data)

#Output regression outcome
stargazer(reg1, reg2, type="text",
          digits=3, 
          dep.var.labels=c("Per Capita Beer Sales in Gallons"),
          covariate.labels=
            c("Beer Tax",
              "Cigarette Tax in Real Dollar",
              "Intercept"),
          out="reg_output.txt")

##Predicted Change in beercons with one-standard-deviation increase 
##and corresponding statistical tests
mu = 0 #test is different from 0

#beertax
#predicted change
beertax_beta = coef(summary(reg1))[, "Estimate"][2]
beertax_se = coef(summary(reg1))[, "Std. Error"][2]
beertax_sd = sd(data$beertax)
one_sd_change_beertax = beertax_beta * beertax_sd

#t test for whether predicted change is statistically significant
t_stat_beertax = ((beertax_beta - mu) * beertax_sd) / (beertax_se * beertax_sd)
beertax_pvalue = 2 * pnorm(-abs(t_stat_beertax))

#confidence interval
confint(reg1) * beertax_sd

#cigtax
#predicted change
cigtax_beta = coef(summary(reg2))[, "Estimate"][2]
cigtax_se = coef(summary(reg2))[, "Std. Error"][2]
cigtax_sd = sd(data$cigtax)
one_sd_change_cigtax = cigtax_beta * cigtax_sd

#t test for whether predicted change is statistically significant
t_stat_cigtax = ((cigtax_beta - mu) * cigtax_sd) / (cigtax_se * cigtax_sd)
cigtax_pvalue = 2 * pnorm(-abs(t_stat_cigtax))

#confidence interval
confint(reg2) * cigtax_sd


#*******************************************************************************
#QUESTION7
##Scattor Plots of beercons, beertax and cigtax
#beertax and beercons
png("Relationship between cigtax and beertax.png")
plot(data$cigtax, data$beertax,
     main="Relationship between cigtax and beertax",
     xlab="Cigarette Tax in Real Dollar per Pack",
     ylab="Beer Tax in Real Dollar per Gallons",
     col="orange",
     pch = 20)
abline(lm(beertax~cigtax, data = data), col="blue", lwd=2)
dev.off()

#cigtax and beercons
png("Relationship between cigtax and beercons.png")
plot(data$cigtax, data$beercons,
     main="Relationship between cigtax and beercons",
     xlab="Cigarette Tax in Real Dollar per Pack",
     ylab="Per Capita Beer Sales in Gallons",
     col="forestgreen",
     pch = 20)
abline(lm(beercons~cigtax, data = data), col="blue", lwd=2)
dev.off()

##Comparison between Reg3 and Reg1
beta1_reg1 = coef(summary(reg1))[,"Estimate"][2]
beta1_reg3 = -0.422

value_spread_of_reg1_reg3 = beta1_reg3 - beta1_reg1
percentage_spread_of_reg1_reg3 = (beta1_reg3 - beta1_reg1) / beta1_reg1
direction_of_reg1_reg3 = sign(beta1_reg1) * sign(beta1_reg3)

#*******************************************************************************
#QUESTION8
#Construct linear regression
#sub-sample1: year <= 1994
reg1_before1994 = lm(beercons[year<=1994]~beertax[year<=1994], data = data)

#sub-sample2: year > 1994
reg1_after1994 = lm(beercons[year>1994]~beertax[year>1994], data = data)

#Output regression outcome
stargazer(reg1_before1994, reg1_after1994, type="text",
          digits=3, 
          dep.var.labels=c("Per Capita Beer Sales in Gallons",""),
          covariate.labels=
            c("Beer Tax Before 1994",
              "Beer Tax After 1994",
              "Intercept"),
          out="reg_1994_output.txt")

#Corresponding Coefficient Change
#Original Regression
beta_allYear = coef(summary(reg1))[, "Estimate"][2]
se_allYear = coef(summary(reg1))[, "Std. Error"][2]

#Before 1994
beta_before1994 = coef(summary(reg1_before1994))[, "Estimate"][2]
se_before1994 = coef(summary(reg1_before1994))[, "Std. Error"][2]

#T Test whether before 1994 period's beertax is zero
t.test(data$beertax[data$year<=1994])

#After 1994
beta_after1994 = coef(summary(reg1_after1994))[, "Estimate"][2]
se_after1994 = coef(summary(reg1_after1994))[, "Std. Error"][2]

#T Test of whether after 1994 from beertax is zero
t.test(data$beertax[data$year>1994])

#Change Magnitude from before 1994 and after 1994
beta_change = (beta_after1994 - beta_before1994) / beta_before1994
se_change = (se_after1994 - se_before1994) / se_before1994






















