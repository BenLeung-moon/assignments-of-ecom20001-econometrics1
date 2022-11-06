#*******************************************************************************
#Assignment 3 Code
#By: Haohong Liang, Yanjun Tao, Yifan Zhao
#*******************************************************************************

#*******************************************************************************
# SET WORKING DIRECTIORY AND LOAD DATA

## Set the working directory for the assignment file
#setwd("C:\Users\Ecom1\Ass3\Assignment3")

## Load Stargazer package for summary statistics and regression tables
library(stargazer)
library(AER)
library(ggplot2)

## Load the dataset from a comma separate value
data = read.csv(file = "as3_wine.csv")

## List the variables in the dataset named data
names(data)

## Dimension of the dataset
# 6979 observations
# 28 variables: 
# price, cases, score, age
## region dummy variable:
# napa, bayarea, sonoma, scoast, carn, sierra, mendo, wash, othloc
## type dummy vairables:
# pinotnoir, cabernet, merlot, syrah, nonvarietal
## year dummy variable:
# d1990 ... d1999
dim(data)

# Variable descriptions|
# price: price of a bottle of the wine
# cases: number of cases of the wine produced
# score: WSM wine tasting score from 0 to 100, with 100 being the best taste
# age: how old the wine is in years
## Wine region dummy variables: for 9 different wine-producing regions in
# California and Washington State in the United States, equalling one if the wine
# is from the region and zero otherwise
## Wine type dummy variables:  for 5 different wine types, equalling one if the wine
# is a particular type and zero otherwise
## Year dummy variables: for 10 different years (1990 to 1999), equalling one if the
# wine is produced in a particular year and zero otherwise

data$score = as.numeric(unlist(data$score))

#*******************************************************************************
# QUESTION1:
# produce scatter plot for price and  score
png("scatter_plot_with_price_and_score.png")
ggplot(data, aes(y=price, x=score)) + 
         geom_point(alpha = 0.3) + 
         stat_smooth(method = "lm", formula = y ~ poly(x, 3), col="blue") + 
         ggtitle("Relationship Between Price and Score") + 
         theme(plot.title = element_text(hjust = 0.5)) + 
         scale_x_continuous(name="score") + 
         scale_y_continuous(name="price")
dev.off()



#*******************************************************************************
# QUESTION2:
# insert quadratic and cubic data
data$score_sq = data$score*data$score
data$score_cu = data$score*data$score*data$score

# construct linear, quadratic, and cubic regression
reg_linear = lm(price~score, data = data)
se_linear = sqrt(diag(vcovHC(reg_linear, type = "HC1")))

reg_sq = lm(price~score_sq+score, data = data)
se_sq = sqrt(diag(vcovHC(reg_sq, type = "HC1")))

reg_cu = lm(price~score_cu+score_sq+score, data = data)
se_cu = sqrt(diag(vcovHC(reg_cu, type = "HC1")))

#output the result of each regression
stargazer(reg_linear, reg_sq, reg_cu, type="text",
          se = list(se_linear, se_sq, se_cu),
          digits = 2,
          dep.var.labels = c("Price of a Bottle of the Wine"),
          out="sequential hypothesis testing.txt")


#*******************************************************************************
# QUESTION3:
# Construct three regressions
reg_col1 = reg_sq
se_col1 = se_sq

reg_col2 = lm(price~score_sq+score+pinotnoir+cabernet+merlot+syrah, 
              data = data)
se_col2 = sqrt(diag(vcovHC(reg_col2, type = "HC1")))

reg_col3 = lm(price~score_sq+score+pinotnoir+cabernet+merlot+syrah
              +napa+bayarea+sonoma+scoast+carn+sierra+mendo+wash
              +d1991+d1992+d1993+d1994+d1995+d1996+d1997+d1998+d1999, 
              data = data)
se_col3 = sqrt(diag(vcovHC(reg_col3, type = "HC1")))

stargazer(reg_col1,reg_col2,reg_col3,type="text",
          se=list(se_col1, se_col2,se_col3),
          digits=3, 
          dep.var.labels=c("Price of a Bottle of the Wine"),
          omit=c("napa",
                 "bayarea",
                 "sonoma",
                 "scoast",
                 "carn",
                 "sierra",
                 "mendo",
                 "wash",
                 "othloc",
                 "d1990","d1991","d1992","d1993","d1994",
                 "d1995","d1996","d1997","d1998","d1999"),
          out="Q3_reg_output.txt")


#*******************************************************************************
# QUESTION4
# test hypothesis score_sq whether is 0
linearHypothesis(reg_col3,c("score_sq=0"),vcov = vcovHC(reg_col3, "HC1"))


#*******************************************************************************
# QUESTION5
linearHypothesis(reg_col3,c("pinotnoir=cabernet","cabernet=merlot", "merlot=syrah"),vcov = vcovHC(reg_col3, "HC1"))


#*******************************************************************************
# QUESTION6
# Partial Effects of score increasing from 80 to 85
newdata1=data.frame(score=80,score_sq=80*80)
newdata2=data.frame(score=85,score_sq=85*85)

partial_effect_80_to_85 = predict(reg_sq, newdata=newdata2) - predict(reg_sq, newdata=newdata1)

# Fstatistic for Test of Difference in Score
Ftest=linearHypothesis(reg_sq,c("5*score+825*score_sq=0"),vcov = vcovHC(reg_sq, "HC1"))
Fstat=Ftest[2,3]

# calculate standard error
se_80_to_85 = abs(partial_effect_80_to_85)/sqrt(Fstat)

# 95% CI
score_80_to_85_ci95L = partial_effect_80_to_85 - se_80_to_85*1.96
score_80_to_85_ci95H = partial_effect_80_to_85 + se_80_to_85*1.96

# Outputting results
sprintf("partial effect of score increasing from 80 to 85: %f", partial_effect_80_to_85)
sprintf("SE of partial effect of increasing from 80 to 85: %f", se_80_to_85)
sprintf("95 CI lower bound for partial effect of increasing from 80 to 85: %f", score_80_to_85_ci95L)
sprintf("95 CI upper bound for partial effect of increasing from 80 to 85: %f", score_80_to_85_ci95H)


# Partial Effects of score increasing from 85 to 90
newdata1=data.frame(score=85,score_sq=85*85)
newdata2=data.frame(score=90,score_sq=90*90)

partial_effect_85_to_90 = predict(reg_sq, newdata=newdata2) - predict(reg_sq, newdata=newdata1)

# Fstatistic for Test of Difference in Score
Ftest=linearHypothesis(reg_sq,c("5*score+875*score_sq=0"),vcov = vcovHC(reg_sq, "HC1"))
Fstat=Ftest[2,3]

# calculate standard error
se_85_to_90 = abs(partial_effect_85_to_90)/sqrt(Fstat)

# 95% CI
score_85_to_90_ci95L = partial_effect_85_to_90 - se_85_to_90*1.96
score_85_to_90_ci95H = partial_effect_85_to_90 + se_85_to_90*1.96

# Outputting results
sprintf("partial effect of score increasing from 85 to 90: %f", partial_effect_85_to_90)
sprintf("SE of partial effect of increasing from 85 to 90: %f", se_85_to_90)
sprintf("95 CI lower bound for partial effect of increasing from 85 to 90: %f", score_85_to_90_ci95L)
sprintf("95 CI upper bound for partial effect of increasing from 85 to 90: %f", score_85_to_90_ci95H)


#*******************************************************************************
# QUESTION7
# create log(price) and log(score) in data
data$log_price = log(data$price)
data$log_score = log(data$score)

# Construct log-log regression between price and score
reg_log = lm(log_price~log_score+pinotnoir+cabernet+merlot+syrah
              +napa+bayarea+sonoma+scoast+carn+sierra+mendo+wash
              +d1991+d1992+d1993+d1994+d1995+d1996+d1997+d1998+d1999, 
              data = data)
se_log = sqrt(diag(vcovHC(reg_log, type = "HC1")))

# outputting result
stargazer(reg_log,type="text",
          se=list(se_log),
          digits=3, 
          omit=c("napa",
                 "bayarea",
                 "sonoma",
                 "scoast",
                 "carn",
                 "sierra",
                 "mendo",
                 "wash",
                 "othloc",
                 "d1990","d1991","d1992","d1993","d1994",
                 "d1995","d1996","d1997","d1998","d1999"),
          out="Q7_reg_output.txt")


#*******************************************************************************
# QUESTION8
linearHypothesis(reg_log,c("log_score=1"),vcov = vcovHC(reg_log, "HC1"))


#*******************************************************************************
# QUESTION9
# create interactive variable in data
data$pino_score = data$log_score * data$pinotnoir
data$cabe_score = data$log_score * data$cabernet
data$merl_score = data$log_score * data$merlot
data$syra_score = data$log_score * data$syrah

# construct respective log-log regression
reg_log_inter = lm(log_price~log_score+pino_score+cabe_score+merl_score+syra_score
                     +pinotnoir+cabernet+merlot+syrah
             +napa+bayarea+sonoma+scoast+carn+sierra+mendo+wash
             +d1991+d1992+d1993+d1994+d1995+d1996+d1997+d1998+d1999, 
             data = data)
se_log_inter = sqrt(diag(vcovHC(reg_log_inter, type = "HC1")))

# outputting result
stargazer(reg_log, reg_log_inter,
          type="text",
          se=list(se_log, se_log_inter),
          digits=3, 
          omit=c("napa",
                 "bayarea",
                 "sonoma",
                 "scoast",
                 "carn",
                 "sierra",
                 "mendo",
                 "wash",
                 "othloc",
                 "d1990","d1991","d1992","d1993","d1994",
                 "d1995","d1996","d1997","d1998","d1999"),
          out="Q8_reg_output.txt")


#*******************************************************************************
# QUESTION10
linearHypothesis(reg_log_inter,c("pino_score=cabe_score", 
                                 "cabe_score=merl_score",
                                 "merl_score=syra_score")
                 ,vcov = vcovHC(reg_log_inter, "HC1"))


