# 계량경제이론1
# 황태원(2020711771)
# HW4

#install.packages("car")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("AER")
library(AER)
library(car)
library(sandwich)
library(lmtest)

getwd()
setwd("C:/Users/twter/Desktop/R")
dir()

fertil <- read.csv("fertil2.csv")
fertil$age2 = fertil[,2]^2

# 1) 
ols = lm(children~educ+age+age2, data=fertil)
coeftest(ols)

# 2)
ols_IV = lm(educ~frsthalf+age+age2, data=fertil)
coeftest(ols_IV)

# 3) 
tsls = ivreg(children~educ+age+age2|frsthalf+age+age2, data=fertil)
summary(tsls)

# 4) 
ols2 = lm(children~educ+age+age2+tv, data=fertil)
coeftest(ols2)

tsls2 = ivreg(children~educ+age+age2+tv|frsthalf+age+age2+tv, data=fertil)
coeftest(tsls2)
