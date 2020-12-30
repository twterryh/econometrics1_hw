# Finals Part 2
# 황태원 (2020711771)

setwd("C:/Users/twter/Google Drive/Graduate/코스웍/2020 한희준 계량경제이론1/finals part 2")

############################################################################################
# 1 Thesis Replication

# install.packages("fBasics")
# install.packages("clubSandwich")
# install.packages("ivreg")
# install.packages("AER")
install.packages("gmm")
library(gmm)
library(fBasics)
library(clubSandwich)
library(ivreg)
library(AER)

############################################################################################
df = read.csv("황태원(2020711771)_data.csv")
df = pdata.frame(df, index=c("code_numeric", "year"))
n = nrow(df)

############################################################################################
# Table 2 Replication

col1 = lm(fhp ~ lag1_fhp + lag1_gdp + factor(year), data=subset(df, sample==1))
coeftest(col1, vcov=vcovHC(col1, type="HC0", cluster=c("group")))

col2 = lm(fhp ~ lag1_fhp + lag1_gdp + factor(year) + factor(code), data=subset(df, sample==1))
coeftest(col2, vcov=vcovHC(col2, type="HC0", cluster=c("group")))

col3 = ivreg(dif1_fhp ~ dif2_fhp + dif2_gdp + factor(year)| lag2_fhp + lag2_gdp + factor(year), 
             data=subset(df, sample==1))
coeftest(col3, vcov=vcovHC(col3, type="HC0", cluster=c("group")))

col4 = gmm(g = fhp ~ lag1_fhp + lag1_gdp + factor(year), x = ~lag1_fhp + factor(year) + lag2_gdp, 
           data=subset(df, sample==1), vcov = "HAC", wmatrix="ident")
summary(col4)

col5 = lm(fhp ~ lag1_gdp + factor(year) + factor(code), data=subset(df, sample==1))
coeftest(col5, vcov=vcovHC(col5, type="HC0", cluster=c("group")))


############################################################################################
rm(list = ls())
############################################################################################
# 3 MLE Estimation

# install.packages("bbmle")
# install.packages("lmtest")
library(bbmle)
library(lmtest)

############################################################################################
data = read.delim("RussianData.txt")
data = subset(data, select= -c(dummy, b2gr, b3gr))
n = nrow(data)

ylag = replicate(n, 0)
ylag[2:n] = data$y[1:n-1]

data = cbind(data, ylag)
data = data[2:n,]
n = nrow(data)

############################################################################################
# 3. 1)

logftn = function(b0, b1, b2, sigma2){
  Y.mean = b0 + b1*data$ylag + b2*data$b1gr
  log1 <- -0.5*n*log(2*pi) - 0.5*n*log(sigma2/(1-b1^2)) - sum((data$y-Y.mean)^2)/(2*sigma2/(1-b1^2))
  return(-log1)
}

model.mle <- mle2(logftn, start = list(b0=0, b1=0, b2=2, sigma2=1))
summary(model.mle)

############################################################################################
# 3. 2)

data$e = replicate(n, 0)

logftn = function(b0, b1, b2, b3, sigma2){
  
  for (t in 2:n) {
    data$e[t] = data$y[t] - (b0 + b1*data$y[t-1] + b2*data$b1gr[t] + b3*data$e[t-1])
  }
  
  log1 <- -0.5*n*log(2*pi) - 0.5*n*log(sigma2/(1-b1^2)) - sum(data$e^2)/(2*sigma2/(1-b1^2))
  return(-log1)
}

model.mle2 <- mle2(logftn, start = list(b0=0, b1=0, b2=2, b3=1, sigma2=1))
summary(model.mle2)