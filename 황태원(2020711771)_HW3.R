# 계량경제이론1
# 황태원(2020711771)
# HW3

getwd()
setwd("C:/Users/twter/Desktop/R")
dir()

# 6. HPRICE1.xls
# install.packages("car")
# install.packages("sandwich")
# install.packages("lmtest")
# install.packages("readxl")
library(car)
library(sandwich)
library(lmtest)
library(readxl)

hprice1 = read_excel("HPRICE1.xls", col_names=FALSE)
complete.cases(hprice1)
hprice = na.omit(hprice1)
n = nrow(hprice)
colnames(hprice)[1] = "price"
colnames(hprice)[2] = "assess"
colnames(hprice)[3] = "bdrms"
colnames(hprice)[4] = "lotsize"
colnames(hprice)[5] = "sqrft"
colnames(hprice)[6] = "colonial"
colnames(hprice)[7] = "lprice"
colnames(hprice)[8] = "lassess"
colnames(hprice)[9] = "llotsize"
colnames(hprice)[10] = "lsqrft"

# 6.(a)
ols1 = lm(price~lotsize+sqrft+bdrms, data=hprice)
coeftest(ols1)

# 6.(b)
bptest(ols1, ~lotsize+sqrft+bdrms, data=hprice)

# 6.(c)
coeftest(ols1,vcov=vcovHC)
coeftest(ols1)

# 6.(d)
ols2 = lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice)
coeftest(ols2)

bptest(ols2, ~log(lotsize)+log(sqrft)+bdrms, data=hprice)

coeftest(ols2,vcov=vcovHC)
coeftest(ols2)

# 7. FISH.xls
# install.packages("sandwich")
# install.packages("lmtest")
# install.packages("orcutt")
library(orcutt)
library(sandwich)
library(lmtest)

rm(list = ls())
fish1 = read_excel("FISH.xls", col_names=FALSE)
complete.cases(fish1)
fish = na.omit(fish1)
n = nrow(fish)
colnames(fish)[1]="prca"
colnames(fish)[2]="prcw"
colnames(fish)[3]="qtya"
colnames(fish)[4]="qtyw"
colnames(fish)[5]="mon"
colnames(fish)[6]="tues"
colnames(fish)[7]="wed"
colnames(fish)[8]="thurs"
colnames(fish)[9]="speed2"
colnames(fish)[10]="wave2"
colnames(fish)[11]="speed3"
colnames(fish)[12]="wave3"
colnames(fish)[13]="avgprc"
colnames(fish)[14]="totqty"
colnames(fish)[15]="lavgprc"
colnames(fish)[16]="ltotqty"
colnames(fish)[17]="t"
colnames(fish)[18]="lavgp_1"
colnames(fish)[19]="gavgprc"
colnames(fish)[20]="gavgp_1"

# 7.(a)
ols1 = lm(lavgprc~mon+tues+wed+thurs+t, data=fish)
coeftest(ols1)

# 7.(b)
ols2 = lm(lavgprc~mon+tues+wed+thurs+t+wave2+wave3, data=fish)
coeftest(ols2)

# 7.(d)
bgtest(ols2,order=1)

# 7.(e)
NW_vcov = NeweyWest(lm(lavgprc~mon+tues+wed+thurs+t+wave2+wave3, data=fish),lag = 3, prewhite=F, adjust=T)
coeftest(ols2,vcov=NW_vcov)
coeftest(ols2)

# 7.(f)
co=cochrane.orcutt(ols2, convergence = 8, max.iter=100)
summary(co)

co_restricted = cochrane.orcutt(lm(lavgprc~mon+tues+wed+thurs+t, data=fish), convergence = 8, max.iter=100)
summary(co_restricted)

uhat1 = resid(co)
uhat2 = resid(co_restricted)
ssr1 = sum(uhat1^2)  #SSR of unrestricted model
ssr2 = sum(uhat2^2)  #SSR of restricted model 
Fstat = ((ssr2-ssr1)/2)/(ssr1/(n-8))
pval = 1 - pf(Fstat,2,n-8)
pval