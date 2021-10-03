#Exercise 3
#Important:
#Set your working directory to source file location!!!
# Or launch R in the project folder

rm(list=ls());
cat("\014");

library(readxl)


#The five-factors
Fama  <- read_excel("F-F_Research_Data_5_Factors_2x3.xlsx", skip = 3, n_max=697, col_types = "numeric")

#The returns for 49 industries
industries <- read_excel("49_Industry_Portfolios.xlsx", skip = 11, n_max=1142, col_types = "numeric")
industries = tail(industries,697) #Remove extra years from the begging. We are only using the years 1963-2021

#The returns for the smoke industry
smoke = industries[,6]
smoke = as.matrix(smoke)


Fama = Fama[,-1]  #remove the dates
Fama = as.matrix(Fama)  #make dataframe into matrix 

excessReturns = smoke - Fama[,6]


Fama = Fama[,1:5] #Remove the riskfree rate column

#använd i ställe:
#lm(excessReturns ~., data=as.data.frame(Fama))
reg1 = lm(excessReturns ~ Fama)
names(reg1$coefficients) <- c("(Intercept)", colnames(Fama))
summary(reg1)
#plot(reg1)


# check the hyptheses that....
nullhyp1 = c("Mkt-RF=0")
nullhyp2 = c("HML=0")
nullhyp3 = c("SMB=0")
nullhyp4 = c("Mkt-RF","HML","SMB")

library(car)
linearHypothesis(reg1, nullhyp1)
linearHypothesis(reg1, nullhyp2)
linearHypothesis(reg1, nullhyp3)
linearHypothesis(reg1, nullhyp4)

geoMean <- function(x){
  geo = exp(colMeans(log(1+x/100))) #geoMean
  (geo - 1)*100 #in percentages
}

summary(smoke)
geoMean(smoke) # 1.144416% per month
((geoMean(smoke)/100+1)^12-1)*100 #14.63123% per year

summary(Fama)
geoMean(Fama)
((geoMean(Fama)/100+1)^12-1)*100 #per year

industries = industries[,-1]
round(colMeans(industries),2)   #Vi måst komma ihåg o remova alla -99.99
round(geoMean(industries),2)
exp(mean(log(1+Fama[,2]/100)))^12 - 1
exp(mean(log(1+smoke/100)))^12 - 1


# Tests for multicollinearity
cor(as.data.frame(model.matrix(reg1)[,-1]))  # We find high correlation between HML and CMA. Corr = 0.67
vif(lm(excessReturns ~., data = as.data.frame(Fama))) # Higher than 10?


#lets drop HML
noHML = Fama[,-3]
reg2 = lm(excessReturns ~ noHML)
names(reg2$coefficients) <- c("(Intercept)", colnames(noHML))
summary(reg2)


#auxiliary regressions?
#lm(model.matrix(reg1)[,-1] ~ model.matrix(reg1)[,-1])


# Test for normality. We have a big sample size, which decrease the problem
library(moments);
jarque.test(residuals(reg1)); #we reject the null hypothesis => Evidence for non-normal residuals
plot(residuals(reg1))
hist(residuals(reg1), breaks=50)
plot(smoke, Fama[,5])
#CLT. As we used the whole sample for the FF5 factors from the French library, we consider the sample size to be as big as possible
#As there exists no other samples of the returns except the real historical returns
#If we would have daily data, we would get a bigger sample, and the statistca would become more normally distributed (CLT would apply)

#Let's try make a logarithmic transform of the returns (continously compounded)
logRet = lm(log(1+excessReturns/100) ~ log(1+Fama/100))
summary(logRet)
hist(residuals(logRet), breaks=50)
jarque.test(residuals(logRet)); #no improvement. Now the residuals are even less normal

#We expect heteroskedasticity
#Heteroskedasticity test: White
white = lm(residuals(reg1)^2 ~.^2, data=as.data.frame(Fama))
whiteDesc = summary(white)
NR2 = nobs(reg1) * whiteDesc$r.squared
#pValue = 1 - pchisq(NR2,white$df.residual)
pValue = 1 - pchisq(NR2,ncol(model.matrix(white)[,-1])) 
pValue #p-value is approximately 0.45. We reject the null hypothesis on significance values of 1, 5 and 10 percent. Seems like we have heteroskedasticity

library(lmtest)
bptest(reg1)      # Breusch-Pagan is the same test, and confirms the same message

#Which variables create the problem? Let's see
summary(white)
#The interactions Mkt-RF:RMW and RMW:CMA are significant on all general levels
#HML:RMW is significant on the 5% level. 
#In general RMW, seems to cause heteroskedasticity. 
#We could try to remove it. But we cant remove significant variables, and RMW is significant on all general significance levels
#Instead: let's correct for heteroskedasticity using robust standard errors
#For the White robust standard errors: When we have a large sample and heteroskedasticity is expected, we use HCE3

library(sandwich) # also using library(lmtest)
robustReg1 = coeftest(reg1, vcov=vcovHC(reg1, type=c("HC3")))
robustReg1
linearHypothesis(reg1, nullhyp4, vcov=vcovHC(reg1, type=c("HC3")));


#We expect autocorrelation. Let's test
#"bgtest" is from the "lmtest" pack
bgtest(reg1, 12)  #test of order 12 gives us the p-value is 0.3371. We seem to be having autocorrelation


# resid = residuals(reg1)
# laggedResids = c()
# for(n in 1:12){
#   laggedResids = cbind(laggedResids,lag(resid,n))}
# laggedReg = lm(resid ~ Fama + lag(resid, -1) + lag(resid, -2) + lag(resid, -3))
# summary(laggedReg)

#Correct for heteroskedasticity and autocorrelation (Andrews)
robustReg1AutoHetero = coeftest(reg1, vcov=vcovHAC(reg1))
robustReg1AutoHetero
linearHypothesis(reg1, nullhyp4, vcov=vcovHAC(reg1)) #model is still significant


#library(stargazer)


