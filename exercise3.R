#Exercise 3
#Important:
#Set your working directory to source file location!!!
# Or launch R in the project folder

rm(list=ls());
cat("\014");

library(readxl)

#Three-factor model
#Fama  <- read_excel("F-F_Research_Data_Factors.xlsx", skip = 3, n_max=1142, col_types = "numeric")
industries <- read_excel("49_Industry_Portfolios.xlsx", skip = 11, n_max=1142, col_types = "numeric")

#Five-factor model
Fama  <- read_excel("F-F_Research_Data_5_Factors_2x3.xlsx", skip = 3, n_max=697, col_types = "numeric")
industries = tail(industries,697)

smoke = industries[,6]
smoke = as.matrix(smoke)


# ThreeFactor = ThreeFactor[,-1]
# ThreeFactor = as.matrix(ThreeFactor)


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
round(colMeans(industries),2)
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


#do auxiliary regressions
#lm(model.matrix(reg1)[,-1] ~ model.matrix(reg1)[,-1])


# Test for normality
library(moments);
jarque.test(residuals(reg1)); #we reject the null hypothesis => Evidence for non-normal residuals
plot(residuals(req1))
#CLT. Vi har tagit så stor sample size som möjligt


#Heteroskedasticity test: White
# white = lm(residuals(reg1)^2 ~.^2, data=as.data.frame(Fama))
# whiteDesc = summary(white)
# NR2 = nobs(reg1) * whiteDesc$r.squared
# NR2
# pchisq(NR2,white$df.residual) #p-value is approximately zero

# library(lmtest);
# bptest(reg1);        # Breusch-Pagan



#robust standard errors

#Auto test med Andrews


#noNinetyNines = industries[]
#round(colMeans(industries),3)

#library(stargazer);






