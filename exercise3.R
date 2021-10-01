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

Fama = Fama[,-1]
Fama = as.matrix(Fama)

reg = lm(smoke ~ Fama)

names(reg$coefficients) <- c("(Intercept)", tail(colnames(Fama),length(colnames(Fama))-1))
summary(reg)


nullhyp1 = c("Mkt-RF=0")
nullhyp2 = c("HML=0")
nullhyp3 = c("SMB=0")
nullhyp4 = c("Mkt-RF","HML","SMB")

library(car)
linearHypothesis(reg, nullhyp1)
linearHypothesis(reg, nullhyp2)
linearHypothesis(reg, nullhyp3)
linearHypothesis(reg, nullhyp4)

summary(smoke)
summary(Fama)

# exp(mean(log(1+Fama[,2]/100)))^12 - 1
# exp(mean(log(1+smoke/100)))^12 - 1




# Tests for multicollinearity
cor(as.data.frame(model.matrix(reg)[,-1]))  # We find high correlation between HML and CMA. Corr = 0.67
vif(as.data.frame(model.matrix(reg)[,-1]))


# Test for normality
library(moments);
jarque.test(residuals(reg));


#Heteroskedasticity med White

#Auto test med 


#noNinetyNines = industries[]
#round(colMeans(industries),3)

#library(stargazer);






