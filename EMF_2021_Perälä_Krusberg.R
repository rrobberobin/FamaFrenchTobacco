#Important:
#Set your working directory to source file location!!!
# Or launch R in the project folder


#Exercise 2
rm(list=ls());
cat("\014");


#Creating made-up annual returns (in %) for 3 assets, as a template for the covariance matrix
AssetA<-c(5.7,3.1,-7.8,9.2,-2.7,4.5)
AssetB<-c(-18.3,12.6,-2.2,9.1,3.3,4.4)
AssetC<-c(10.5,-1.2,4.9,5.6,7.7,-0.7)


#Merging the return vectors into an annual return matrix
Annualreturnsmatrix<-matrix(cbind(AssetA,AssetB,AssetC),nrow=6,ncol=3,byrow=FALSE)
#Naming matrix rows
rownames(Annualreturnsmatrix)<-c("2016","2017","2018","2019","2020","2021")
#Naming matrix columns
colnames(Annualreturnsmatrix)<-c("Asset A","Asset B","Asset C")
#Printing Annual return matrix
Annualreturnsmatrix

#Creating a covariance matrix from the annual returns matrix. Asset variances are shown in the matrix diagonal.
covMat<-cov(Annualreturnsmatrix)
round(covMat,2)


#function for the geometric mean
geoMean <- function(x){
  geo = exp(mean(log(1+x/100))) #calculate the geometric mean
  (geo - 1)*100 #make into percentages
  
}

#Creating an expected return vector through the geometric average of historical annual asset returns
#we us the geoMean function created earlier
expRet<-c(geoMean(AssetA),geoMean(AssetB),geoMean(AssetC))
print(round(expRet, 2))#print with 2 decimals

#The risk-free return
riskFree<-c(1.33)


#a)
#function for testing if matrix is positive semi-definite (PSD)
PSD <- function(x) {
  
  #We have to test if the matrix is symmetrical. We test it with the method isSymmetric()
  if(isSymmetric(x)){       #if it is symmetrical, we can calculate the eigenvalues,
    eig <- eigen(x)$values  #we assign the eigenvalues to the variable "eig"
    all(eig >= 0)           #tests if all eigenvalues are non-negative. Returns true if all are non-negative
  }
  else FALSE    #if it is not symmetric, we cant calculate the eigenvalues. That means it is not PSD (positive semi-definite), so we return false
}

#we use the function on our matrix
PSD(covMat)
if(PSD(covMat)) print(round(eigen(covMat)$values,2))     #let's print the eigenvalues as well with 2 decimals

#doubleChecking if PSD with a ready made package
library(matrixcalc)
is.positive.semi.definite(covMat)

#b)

library(matlib) #package needed for taking inverses of matrices

#function for calculating the weights for the minimum variance portfolio
minVarWeights <- function(x) {
  if(PSD(x)){   #check if matrix is PSD
    rows <- nrow(x) #how many rows does the matrix have
    
    #the matrix formula for calculating the minimum variance portfolio
    numerator <- inv(x) %*% rep(1,rows)   #the numerator
    denominator <- t(rep(1,rows)) %*% inv(x) %*% rep(1,rows)  #the denominator
    
    weights <- numerator / denominator[1]   #dividing the numerator by denominator gives us the weights
    
    #sanity check. The weights have to sum up to 1
    if(sum(weights) == 1) weights
    else print("Weights do not sum to 1")
  }
  else NA   #if the matrix is not PSD, we can't calculate a minimum variance portfolio
}

round(minVarWeights(covMat),3)  #the weights for the minimum variance portfolio


#doubleChecking the minimum variance weights with a ready made package
library(NMOF)
minvar(covMat)
minvar(covMat, wmin = -Inf, wmax = Inf) #test when shorting is allowed



#c)

#function for calculating the variance
variance <- function(x) {
  if(PSD(x)){   #check if PSD
    t(minVarWeights(x)) %*% x %*% minVarWeights(x)  #the matrix formula for variance
  }
  else NA  #can't calculate variance if the matrix is not PSD
}

round(variance(covMat),2)

#function for calculating the Sharpe ratio
sharpe <- function(matrx, expReturns, riskFr) {
  if(PSD(matrx)){   #check if PSD
    weights <- minVarWeights(matrx)
    (t(weights) %*% expReturns - riskFr)/ sqrt(variance(matrx))   #using the formula for Sharpe ratio
  }
  else NA   #can't calculate variance if the matrix is not PSD
}

round(sharpe(covMat,expRet,riskFree),2)  #use the function to calculate the Sharpe ratio and round to 2 decimals






#Exercise 3
# rm(list=ls());
# cat("\014");

library(readxl)

#The five-factors
Fama  <- read_excel("F-F_Research_Data_5_Factors_2x3.xlsx", skip = 3, n_max=697, col_types = "numeric")

#The returns for 49 industries
industries <- read_excel("49_Industry_Portfolios.xlsx", skip = 11, n_max=1142, col_types = "numeric")
industries = tail(industries,697) #Remove extra years from the begging. We are only using the years 1963-2021

#The returns for the smoke industry
smoke = industries[,6]
smoke = as.matrix(smoke)
plot.ts(smoke)
hist(smoke, breaks=50, main = "Histogram of smoke returns")

Fama = Fama[,-1]  #remove the dates
Fama = as.matrix(Fama)  #make dataframe into matrix 

excessReturns = smoke - Fama[,6]


Fama = Fama[,1:5] #Remove the riskfree rate column

#använd i ställe:
#lm(excessReturns ~., data=as.data.frame(Fama))
reg1 = lm(excessReturns ~ Fama)
names(reg1$coefficients) <- c("(Intercept)", colnames(Fama))
summary(reg1)


nullhyp1 = c("Mkt-RF")
nullhyp2 = c("HML")
nullhyp3 = c("SMB")
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


#lets try dropping HML
noHML = Fama[,-3]
reg2 = lm(excessReturns ~ noHML)
names(reg2$coefficients) <- c("(Intercept)", colnames(noHML))
summary(reg2)


# Test for normality. We have a big sample size, which decrease the problem
library(moments);
jarque.test(residuals(reg1)); #we reject the null hypothesis => Evidence for non-normal residuals
hist(residuals(reg1), breaks=50, main = "Histogram of model residuals")
#As we used the whole sample for the FF5 factors from the French library, we consider the sample size to be as big as possible
#As there exists no other samples of the returns except the real historical returns
#If we would have daily data, we would get a bigger sample, and the static would become more normally distributed (CLT would apply)

#Let's try make a logarithmic transform of the returns
logRet = lm(log(1+excessReturns/100) ~ log(1+Fama/100))
summary(logRet)
hist(residuals(logRet), breaks=50)
jarque.test(residuals(logRet)) #no improvement. Now the residuals are even less normal

#We expect heteroskedasticity:
#Heteroskedasticity test: White
white = lm(residuals(reg1)^2 ~.^2, data=as.data.frame(Fama))
NR2 = nobs(reg1) * summary(white)$r.squared
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

#robust standard errors
library(sandwich) # also using library(lmtest)
robustReg1 = coeftest(reg1, vcov=vcovHC(reg1, type=c("HC3")))
robustReg1
linearHypothesis(reg1, nullhyp4, vcov=vcovHC(reg1, type=c("HC3")));


#We expect autocorrelation as we have monthly data. Let's test
#"bgtest" is from the "lmtest" pack
bgtest(reg1, 12)  #test of order 12 gives us the p-value is 0.3371. We seem to be having autocorrelation
acf(smoke,lag.max=12)


#Correct for heteroskedasticity and autocorrelation (Andrews)
robustReg1AutoHetero = coeftest(reg1, vcov=vcovHAC(reg1))
robustReg1AutoHetero
linearHypothesis(reg1, nullhyp4, vcov=vcovHAC(reg1)) #model is still significant


#Information criteria
round(AIC(reg1),2)
round(BIC(reg1),2)


library(stargazer)
stargazer(reg1, reg2,
          type="text")



