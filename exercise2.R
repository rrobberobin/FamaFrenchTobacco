rm(list=ls());
cat("\014");

#Exercise 2


#Creating made-up annual returns % for 3 stocks, as a template for the variance-covariance matrix
StockA<-c(5.7,3.1,-7.8,9.2,-2.7,4.5)
StockB<-c(-18.3,12.6,-2.2,9.1,3.3,4.4)
StockC<-c(10.5,-1.2,4.9,5.6,7.7,-0.7)


#Merging the return vectors into an annual return matrix
Annualreturnsmatrix<-matrix(cbind(StockA,StockB,StockC),nrow=6,ncol=3,byrow=FALSE)
#Naming matrix rows
rownames(Annualreturnsmatrix)<-c("2016","2017","2018","2019","2020","2021")
#Naming matrix columns
colnames(Annualreturnsmatrix)<-c("Stock A","Stock B","Stock C")
#Printing Annual return matrix for testing purposes
Annualreturnsmatrix

#Creating covariance matrix for the annual returns matrix. Stock variance is shown in the matrix diagonal.
Covariancematrix<-cov(Annualreturnsmatrix)
Covariancematrix


#Creating expected return % vector through arithmetic average of historical annual stock returns 
#I am using arithmetic return average instead of geometric since the annual return data is "only" 6 years. 
Expectedreturns<-c((sum(StockA)/length(StockA)),(sum(StockB)/length(StockB)),(sum(StockC)/length(StockC))) 
Riskfreereturn<-c(1.33) 



#all numbers are in annual percentages
covMat <- Covariancematrix
expRet <- Expectedreturns
riskFree <- Riskfreereturn

#a)
#function fort testing if matrix is positive semi-definite (PSD)
posSemDef <- function(x) {
  
  #We have to test if the matrix is symmetrical. We test it with the method isSymmetric()
  if(isSymmetric(x)){     #if it is symmetrical, we can calculate the eigenvalues,
    eig <- eigen(x)$values  #we assign the eigenvalues to the variable eig
    all(eig >= 0)           #tests if all eigenvalues are non-negative. Returns true if all are non-negative
  }
  else FALSE    #if it is not symmetric, we cant calculate the eignevalues. That means it is not PSD (positive semi-definite), so we return false
}

#test if PSD
posSemDef(covMat)

#doubleChecking with a ready made package
library(matrixcalc)
is.positive.semi.definite(covMat)

#b)

library(matlib) #package needed for taking inverses of matrices

#function for calculating the weights for the minimum variance portfolio
minVar <- function(x) {
  if(posSemDef(x)){
    rank <- sqrt(length(x))
    numerator <- inv(x) %*% rep(1,rank)
    denominator <- t(rep(1,rank)) %*% inv(x) %*% rep(1,rank)
    
    weights <- numerator / denominator[1]
    
    #sanity check
    if(sum(weights) == 1){
      weights
    }
    else print("Weights do not sum to 1")
    
  }
  else NA
}

minVar(covMat)


#doubleChecking with a ready made package
library(NMOF)
minvar(covMat)

#c)

#function for calculating the variance
variance <- function(x) {
  if(posSemDef(x)){
    t(minVar(x)) %*% x %*% minVar(x)
  }
  else NA
}

#function for calculating the Sharpe ratio
sharpe <- function(matrx, expReturns, riskFr) {
  if(posSemDef(matrx)){
    weights <- minVar(matrx)
    (t(weights) %*% expReturns - riskFr)/ sqrt(variance(matrx))
  }
  else NA
}

sharpe(covMat,expRet,riskFree)

#doubleChecking with a ready made package
#......




