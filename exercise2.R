#Exercise 2
#Important:
#Set your working directory to source file location!!!
# Or launch R in the project folder

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
Annualreturnsmatrix

#Creating a covariance matrix from the annual returns matrix. Asset variances are shown in the matrix diagonal.
covMat<-cov(Annualreturnsmatrix)
print(round(covMat,2))


#function for the geometric mean
geoMean <- function(x){
  geo = exp(mean(log(1+x/100))) #calculate the geometric mean
  (geo - 1)*100 #make into percentages
  
}

#Creating an expected return vector through the geometric average of historical annual asset returns
#we us the geoMean function created earlier
expRet<-c(geoMean(AssetA),geoMean(AssetB),geoMean(AssetC))
names(expRet) = colnames(Annualreturnsmatrix)
cat("\n")
print(round(expRet, 2))#print with 2 decimals

#The risk-free rate
riskFree<-c(1.33)
cat("\nThe risk-free rate:\n")
cat(riskFree)

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
if(PSD(covMat)) {
  cat("\n\nEigenvalues:\n")
  print(round(eigen(covMat)$values,2))     #let's print the eigenvalues as well with 2 decimals
}
  
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


cat("\n\nWeights for the minimum variance portfolio\n")
print(round(minVarWeights(covMat),3))  #the weights for the minimum variance portfolio


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

cat("\nVariance:\n")
cat(round(variance(covMat),2))

#function for calculating the Sharpe ratio
sharpe <- function(matrx, expReturns, riskFr) {
  if(PSD(matrx)){   #check if PSD
    weights <- minVarWeights(matrx)
    (t(weights) %*% expReturns - riskFr)/ sqrt(variance(matrx))   #using the formula for Sharpe ratio
  }
  else NA   #can't calculate variance if the matrix is not PSD
}

cat("\n\nSharpe:\n")
cat(round(sharpe(covMat,expRet,riskFree),2))  #use the function to calculate the Sharpe ratio and round to 2 decimals

