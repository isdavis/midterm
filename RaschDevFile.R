#This is my development file for the midterm. First, set the directory: 

setwd("C:/Users/isdav/Documents/Github/midterm")
getwd()

#Now to write the functions. In order for any of them to work, we need to define what a rasch object is: 

setClass("rasch", representation(name="character", a = "numeric", y = "numeric"))
i<-new("rasch", name = "Ian", a=c(2,3,4), y=c(1,0,1))

#Now, for the probability function. Input "stud" is a rasch object representing a student 

rasch.prob<-function(stud, theta) {
  a<-stud@a
  y<-stud@y
#This function inside the rasch.prob function performs the formula and allows us to apply it over vectors 
  probability<-function(ai, th) {
    return(exp(th-ai)/(1+exp(th-ai)))
  }
#This function inside the rasch.prob function takes in the vector P and outputs the vector PQ as described
#in the first part of the probability function question
  makeQ<-function(xi, yi) {
    ret<-xi
    if(yi==0) { ret<-(1-xi)}
    return(ret)
  }
#I often use 'ret' as a variable name for something I eventually wish to return. In this case, 'ret' 
#corresponds to the vector P
  ret<-sapply(a, probability, th=theta)
#finalReturnVector corresponds to the vector PQ, and we then return them both. 
  finalReturnVector<-mapply(makeQ,ret,y)
  return(c(finalReturnVector, ret))
}

#This is a test of the rasch.prob function. It seems to work
rasch.prob(i, 1)

#The likelihood function. This calls rasch.prob and returns the prodcuct of the vector PQ 
rasch.like<-function(stud, theta) {
  PQ<-rasch.prob(stud, theta)[2]
  return(prod(PQ))
}

#Prior function
rasch.prior<-function(theta) {
  return(dnorm(theta, mean=0, sd=3))
}

#Small tests: 
rasch.like(i, 2)
rasch.prior(2)

#EAP function. This defaults to an integral from -6 to 6. 
rasch.EAP<-function(stud, lower=-6, upper=6) {
#Denominator function: L(theta|y)*pi(theta)
  denominator<-function(stud, theta) {
    rasch.like(stud, theta)*rasch.prior(theta)
  }
#numerator function: L(theta|y)*pi(theta)*theta
  numerator<-function(stud, theta) {
    rasch.like(stud, theta)*rasch.prior(theta)*theta
  }
  estimateN<-integrate(numerator, stud=stud, lower, upper)$value
  print(estimateN)
  estimateD<-integrate(denominator, stud=stud, lower, upper)$value
  print(estimateD)
  return(estimateN/estimateD)
}

#test
rasch.EAP(i, 0,2)

#package created! sort of
getwd()
package.skeleton("rasch")

#we can edit the description file, delete all but the R files, and then edit the R files so that they have 
#the correct preambles, allowing roxygen to create rd files that work properly 

rm(list=ls())
library(devtools)
library(roxygen2)

#now with all the R files properly introduced, we can run this code to set up the package

current.code<-as.package("rasch")
load_all(current.code)
document(current.code)

#with this done, we can set up the class structure with another R file 
#if the class has been set up correctly, we should be able to create a new object without issue: 

new("rasch")
u<-new("rasch", name="Ian Davis", a=c(1,2,3,4), y=c(1,1,0,0))

check(current.code)
rasch.EAP(u)
rasch.print(u)
#Everything works! sort of?? I'm going to investigate why my function returns 0 

i<-new("rasch", name = "Ian", a=c(0.2,0.5,0.7,0.2), y=c(1,1,1,1))
numerator<-function(stud, theta) {
  rasch.like(stud, theta)*rasch.prior(theta)*theta
}
denominator<-function(stud, theta) {
  rasch.like(stud, theta)*rasch.prior(theta)
}
#After a bunch of troubleshooting, I found this: these two expressions, which to my understanding, 
# should be equal, in fact are not: 

integrate(numerator, stud=i, -6,0)$value+integrate(numerator, stud=i, 0,6)$value
integrate(numerator, stud=i, -6,6)$value

#I don't know why this is? granted I'm tired and maybe I just dont understand math but this strikes me as wrong
# I don't know enough about the integrate function to fix this, so I'm just going to substitute the first 
#expression, with addition, into the rasch.EAP function so that it calculates correctly (I believe)

current.code<-as.package("rasch")
load_all(current.code)
document(current.code)
check(current.code)

setwd("C:/Users/isdav/Documents/GitHub/midterm/rasch")
devtools::test()

## Install the package
install(pkg=current.code, local=TRUE)


## Build a version of the package to share manually
build(current.code, path=getwd())
