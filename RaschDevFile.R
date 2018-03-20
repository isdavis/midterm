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
  estimateN<-integrate(numerator, stud=stud, lower, upper)
  print(estimateN)
  estimateD<-integrate(denominator, stud=stud, lower, upper)
  print(estimateD)
  return(estimateN/estimateD)
}

#test
rasch.EAP(i, 0,2)


