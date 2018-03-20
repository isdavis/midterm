rasch.EAP <-
function(stud, lower=-6, upper=6) {
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
