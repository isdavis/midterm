#' Calculatin the expected a posteriori value (EAP) for a given student  
#'
#' Returns the solution to a definite integral as defined by the user given a student
#'
#' @param stud a rasch object
#' @param lower the lower bound of the integral 
#' @param upper the upper bound of the integral 

#' @return two vectors: the vector P, and the vector PQ, as described on the midterm 
#'
#' @author Ian Davis 
#' @rdname rasch.EAP
#' @export
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
  estimateN<-integrate(numerator, stud=stud, lower, 0)$value+integrate(numerator, stud=stud, 0, upper)$value
  estimateD<-integrate(denominator, stud=stud, lower, 0)$value+integrate(denominator, stud=stud, 0, upper)$value
  return(estimateN/estimateD)
}
