#' Calculating the log likelihood of Poisson data
#'
#' Returns the height of the LL for a given value of lambda
#'
#' @param stud a rasch object
#' @param theta A numeric object 

#' @return two vectors: the vector P, and the vector PQ, as described on the midterm 
#'
#' @author Ian Davis 
#' @rdname rasch.prob
#' @export
rasch.prob <-
function(stud, theta) {
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
