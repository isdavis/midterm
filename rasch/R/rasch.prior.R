#' Calculating the prior 
#'
#' Returns the prior value of a given theta 
#'
#' @param theta A numeric object 

#' @return the prior, the result of dnorm at theta of a distribution with mean=0 and sd=3 
#'
#' @author Ian Davis 
#' @rdname rasch.prior
#' @export
rasch.prior <-
function(theta) {
  return(dnorm(theta, mean=0, sd=3))
}
