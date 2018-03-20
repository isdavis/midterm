#' Calculating the likelihood of a given theta and student 
#'
#' Returns the product of the PQ vector cretedy by the prob function 
#'
#' @param stud a rasch object
#' @param theta A numeric object 

#' @return the product of the vector PQ, as retrieved from the rasch.prob function 
#'
#' @author Ian Davis 
#' @rdname rasch.like
#' @export
rasch.like <-
function(stud, theta) {
  PQ<-rasch.prob(stud, theta)[2]
  return(prod(PQ))
}
