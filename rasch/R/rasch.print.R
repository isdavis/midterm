#' Printing out the values of the EAP function 
#'
#' Returns the name and EAP of a given student 
#'
#' @param stud a rasch object containing a student's information 

#' @return the student's name and estimated theta 
#'
#' @author Ian Davis 
#' @rdname rasch.print
#' @export
rasch.print <-
function(stud) {
  print(c(stud@name, rasch.EAP(stud)))
}
