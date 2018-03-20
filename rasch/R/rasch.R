#' an object with a students information 
#' 
#' object of class rasch contains student information to help determine theta
#' 
#' 
#' An object of the class 'Poisson' has the following slots:
#' \itemize{
#' \item \code{name} the student's name 
#' \item \code{a} a numeric vector with all the a values that determine question difficulty 
#' \item \code{y} a numeric vector with all the student's answers (0 for correct, 1 for correct)
#' }
#'
#' @author Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases rasch-class initialize,rasch-method 
#' @rdname rasch
#' @export
setClass(Class="rasch", 
         representation = representation(
           name = "character",
           a = "numeric",
           y = "numeric"
         ),
         prototype = prototype(
           name = c(),
           a = c(),
           y = c()
         )
)

#' @export
setMethod("initialize", "rasch", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

