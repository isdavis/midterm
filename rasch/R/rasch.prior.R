rasch.prior <-
function(theta) {
  return(dnorm(theta, mean=0, sd=3))
}
