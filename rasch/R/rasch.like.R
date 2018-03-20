rasch.like <-
function(stud, theta) {
  PQ<-rasch.prob(stud, theta)[2]
  return(prod(PQ))
}
