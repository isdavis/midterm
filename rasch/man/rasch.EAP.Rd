% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasch.EAP.R
\name{rasch.EAP}
\alias{rasch.EAP}
\title{Calculatin the expected a posteriori value (EAP) for a given student}
\usage{
rasch.EAP(stud, lower = -6, upper = 6)
}
\arguments{
\item{stud}{a rasch object}

\item{lower}{the lower bound of the integral}

\item{upper}{the upper bound of the integral}
}
\value{
two vectors: the vector P, and the vector PQ, as described on the midterm
}
\description{
Returns the solution to a definite integral as defined by the user given a student
}
\author{
Ian Davis
}
