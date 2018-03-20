\name{rasch.prob}
\alias{rasch.prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasch.prob(stud, theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stud}{
%%     ~~Describe \code{stud} here~~
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (stud, theta) 
{
    a <- stud@a
    y <- stud@y
    probability <- function(ai, th) {
        return(exp(th - ai)/(1 + exp(th - ai)))
    }
    makeQ <- function(xi, yi) {
        ret <- xi
        if (yi == 0) {
            ret <- (1 - xi)
        }
        return(ret)
    }
    ret <- sapply(a, probability, th = theta)
    finalReturnVector <- mapply(makeQ, ret, y)
    return(c(finalReturnVector, ret))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
