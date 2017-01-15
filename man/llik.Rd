\name{llik}
\alias{llik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
llik(mydata, mu, sig, response = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
%%     ~~Describe \code{mydata} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
}
  \item{response}{
%%     ~~Describe \code{response} here~~
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
function (mydata, mu, sig, response = 1) 
{
    mydata = na.omit(mydata)
    x = mydata$X
    y = mydata$Y
    n = mydata$COUNT
    if (response == 0) 
        y = abs(y - 1)
    i1 = which(y == 1)
    eps = 1e-06
    if (sig < eps) 
        sig = eps
    ll = n[i1] * log(pnorm((x[i1] - mu)/sig))
    ll = c(ll, n[-i1] * log(1 - pnorm((x[-i1] - mu)/sig)))
    a = sum(ll)
    if (is.na(a)) 
        a = -Inf
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
