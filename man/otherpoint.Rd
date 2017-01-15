\name{otherpoint}
\alias{otherpoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
otherpoint(rx, ry, muhat, levs0, con)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
}
  \item{muhat}{
%%     ~~Describe \code{muhat} here~~
}
  \item{levs0}{
%%     ~~Describe \code{levs0} here~~
}
  \item{con}{
%%     ~~Describe \code{con} here~~
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
function (rx, ry, muhat, levs0, con) 
{
    k = 1.5
    slo = Inf
    shi = -Inf
    s = 1
    while (slo > shi) {
        m = muhat - qnorm(con) * s
        val = xyllik(rx, ry, m, s)
        if (val > levs0) {
            slo = s
            s = k * s
        }
        else {
            shi = s
            s = s/k
        }
    }
    eps = 1e-05
    delt = 1
    while (delt > eps) {
        s = (slo + shi)/2
        m = muhat - qnorm(con) * s
        val = xyllik(rx, ry, m, s)
        if (val > levs0) 
            slo = s
        else shi = s
        delt = abs(val - levs0)
    }
    s = (slo + shi)/2
    m = muhat - qnorm(con) * s
    return(c(m, s))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
