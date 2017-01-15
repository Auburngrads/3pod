\name{ulik}
\alias{ulik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ulik(rx, ry, levs0, em, es)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
}
  \item{levs0}{
%%     ~~Describe \code{levs0} here~~
}
  \item{em}{
%%     ~~Describe \code{em} here~~
}
  \item{es}{
%%     ~~Describe \code{es} here~~
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
function (rx, ry, levs0, em, es) 
{
    shi = es
    slo = es/2
    val1 = val2 = xyllik(rx, ry, em, slo)
    while (val1 > levs0) {
        shi = slo
        slo = slo/2
        val1 = xyllik(rx, ry, em, slo)
    }
    while (val2 < levs0) {
        slo = shi
        shi = 2 * shi
        val2 = xyllik(rx, ry, em, shi)
    }
    eps = 1e-05
    delt = 1
    while (delt > eps) {
        s = (slo + shi)/2
        val = xyllik(rx, ry, em, s)
        if (val > levs0) 
            shi = s
        else slo = s
        delt = abs(val - levs0)
    }
    s = (slo + shi)/2
    return(s)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
