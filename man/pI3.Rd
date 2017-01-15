\name{pI3}
\alias{pI3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pI3(d0, dat0, sg, tmu, tsig, reso, ln, iseed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d0}{
%%     ~~Describe \code{d0} here~~
}
  \item{dat0}{
%%     ~~Describe \code{dat0} here~~
}
  \item{sg}{
%%     ~~Describe \code{sg} here~~
}
  \item{tmu}{
%%     ~~Describe \code{tmu} here~~
}
  \item{tsig}{
%%     ~~Describe \code{tsig} here~~
}
  \item{reso}{
%%     ~~Describe \code{reso} here~~
}
  \item{ln}{
%%     ~~Describe \code{ln} here~~
}
  \item{iseed}{
%%     ~~Describe \code{iseed} here~~
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
function (d0, dat0, sg, tmu, tsig, reso, ln, iseed) 
{
    j = m.update(d0)
    M0 = j$M0
    m1 = j$m1
    del = m1 - M0
    xx = (M0 + m1)/2
    if (sg + del > 0) 
        xx = xx + c(1, -1) * sg/2
    lxx = length(xx)
    for (i in 1:lxx) {
        if (i < lxx) 
            u = gd0(xx[i], d0, dat0, "I3", tmu, tsig, reso, ln, 
                iseed)
        if (i == lxx) 
            u = gd0(xx[i], d0, dat0, "I3", tmu, tsig, reso, ln, 
                iseed)
        d0 = u$d0
        dat0 = u$dat0
    }
    ret = list(d0, dat0)
    names(ret) = c("d0", "dat0")
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
