\name{ntau}
\alias{ntau}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ntau(dat, response = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
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
function (dat, response = 1) 
{
    if (response == 0) 
        dat$Y = 1 - dat$Y
    st = dat$X
    i1 = which(dat$Y == 1)
    i0 = which(dat$Y == 0)
    r1 = r0 = n = dat$COUNT
    r1[i0] = 0
    r0[i1] = 0
    nt = sum(n)
    n1 = sum(r1)
    tau1 = sum((r1/n - n1/nt) * (n * st))
    tau2 = sum(r1 * st) - n1 * sum(n * st)/nt
    tau3 = sum(r1 * st) - n1 * mean(st, weights = n)
    tau4 = n1 * (mean(st, weights = r1) - mean(st, weights = n))
    return(tau4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
