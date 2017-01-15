\name{phaseII}
\alias{phaseII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
phaseII(d0, dat0, n2, reso, about, titl, unit, ln)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d0}{
%%     ~~Describe \code{d0} here~~
}
  \item{dat0}{
%%     ~~Describe \code{dat0} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
}
  \item{reso}{
%%     ~~Describe \code{reso} here~~
}
  \item{about}{
%%     ~~Describe \code{about} here~~
}
  \item{titl}{
%%     ~~Describe \code{titl} here~~
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
}
  \item{ln}{
%%     ~~Describe \code{ln} here~~
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
function (d0, dat0, n2, reso, about, titl, unit, ln) 
{
    xl = xu = xstar = mu2 = mu4 = sg2 = sg4 = rep(0, n2)
    for (i in 1:n2) {
        nq = glmmle(d0)
        mu2[i] = nq$mu
        sg2[i] = nq$sig
        xl[i] = min(d0$X)
        xu[i] = max(d0$X)
        mu4[i] = max(xl[i], min(mu2[i], xu[i]))
        sg4[i] = min(sg2[i], xu[i] - xl[i])
        j = ykpm(d0, mu4[i], sg4[i])
        xstar[i] = j$xstar
        id = "II1"
        if (i > 1) 
            id = "II2"
        u = getd0(xstar[i], d0, dat0, id, reso, about, titl, 
            unit, ln)
        d0 = u$d0
        dat0 = u$dat0
        endi = u$endi
        if (endi == 1) 
            break
    }
    ret = list(d0, dat0, endi)
    names = c("d0", "dat0", "endi")
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
