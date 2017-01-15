\name{simp}
\alias{simp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simp(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
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
function (dat) 
{
    xx = dat$X
    yy = dat$Y
    n = dat$COUNT
    xx = rep(xx, n)
    yy = rep(yy, n)
    sux = sort(unique(xx))
    lsx = length(sux)
    xxx = yyy = nnn = numeric(0)
    for (i in 1:lsx) {
        i1 = sum(yy[xx == sux[i]])
        if (i1 > 0) {
            xxx = c(xxx, sux[i])
            yyy = c(yyy, 1)
            nnn = c(nnn, i1)
        }
        i0 = sum(1 - yy[xx == sux[i]])
        if (i0 > 0) {
            xxx = c(xxx, sux[i])
            yyy = c(yyy, 0)
            nnn = c(nnn, i0)
        }
    }
    dat = matrix(c(xxx, yyy, nnn), ncol = 3)
    dat = data.frame(dat)
    names(dat) = c("X", "Y", "COUNT")
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
