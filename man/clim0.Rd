\name{clim0}
\alias{clim0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clim0(rx, ry, m, s, levb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{s}{
%%     ~~Describe \code{s} here~~
}
  \item{levb}{
%%     ~~Describe \code{levb} here~~
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
function (rx, ry, m, s, levb) 
{
    done = 0
    sigmax = 0
    len = 50
    k = 5
    xll = c(-1, 1) * k
    m1 = min(rx[ry == 1])
    M0 = max(rx[ry == 0])
    yll = k * (m1 - M0)
    if (yll == 0) 
        yll = 1
    z0 = matrix(rep(0, len^2), ncol = len)
    while (done == 0) {
        xl = m + xll * s
        yl = c(sigmax, yll * s)
        x0 = seq(xl[1], xl[2], length = len)
        y0 = seq(yl[1], yl[2], length = len)
        for (i in 1:len) for (j in 1:len) z0[i, j] = xyllik(rx, 
            ry, x0[i], y0[j])
        z0 = exp(z0)
        cl = contourLines(x0, y0, z0, levels = levb)
        ncl = length(cl)
        iplot = 0
        if (ncl > 0) {
            rxcl = rycl = numeric(0)
            for (i in 1:ncl) {
                rxcl = range(c(rxcl, cl[[i]]$x))
                rycl = range(c(rycl, cl[[i]]$y))
            }
            if (iplot == 1) {
                plot(rxcl, rycl, type = "n")
                for (i in 1:ncl) points(cl[[i]]$x, cl[[i]]$y, 
                  type = "l")
            }
        }
        if (ncl == 0 | ncl == 4) {
            done = 0
            xll = 1.5 * xll
            yll = 1.5 * yll
        }
        if (ncl == 1) {
            done = 1
            x = cl[[1]]$x
            y = cl[[1]]$y
            en = length(x)
            if (y[1] != y[en] & x[1] == xl[1]) {
                xll[1] = 1.5 * xll[1]
                done = 0
            }
            if (y[1] != y[en] & x[1] == xl[2]) {
                xll[2] = 1.5 * xll[2]
                done = 0
            }
        }
        if (ncl == 2) {
            done = 0
            if (rxcl[1] == xl[1]) 
                xll[1] = 1.5 * xll[1]
            if (rxcl[2] == xl[2]) 
                xll[2] = 1.5 * xll[2]
            if (rycl[2] == yl[2]) 
                yll = 1.5 * yll
        }
        if (ncl == 3) {
            done = 0
            yll = 1.5 * yll
            if (rxcl[1] == xl[1]) 
                xll[1] = 1.5 * xll[1]
            if (rxcl[2] == xl[2]) 
                xll[2] = 1.5 * xll[2]
        }
    }
    return(c(xl, yl))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
