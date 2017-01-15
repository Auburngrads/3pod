\name{clim}
\alias{clim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clim(rx, ry, m, s, uu, levb)
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
  \item{uu}{
%%     ~~Describe \code{uu} here~~
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
function (rx, ry, m, s, uu, levb) 
{
    done = 0
    sigmax = 0.001
    xll = c(-5, 5)
    yll = 10
    len = 50
    z0 = matrix(rep(0, len^2), ncol = len)
    while (done == 0) {
        xl = m + xll * s
        yl = c(sigmax, yll * s)
        x0 = seq(xl[1], xl[2], length = len)
        y0 = seq(yl[1], yl[2], length = len)
        for (i in 1:len) for (j in 1:len) z0[i, j] = xyllik(rx, 
            ry, x0[i], y0[j])/uu
        cl = contourLines(x0, y0, z0, levels = levb)
        ncl = length(cl)
        if (ncl > 0) {
            nxl = nyl = numeric(0)
            for (i in 1:ncl) {
                g = cl[[i]]
                nxl = range(c(nxl, g$x))
                nyl = range(c(nyl, g$y))
            }
            done = 1
            if (nxl[1] == x0[1]) {
                xll[1] = 1.5 * xll[1]
                done = 0
            }
            if (nxl[2] == x0[len]) {
                xll[2] = 1.5 * xll[2]
                done = 0
            }
            if (nyl[1] == y0[1]) {
                sigmax = sigmax/1.5
                done = 0
            }
            if (nyl[2] == y0[len]) {
                yll = 1.5 * yll
                done = 0
            }
        }
        else {
            done = 0
            xll = 1.5 * xll
            yll = yll * 1.5
            sigmax = sigmax/1.5
        }
    }
    return(c(nxl, nyl))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
