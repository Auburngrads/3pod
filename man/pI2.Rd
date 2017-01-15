\name{pI2}
\alias{pI2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pI2(d0, dat0, sg, tmu, tsig, reso, ln, iseed)
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
    nret = c("d0", "dat0", "sg")
    idii = ""
    while (1) {
        j = m.update(d0)
        m1 = j$m1
        M0 = j$M0
        del = m1 - M0
        while (del >= 1.5 * sg) {
            xx = (M0 + m1)/2
            id = paste(idii, "I2(ib)", sep = "")
            u = gd0(xx, d0, dat0, id, tmu, tsig, reso, ln, iseed)
            d0 = u$d0
            dat0 = u$dat0
            j = m.update(d0)
            m1 = j$m1
            M0 = j$M0
            del = m1 - M0
        }
        if (del < 0) {
            ret = list(d0, dat0, sg)
            names = nret
            return(ret)
        }
        j = n.update(d0)
        n0 = j$n0
        n1 = j$n1
        if (del >= 0) {
            if (n0 > n1) {
                xx = m1 + 0.3 * sg
                id = paste(idii, "I2(ic)", sep = "")
                u = gd0(xx, d0, dat0, id, tmu, tsig, reso, ln, 
                  iseed)
                d0 = u$d0
                dat0 = u$dat0
                if (d0$Y[nrow(d0)] == 0) {
                  ret = list(d0, dat0, sg)
                  names = nret
                  return(ret)
                }
                if (d0$Y[nrow(d0)] == 1) {
                  xx = M0 - 0.3 * sg
                  id = paste(idii, "I2(ic)", sep = "")
                  u = gd0(xx, d0, dat0, id, tmu, tsig, reso, 
                    ln, iseed)
                  d0 = u$d0
                  dat0 = u$dat0
                  if (d0$Y[nrow(d0)] == 1) {
                    ret = list(d0, dat0, sg)
                    names = nret
                    return(ret)
                  }
                  if (d0$Y[nrow(d0)] == 0) {
                    sg = 2 * sg/3
                    idii = paste(idii, "r", sep = "")
                  }
                }
            }
            if (n0 <= n1) {
                xx = M0 - 0.3 * sg
                id = paste(idii, "I2(id)", sep = "")
                u = gd0(xx, d0, dat0, id, tmu, tsig, reso, ln, 
                  iseed)
                d0 = u$d0
                dat0 = u$dat0
                if (d0$Y[nrow(d0)] == 1) {
                  ret = list(d0, dat0, sg)
                  names = nret
                  return(ret)
                }
                if (d0$Y[nrow(d0)] == 0) {
                  xx = m1 + 0.3 * sg
                  id = paste(idii, "I2(id)", sep = "")
                  u = gd0(xx, d0, dat0, id, tmu, tsig, reso, 
                    ln, iseed)
                  d0 = u$d0
                  dat0 = u$dat0
                  if (d0$Y[nrow(d0)] == 0) {
                    ret = list(d0, dat0, sg)
                    names = nret
                    return(ret)
                  }
                  if (d0$Y[nrow(d0)] == 1) {
                    sg = 2 * sg/3
                    idii = paste(idii, "r", sep = "")
                  }
                }
            }
        }
    }
    ret = list(d0, dat0, sg)
    names(ret) = nret
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
