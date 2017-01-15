\name{phaseI2}
\alias{phaseI2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
phaseI2(d0, dat0, sg, reso, about, titl, unit, ln)
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
function (d0, dat0, sg, reso, about, titl, unit, ln) 
{
    nret = c("d0", "dat0", "endi", "sg")
    endi = 0
    idii = ""
    while (1) {
        j = m.update(d0)
        m1 = j$m1
        M0 = j$M0
        del = m1 - M0
        while (del >= 1.5 * sg & endi == 0) {
            if (endi == 0) {
                xx = (M0 + m1)/2
                id = paste(idii, "I2(ib)", sep = "")
                u = getd0(xx, d0, dat0, id, reso, about, titl, 
                  unit, ln)
                d0 = u$d0
                dat0 = u$dat0
                endi = u$endi
                j = m.update(d0)
                m1 = j$m1
                M0 = j$M0
                del = m1 - M0
            }
        }
        if (del < 0 | endi == 1) {
            ret = list(d0, dat0, endi, sg)
            names = nret
            return(ret)
        }
        j = n.update(d0)
        n0 = j$n0
        n1 = j$n1
        if (del >= 0 & endi == 0) {
            if (n0 > n1 & endi == 0) {
                xx = m1 + 0.3 * sg
                id = paste(idii, "I2(ic)", sep = "")
                u = getd0(xx, d0, dat0, id, reso, about, titl, 
                  unit, ln)
                d0 = u$d0
                dat0 = u$dat0
                endi = u$endi
                if (d0$Y[nrow(d0)] == 0 | endi == 1) {
                  ret = list(d0, dat0, endi, sg)
                  names = nret
                  return(ret)
                }
                if (d0$Y[nrow(d0)] == 1 & endi == 0) {
                  xx = M0 - 0.3 * sg
                  id = paste(idii, "I2(ic)", sep = "")
                  u = getd0(xx, d0, dat0, id, reso, about, titl, 
                    unit, ln)
                  d0 = u$d0
                  dat0 = u$dat0
                  endi = u$endi
                  if (d0$Y[nrow(d0)] == 1 | endi == 1) {
                    ret = list(d0, dat0, endi, sg)
                    names = nret
                    return(ret)
                  }
                  if (d0$Y[nrow(d0)] == 0 & endi == 0) {
                    sg = 2 * sg/3
                    idii = paste(idii, "r", sep = "")
                  }
                }
            }
            if (n0 <= n1 & endi == 0) {
                xx = M0 - 0.3 * sg
                id = paste(idii, "I2(id)", sep = "")
                u = getd0(xx, d0, dat0, id, reso, about, titl, 
                  unit, ln)
                d0 = u$d0
                dat0 = u$dat0
                endi = u$endi
                if (d0$Y[nrow(d0)] == 1 | endi == 1) {
                  ret = list(d0, dat0, endi, sg)
                  names = nret
                  return(ret)
                }
                if (d0$Y[nrow(d0)] == 0 & endi == 0) {
                  xx = m1 + 0.3 * sg
                  id = paste(idii, "I2(id)", sep = "")
                  u = getd0(xx, d0, dat0, id, reso, about, titl, 
                    unit, ln)
                  d0 = u$d0
                  dat0 = u$dat0
                  endi = u$endi
                  if (d0$Y[nrow(d0)] == 0 | endi == 1) {
                    ret = list(d0, dat0, endi, sg)
                    names = nret
                    return(ret)
                  }
                  if (d0$Y[nrow(d0)] == 1 & endi == 0) {
                    sg = 2 * sg/3
                    idii = paste(idii, "r", sep = "")
                  }
                }
            }
        }
    }
    ret = list(d0, dat0, endi, sg)
    names(ret) = nret
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
