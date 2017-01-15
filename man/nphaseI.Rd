\name{nphaseI}
\alias{nphaseI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nphaseI(dat0, mlo, mhi, sg, reso, about, titl, unit, ln)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat0}{
%%     ~~Describe \code{dat0} here~~
}
  \item{mlo}{
%%     ~~Describe \code{mlo} here~~
}
  \item{mhi}{
%%     ~~Describe \code{mhi} here~~
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
function (dat0, mlo, mhi, sg, reso, about, titl, unit, ln) 
{
    cnam = c("X", "Y", "COUNT", "RX", "EX", "TX", "ID")
    d1 = data.frame(t(rep(0, 6)))
    d1 = cbind(d1, "END")
    d0 = d1[-1, ]
    names(d0) = names(d1) = cnam
    d0$ID = as.character(d0$ID)
    if (is.null(dat0)) 
        dat0 = d0
    del = (mhi - mlo)/6
    eps = 1e-07
    n = 0
    endi = 0
    bl = c("B0", "B1", "B2", "B3", "B4")
    lf = 0
    while (endi == 0) {
        if (n == 0) 
            block = 0
        else {
            j = n.update(d0)
            k0 = j$n0
            k1 = j$n1
            xlo = min(d0$X)
            xhi = max(d0$X)
            if (k1 <= eps) 
                block = 1
            else {
                if (k1 >= n - eps) 
                  block = 2
                else {
                  j = m.update(d0)
                  m1 = j$m1
                  M0 = j$M0
                  dif = m1 - M0
                  dif = round(m1 - M0, 14)
                  if (dif > sg) 
                    block = 3
                  else {
                    if (dif >= 0) 
                      block = 4
                    else block = 5
                  }
                }
            }
        }
        if (block == 0) 
            if (!ln) 
                xbef = (mlo + mhi)/2
            else {
                v = ifg(mlo, mhi)
                xbef = log((v[1] + v[2])/2)
                lf = 1
            }
        if (block == 1) 
            if (lf == 0) 
                xbef = max(c((mhi + xhi)/2, xhi + 2 * sg, 2 * 
                  xhi - xlo))
            else {
                xbef = log((v[1] + 3 * v[2])/4)
                lf = 0
            }
        if (block == 2) 
            if (lf == 0) 
                xbef = min(c((mlo + xlo)/2, xlo - 2 * sg, 2 * 
                  xlo - xhi))
            else {
                xbef = log((3 * v[1] + v[2])/4)
                lf = 0
            }
        if (block == 3) 
            xbef = (m1 + M0)/2
        if (block == 4) {
            m = (m1 + M0)/2
            es = sg
            sg = 0.8 * sg
            m = max(xlo, min(m, xhi))
            es = min(es, (xhi - xlo))
            v = ykpm(d0, m, es)
            xbef = v$xstar
        }
        if (block == 5) {
            about = chabout(about, nrow(d0), 5)
            break
        }
        u = getd0(xbef, d0, dat0, bl[block + 1], reso, about, 
            titl, unit, ln)
        d0 = u$d0
        dat0 = u$dat0
        endi = u$endi
        n = nrow(d0)
    }
    ret = list(d0, dat0, endi, sg)
    nret = c("d0", "dat0", "endi", "sg")
    names = nret
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
