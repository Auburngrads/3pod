\name{pI1}
\alias{pI1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pI1(mlo, mhi, sg, tmu, tsig, reso, ln, iseed, dat0 = data.frame(numeric(0)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mlo}{
%%     ~~Describe \code{mlo} here~~
}
  \item{mhi}{
%%     ~~Describe \code{mhi} here~~
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
  \item{dat0}{
%%     ~~Describe \code{dat0} here~~
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
function (mlo, mhi, sg, tmu, tsig, reso, ln, iseed, dat0 = data.frame(numeric(0))) 
{
    nret = c("d0", "dat0")
    cnam = c("X", "Y", "COUNT", "RX", "EX", "TX", "ID")
    d1 = data.frame(t(rep(0, 6)))
    d1 = cbind(d1, "END")
    d0 = d1[-1, ]
    names(d0) = names(d1) = cnam
    d0$ID = as.character(d0$ID)
    if (is.null(dat0)) 
        dat0 = d0
    del = (mhi - mlo)/6
    epsi = del/1000
    mi = c(mlo, mhi)
    a = matrix(c(0.75, 0.25, 0.25, 0.75), ncol = 2, byrow = T)
    xx = t(a \%*\% mi)
    for (i in 1:2) {
        u = gd0(xx[i], d0, dat0, "I1", tmu, tsig, reso, ln, iseed)
        d0 = u$d0
        dat0 = u$dat0
    }
    i1 = 0
    x = d0$X
    y = d0$Y
    if (all(y == c(0, 0))) {
        xx = mi[2]
        while (1) {
            i1 = i1 + 1
            if (i1\%\%3 == 0) 
                sg = 2 * sg
            xx = xx + 1.5 * i1 * sg
            u = gd0(xx, d0, dat0, "I1(i)", tmu, tsig, reso, ln, 
                iseed)
            d0 = u$d0
            dat0 = u$dat0
            if (d0$Y[nrow(d0)] == 1) 
                break
        }
    }
    if (all(y == c(1, 1))) {
        xx = mi[1]
        while (1) {
            i1 = i1 + 1
            if (i1\%\%3 == 0) 
                sg = 2 * sg
            xx = xx - 1.5 * i1 * sg
            u = gd0(xx, d0, dat0, "I1(ii)", tmu, tsig, reso, 
                ln, iseed)
            d0 = u$d0
            dat0 = u$dat0
            if (d0$Y[nrow(d0)] == 0) 
                break
        }
    }
    if (all(y == c(0, 1))) 
        d0$ID = rep("I1(iii)", length(y))
    if (all(y == c(1, 0))) {
        xx = c(mlo - 3 * sg, mhi + 3 * sg)
        for (i in 1:2) {
            u = gd0(xx[i], d0, dat0, "I1(iv)", tmu, tsig, reso, 
                ln, iseed)
            d0 = u$d0
            dat0 = u$dat0
        }
    }
    ret = list(d0, dat0)
    names(ret) = nret
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
