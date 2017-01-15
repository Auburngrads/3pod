\name{phaseI1}
\alias{phaseI1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
phaseI1(dat0, mlo, mhi, sg, reso, about, titl, unit, ln)
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
    nret = c("d0", "dat0", "endi", "sg")
    cnam = c("X", "Y", "COUNT", "RX", "EX", "TX", "ID")
    d1 = data.frame(t(rep(0, 6)))
    d1 = cbind(d1, "END")
    d0 = d1[-1, ]
    names(d0) = names(d1) = cnam
    d0$ID = as.character(d0$ID)
    if (is.null(dat0)) 
        dat0 = d0
    endi = 0
    mi = c(mlo, mhi)
    a = matrix(c(0.75, 0.25, 0.25, 0.75), ncol = 2, byrow = T)
    xx = t(a \%*\% mi)
    for (i in 1:2) {
        u = getd0(xx[i], d0, dat0, "I1", reso, about, titl, unit, 
            ln)
        d0 = u$d0
        dat0 = u$dat0
        endi = u$endi
        if (endi == 1) 
            break
    }
    if (endi == 0) {
        x = d0$X
        y = d0$Y
        i1 = 0
        if (all(y == c(0, 0))) {
            while (1) {
                i1 = i1 + 1
                if (i1\%\%3 == 0) 
                  sg = 2 * sg
                xx = mi[2] + 1.5 * i1 * sg
                u = getd0(xx, d0, dat0, "I1(i)", reso, about, 
                  titl, unit, ln)
                d0 = u$d0
                dat0 = u$dat0
                endi = u$endi
                if (d0$Y[nrow(d0)] == 1 | endi == 1) 
                  break
            }
        }
        if (all(y == c(1, 1))) {
            while (1) {
                i1 = i1 + 1
                if (i1\%\%3 == 0) 
                  sg = 2 * sg
                xx = mi[1] - 1.5 * i1 * sg
                u = getd0(xx, d0, dat0, "I1(ii)", reso, about, 
                  titl, unit, ln)
                d0 = u$d0
                dat0 = u$dat0
                endi = u$endi
                if (d0$Y[nrow(d0)] == 0 | endi == 1) 
                  break
            }
        }
        if (all(y == c(0, 1))) 
            d0$ID = rep("I1(iii)", length(y))
        if (all(y == c(1, 0))) {
            xx = c(mlo - 3 * sg, mhi + 3 * sg)
            for (i in 1:2) {
                u = getd0(xx[i], d0, dat0, "I1(iv)", reso, about, 
                  titl, unit, ln)
                d0 = u$d0
                dat0 = u$dat0
                endi = u$endi
                if (endi == 1) 
                  break
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
