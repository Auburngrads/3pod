\name{gd0}
\alias{gd0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gd0(xx, d0, dat0, ID, mu, sig, reso, ln, iseed = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{
%%     ~~Describe \code{xx} here~~
}
  \item{d0}{
%%     ~~Describe \code{d0} here~~
}
  \item{dat0}{
%%     ~~Describe \code{dat0} here~~
}
  \item{ID}{
%%     ~~Describe \code{ID} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
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
function (xx, d0, dat0, ID, mu, sig, reso, ln, iseed = -1) 
{
    nret = c("d0", "dat0")
    cnam = c("X", "Y", "COUNT", "RX", "EX", "TX", "ID")
    d1 = data.frame(t(rep(0, 6)))
    d1 = cbind(d1, "END")
    names(d1) = names(d0) = cnam
    d0$ID = as.character(d0$ID)
    if (is.null(dat0)) 
        dat0 = d1[-1, ]
    n0 = nrow(dat0)
    nd0 = nrow(d0) + 1
    iset = 0
    if (iseed >= 0) 
        iset = nd0 + iseed
    if (n0 == 0) {
        u = gxr(xx, mu, sig, reso, ln, iset)
        d1[1, 1:6] = c(u[1:2], 1, u[3], xx, u[4])
        d1$ID = ID
    }
    if (n0 > 0) {
        d1 = dat0[1, ]
        dat0 = dat0[-1, ]
        if (is.null(dat0)) 
            dat0 = d1[-1, ]
        n0 = nrow(dat0)
    }
    d0 = rbind(d0, d1)
    ret = list(d0, dat0)
    names(ret) = nret
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
