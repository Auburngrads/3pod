\name{yinfomat}
\alias{yinfomat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
yinfomat(dat, mu, sig)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{sig}{
%%     ~~Describe \code{sig} here~~
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
function (dat, mu, sig) 
{
    n = dat$COUNT
    k = (dat$X - mu)/sig
    p = pnorm(k) * (1 - pnorm(k))
    z = dnorm(k)
    v = n * z^2/p
    v[which(v == Inf)] = 0
    iy = which(is.na(v))
    if (length(iy) > 0) {
        v = v[-iy]
        k = k[-iy]
    }
    b11 = sum(v)
    b21 = b12 = sum(v * k)
    b22 = sum(v * k^2)
    infm = matrix(c(b11, b12, b21, b22), nrow = 2, byrow = T)
    deti = det(infm)
    vcov1 = solve(infm)
    rho = vcov1[1, 2]/sqrt(vcov1[1, 1] * vcov1[2, 2])
    xx = list(vcov1, infm, deti, rho)
    names(xx) = c("vcov1", "infm", "deti", "rho")
    return(xx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
