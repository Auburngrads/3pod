\name{skewL}
\alias{skewL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
skewL(c1, nu, tau2, p, be)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c1}{
%%     ~~Describe \code{c1} here~~
}
  \item{nu}{
%%     ~~Describe \code{nu} here~~
}
  \item{tau2}{
%%     ~~Describe \code{tau2} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{be}{
%%     ~~Describe \code{be} here~~
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
function (c1, nu, tau2, p, be) 
{
    j = qnorm(p) + be * nu
    k = sqrt(1 + be^2 * tau2)
    v = pnorm(j/k)
    u = be * tau2 * dnorm(j/k)/k + nu * v
    a = (u - nu * v)/(v * (1 - v))
    ntau2 = a^2 * v * (1 - v) - 2 * a * (u - nu * v) + tau2
    nnu = sqrt(ntau2) * c1
    b = v - (nu - nnu)/a
    return(c(j, k, v, u, a, ntau2, nnu, b))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
