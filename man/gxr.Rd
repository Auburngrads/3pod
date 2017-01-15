\name{gxr}
\alias{gxr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gxr(x, mu, sig, reso, ln, iset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
  \item{iset}{
%%     ~~Describe \code{iset} here~~
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
function (x, mu, sig, reso, ln, iset) 
{
    if (iset > 0) 
        set.seed(iset)
    xsav = x
    if (ln) 
        x = exp(x)
    rx = round(x, 5)
    if (reso > 0) 
        rx = round(x/reso) * reso
    if (ln) 
        xx = rlnorm(1, meanlog = mu, sdlog = sig)
    else xx = mu + rnorm(1) * sig
    r = 0
    if (xx <= rx) 
        r = 1
    tx = round(xx, 5)
    if (ln) 
        x = log(x)
    return(c(xsav, r, rx, tx))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
