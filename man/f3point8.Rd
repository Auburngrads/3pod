\name{f3point8}
\alias{f3point8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f3point8(l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
%%     ~~Describe \code{l} here~~
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
function (l) 
{
    if (l <= 0 | l == 1) 
        return(0)
    x = 0
    del = 0.2
    h = 1
    v = f38(x, l)
    if (v < 0) {
        while (h > 0) {
            ll = x
            x = x + del
            h = f38(x, l) * v
        }
        ul = x
    }
    if (v > 0) {
        while (h > 0) {
            ul = x
            x = x - del
            h = f38(x, l) * v
        }
        ll = x
    }
    eps = 1e-06
    w = 10
    m = (ll + ul)/2
    while (abs(w) > eps) {
        w = f38(m, l)
        if (w < 0) 
            ll = m
        else ul = m
        m = (ll + ul)/2
    }
    m = (ll + ul)/2
    return(m)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
