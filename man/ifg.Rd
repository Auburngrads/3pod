\name{ifg}
\alias{ifg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ifg(fg0, fg1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fg0}{
%%     ~~Describe \code{fg0} here~~
}
  \item{fg1}{
%%     ~~Describe \code{fg1} here~~
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
function (fg0, fg1) 
{
    m1 = 4 * exp((fg0 + 3 * fg1)/4)
    m0 = 4 * exp((3 * fg0 + fg1)/4)
    mhi = (3 * m1 - m0)/8
    mlo = (3 * m0 - m1)/8
    v = c(mlo, mhi)
    return(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
