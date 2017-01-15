\name{fgs}
\alias{fgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fgs(mlo, mhi, sg)
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
function (mlo, mhi, sg) 
{
    fg0 = log((mhi + 3 * mlo)^3/(16 * (3 * mhi + mlo)))/2
    fg1 = log((3 * mhi + mlo)^3/(16 * (mhi + 3 * mlo)))/2
    fsg = (fg1 - fg0)/7
    u = c(fg0, fg1, fsg)
    return(u)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
