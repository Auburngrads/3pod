\name{ptest}
\alias{ptest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ptest(dat, plt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{plt}{
%%     ~~Describe \code{plt} here~~
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
function (dat, plt) 
{
    if (plt == 1) {
        pdat1(dat)
        return()
    }
    if (plt == 2) {
        v = pdat2(dat)
        return(v)
    }
    if (plt == 3) {
        pdat3(dat)
        return()
    }
    if (plt == 4) {
        picdat(dat)
        return()
    }
    if (plt == 5) {
        v = jlrcb(dat)
        return(v)
    }
    if (plt == 6) {
        v = lrcb(dat)
        return(v)
    }
    if (plt == 7) {
        v = cbs(dat, plt)
        return(v)
    }
    if (plt == 8) {
        v = cbs(dat, plt)
        return(v)
    }
    u = paste("plt must be 1, 2, 3, 4, 5, 6, 7 or 8.\nTry again.\n\n", 
        sep = "")
    cat(u)
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
