\name{mkb0}
\alias{mkb0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mkb0(confv, nam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{confv}{
%%     ~~Describe \code{confv} here~~
}
  \item{nam}{
%%     ~~Describe \code{nam} here~~
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
function (confv, nam) 
{
    ncl = length(confv)
    b0 = round(confv, 3)
    b0 = paste(b0, collapse = ",")
    b0 = gsub(",0.", ",.", b0, fixed = T)
    b0 = gsub("0.", ".", b0, fixed = T)
    w1 = w2 = ""
    if (ncl > 1) {
        w1 = "("
        w2 = ")"
    }
    b0 = paste(nam, " = ", w1, b0, w2, sep = "")
    return(b0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
