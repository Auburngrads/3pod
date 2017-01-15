\name{getxr}
\alias{getxr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getxr(x, nd0, reso, ln)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{nd0}{
%%     ~~Describe \code{nd0} here~~
}
  \item{reso}{
%%     ~~Describe \code{reso} here~~
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
function (x, nd0, reso, ln) 
{
    buf = " "
    if (ln) 
        x = exp(x)
    if (nd0 > 9) 
        buf = ""
    rx = round(x, 5)
    if (reso > 0) 
        rx = round(x/reso) * reso
    xx = paste(buf, nd0, ". Test at X ~ ", rx, ". Enter X & R: ", 
        sep = "")
    xx = readline(xx)
    xx = as.numeric(unlist(strsplit(xx, " ")))
    tx = xx[1]
    if (ln) 
        xx[1] = round(log(tx), 5)
    return(c(xx, rx, tx))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
