\name{calcblim}
\alias{calcblim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calcblim(bl, ul)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bl}{
%%     ~~Describe \code{bl} here~~
}
  \item{ul}{
%%     ~~Describe \code{ul} here~~
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
function (bl, ul) 
{
    mlim = slim = numeric(0)
    num = length(bl)
    if (length(bl[[1]][[1]]) != 1) {
        for (k in 1:num) {
            bk = bl[[k]]
            mlim = range(c(mlim, bk[[1]]))
            slim = range(c(slim, bk[[2]]))
        }
    }
    num = length(ul)
    if (length(ul[[1]][[1]]) != 1) {
        for (k in 1:num) {
            bk = ul[[k]]
            mlim = range(c(mlim, bk[[1]], bk[[3]]))
            slim = range(c(slim, bk[[2]], bk[[4]]))
        }
    }
    a = list(mlim, slim)
    names(a) = c("mlim", "slim")
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
