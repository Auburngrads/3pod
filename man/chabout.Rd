\name{chabout}
\alias{chabout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chabout(about, s47, loc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{about}{
%%     ~~Describe \code{about} here~~
}
  \item{s47}{
%%     ~~Describe \code{s47} here~~
}
  \item{loc}{
%%     ~~Describe \code{loc} here~~
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
function (about, s47, loc) 
{
    x = about
    x1 = gsub("[|]", ",", x)
    x2 = gsub("[{]", "vv=c(", x1)
    x3 = gsub("[}]", ")", x2)
    eval(parse(text = paste(x3)))
    vv[loc] = s47
    a = wabout(vv)
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
