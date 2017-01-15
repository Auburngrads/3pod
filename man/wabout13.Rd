\name{wabout13}
\alias{wabout13}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wabout13(M, cmlo, cmhi, csg, p, n11, n12, n2, n3, lam, reso)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{cmlo}{
%%     ~~Describe \code{cmlo} here~~
}
  \item{cmhi}{
%%     ~~Describe \code{cmhi} here~~
}
  \item{csg}{
%%     ~~Describe \code{csg} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{n11}{
%%     ~~Describe \code{n11} here~~
}
  \item{n12}{
%%     ~~Describe \code{n12} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
}
  \item{n3}{
%%     ~~Describe \code{n3} here~~
}
  \item{lam}{
%%     ~~Describe \code{lam} here~~
}
  \item{reso}{
%%     ~~Describe \code{reso} here~~
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
function (M, cmlo, cmhi, csg, p, n11, n12, n2, n3, lam, reso) 
{
    g = ", "
    f = ","
    cp = as.character(p)
    cp = gsub("0.", ".", cp)
    cl = as.character(lam)
    cl = gsub("0.", ".", cl)
    cr = as.character(reso)
    cr = gsub("0.", ".", cr)
    if (M == 1) 
        about = paste("{", cmlo, f, cmhi, f, csg, "|", n11, f, 
            n12, f, n2, f, n3, "|", cp, f, cl, f, cr, "}", sep = "")
    else about = paste("{", cmlo, f, cmhi, f, csg, "|", n11, 
        f, n12, f, n2, f, n3, "|", cp, f, cl, f, cr, f, M, "}", 
        sep = "")
    return(about)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
