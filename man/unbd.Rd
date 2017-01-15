\name{unbd}
\alias{unbd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
unbd(rx, ry, levs, mh1, mh2, es, mlim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
}
  \item{levs}{
%%     ~~Describe \code{levs} here~~
}
  \item{mh1}{
%%     ~~Describe \code{mh1} here~~
}
  \item{mh2}{
%%     ~~Describe \code{mh2} here~~
}
  \item{es}{
%%     ~~Describe \code{es} here~~
}
  \item{mlim}{
%%     ~~Describe \code{mlim} here~~
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
function (rx, ry, levs, mh1, mh2, es, mlim) 
{
    mh = (mh1 + mh2)/2
    if (length(mlim) == 2) {
        L = mlim[1]
        U = mlim[2]
    }
    else {
        pm = c(-1, 1)
        I = c(mh1, mh2) + 0.5 * mh * pm
        L = I[1]
        U = I[2]
    }
    if (es > 0) 
        S0 = ST1 = ulik(rx, ry, levs, mh1, es)
    else S0 = ST1 = 0
    num = 51
    J1 = seq(mh1, L, length = num)
    for (i in 2:num) {
        S1 = uliknext(rx, ry, levs, J1[i - 1], S0, J1[i])
        ST1 = c(S1, ST1)
        S0 = S1
    }
    if (es > 0) 
        S0 = ST2 = ulik(rx, ry, levs, mh2, es)
    else S0 = ST2 = 0
    J2 = seq(mh2, U, length = num)
    for (i in 2:num) {
        S1 = uliknext(rx, ry, levs, J2[i - 1], S0, J2[i])
        ST2 = c(ST2, S1)
        S0 = S1
    }
    mt1 = J1[num:1]
    mt2 = J2
    return(list(mt1, ST1, mt2, ST2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
