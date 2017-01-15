\name{graf1}
\alias{graf1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
graf1(limx, t1, k, big, legnd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limx}{
%%     ~~Describe \code{limx} here~~
}
  \item{t1}{
%%     ~~Describe \code{t1} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{big}{
%%     ~~Describe \code{big} here~~
}
  \item{legnd}{
%%     ~~Describe \code{legnd} here~~
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
function (limx, t1, k, big, legnd) 
{
    lw = 3
    spl = "SPlus (dose.p)"
    if (k != 1) 
        spl = "SPlus (GLM)"
    if (big == 0) 
        sp1 = "SP"
    leg = c("FM", "LR", "GLM")
    if (big != 0) 
        leg = c("Fisher Matrix", "Likelihood Ratio", spl)
    qtic = pretty(limx)
    pr = c(1, 10, 100, 1000, 5000, 9000, 9900, 9990, 9999)/10000
    q = qnorm(pr)
    xl = range(limx)
    yl = c(-3.8, 3.8)
    plot(xl, yl, type = "n", xlim = xl, ylim = yl, xaxt = "n", 
        yaxt = "n", xlab = "", ylab = "", cex = 0.8)
    xl1 = "quantile (q)"
    yl1 = "Probability of Response (p\%)"
    mtext(xl1, side = 1, line = 1.6, cex = 0.8)
    mtext(yl1, side = 2, line = 2.3, cex = 0.8)
    mtext(t1, side = 3, line = 1, cex = 0.9)
    isiz1 = 0.7
    if (big == 0) 
        isiz1 = 0.4
    axis(1, at = qtic, labels = T, tck = 0.01, cex = isiz1, mgp = c(3, 
        0.5, 0))
    axis(2, at = q, labels = paste(100 * pr, " ", sep = ""), 
        tck = 0.01, cex.axis = 0.8, mgp = c(3, 0, 0), las = 2)
    axis(3, at = qtic, labels = F, tck = 0.01, cex = isiz1)
    delx = 0.75
    ilt = 3
    for (i in 1:length(pr)) lines(xl + c(-delx, delx), c(q[i], 
        q[i]), lty = ilt)
    abline(v = qtic, lty = ilt)
    del1 = diff(range(limx))/20
    if (legnd > 0) 
        legend(xl[1] + del1, qnorm(0.997), legend = leg, lty = c(1, 
            1, 1), col = c(4, 1, 2), lwd = lw, cex = 0.6, bg = "white")
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
