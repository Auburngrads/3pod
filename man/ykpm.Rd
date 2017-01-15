\name{ykpm}
\alias{ykpm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ykpm(dat, m, es, iplot = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{es}{
%%     ~~Describe \code{es} here~~
}
  \item{iplot}{
%%     ~~Describe \code{iplot} here~~
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
function (dat, m, es, iplot = 0) 
{
    m1 = min(dat$X[which(dat$Y == 1)], na.rm = T)
    M0 = max(dat$X[which(dat$Y == 0)], na.rm = T)
    mx1 = mean(dat$X[dat$Y == 1])
    mx0 = mean(dat$X[dat$Y == 0])
    if (m1 < M0 & mx1 <= mx0) {
        kopt = 1.31525757633905 * sign(0.5 - dat$Y[nrow(dat)])
        xstar = list(m + kopt * es)
        names(xstar) = "xstar"
        return(xstar)
    }
    bb = yinfomat(dat, m, es)
    det0 = bb$deti
    b = bb$infm
    vcov1 = bb$vcov1
    b[2, 1] = b[1, 2] = (b[1, 2] + b[2, 1])/2
    k = (-4000:4000)/1000
    dk = ydeldet(k, b)
    delstar = max(dk, na.rm = T)
    ik = which(dk == delstar)
    ok = "Y"
    if (ik == 1 | ik == length(k)) 
        ok = "N"
    kstar = k[ik]
    xstar = m + kstar * es
    deti = det0 + delstar
    if (iplot != 0) {
        plot(k, dk, type = "l")
        mtext(paste("max(dk)=", round(delstar, 5), " at k=", 
            k[ik], ", deti=", round(det0 + delstar, 4), sep = ""), 
            side = 3, line = 1.2)
        points(k[ik], delstar, pch = 16, col = 3, cex = 0.8)
    }
    xx = list(det0, kstar, xstar, delstar, deti, ok, vcov1)
    names(xx) = c("det0", "kstar", "xstar", "delstar", "deti", 
        "ok", "vcov1")
    return(xx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
