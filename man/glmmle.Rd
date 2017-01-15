\name{glmmle}
\alias{glmmle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
glmmle(mydata, response = 1, maxitt = 10, eps = 1e-06, lgit = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
%%     ~~Describe \code{mydata} here~~
}
  \item{response}{
%%     ~~Describe \code{response} here~~
}
  \item{maxitt}{
%%     ~~Describe \code{maxitt} here~~
}
  \item{eps}{
%%     ~~Describe \code{eps} here~~
}
  \item{lgit}{
%%     ~~Describe \code{lgit} here~~
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
function (mydata, response = 1, maxitt = 10, eps = 1e-06, lgit = F) 
{
    mydata = na.omit(mydata)
    x = rep(mydata$X, mydata$COUNT)
    y = rep(mydata$Y, mydata$COUNT)
    if (response == 0) 
        y = abs(y - 1)
    options(warn = -1)
    nmxx = c("mu", "sig", "llik", "anom", "mix", "overlap", "remo0", 
        "remo1", "bigk", "okk", "mhat", "shat", "xm", "xs")
    bigk = okk = numeric(0)
    j = m.update(mydata)
    M0 = j$M0
    m1 = j$m1
    xm = (j$M0 + j$m1)/2
    xs = (j$M0 - j$m1)
    remo0 = remo1 = 0
    if (all(y == 1) | all(y == 0)) {
        anom = T
        mix = F
        overlap = F
        mu = NA
        sig = NA
        ll = NA
        xx = list(mu, sig, ll, anom, mix, overlap, remo0, remo1, 
            bigk, okk, mu, sig, xm, xs)
        names(xx) = nmxx
        return(xx)
    }
    if (xs < 0) {
        anom = T
        mix = T
        overlap = F
        mu = xm
        sig = -xs/6
        ll = 0
        xx = list(mu, sig, ll, anom, mix, overlap, remo0, remo1, 
            bigk, okk, mu, sig, xm, xs)
        names(xx) = nmxx
        return(xx)
    }
    if (xs == 0) {
        anom = T
        mix = T
        overlap = T
        mu = xm
        sig = 0
        iw = which(x == M0)
        ll = NA
        if (M0 > 0) 
            ll = log(M0^iw)
        xx = list(mu, sig, ll, anom, mix, overlap, remo0, remo1, 
            bigk, okk, mu, sig, xm, xs)
        names(xx) = nmxx
        return(xx)
    }
    u = tauf(x, y)
    tau = u$tau
    p1 = u$p1
    if (tau <= 0) {
        anom = T
        mix = T
        overlap = T
        mu = -Inf
        sig = Inf
        iov = x >= M0 & x <= m1
        ovx = x[iov]
        nov = length(ovx)
        ll = sum(y[ovx]) * log(p1) + (nov - sum(y[ovx])) * log(1 - 
            p1)
        xx = list(mu, sig, ll, anom, mix, overlap, remo0, remo1, 
            bigk, okk, mu, sig, xm, xs)
        names(xx) = nmxx
        return(xx)
    }
    if (lgit) 
        xglm = glm(y ~ x, family = binomial(link = logit), maxit = maxitt, 
            epsilon = eps)
    else xglm = glm(y ~ x, family = binomial(link = probit), 
        maxit = maxitt, epsilon = eps)
    ab = as.vector(xglm$coef)
    anom = F
    mix = T
    overlap = T
    mu = -ab[1]/ab[2]
    sig = 1/ab[2]
    ll = llik(mydata, mu, sig)
    xx = list(mu, sig, ll, anom, mix, overlap, remo0, remo1, 
        bigk, okk, mu, sig, xm, xs)
    names(xx) = nmxx
    return(xx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
