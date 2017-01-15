\name{spIIIsim}
\alias{spIIIsim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spIIIsim(d0, dat0, tmu, tsig, n3, p, lam, reso, ln, iseed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d0}{
%%     ~~Describe \code{d0} here~~
}
  \item{dat0}{
%%     ~~Describe \code{dat0} here~~
}
  \item{tmu}{
%%     ~~Describe \code{tmu} here~~
}
  \item{tsig}{
%%     ~~Describe \code{tsig} here~~
}
  \item{n3}{
%%     ~~Describe \code{n3} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{lam}{
%%     ~~Describe \code{lam} here~~
}
  \item{reso}{
%%     ~~Describe \code{reso} here~~
}
  \item{ln}{
%%     ~~Describe \code{ln} here~~
}
  \item{iseed}{
%%     ~~Describe \code{iseed} here~~
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
function (d0, dat0, tmu, tsig, n3, p, lam, reso, ln, iseed) 
{
    jvec = matrix(rep(0, 10 * (n3 + 1)), ncol = 10)
    nq = glmmle(d0)
    mu = nq$mu
    sig = nq$sig
    ww = yinfomat(d0, mu, sig)
    tau2 = sum(t(c(1, qnorm(p)^2)) * diag(ww$vcov1))
    ti = round((c(3, 5)/qnorm(0.975))^2, 4)
    if (ln) 
        ti = round((c(3, 5)/qlnorm(0.975))^2, 4)
    tau2 = min(max(tau2[1], ti[1]), ti[2])
    m1 = min(d0$X, na.rm = T)
    m2 = max(d0$X, na.rm = T)
    m2 = min(c(mu, m2), na.rm = T)
    mut = max(c(m1, m2), na.rm = T)
    sigt = min(sig, diff(range(d0$X)), na.rm = T)
    be = sig/(2 * sigt)
    if (ln) 
        be = (plnorm(qlnorm(p)) * sig)/(pnorm(qnorm(p)) * sigt)
    c1 = f3point8(lam)
    nu = sqrt(tau2) * c1
    xx = mut + qnorm(p) * sigt + nu
    u = gd0(xx, d0, dat0, "III1", tmu, tsig, reso, ln, iseed)
    d0 = u$d0
    dat0 = u$dat0
    ny = length(d0$Y)
    yy = d0$Y[ny]
    jvec[1, ] = c(0, 0, 0, 0, 0, tau2, nu, 0, xx, yy)
    for (i in 1:n3) {
        vv = skewL(c1, nu, tau2, p, be)
        a = vv[5]
        tau2 = vv[6]
        nu = vv[7]
        b = vv[8]
        xx = d0$X[nrow(d0)] - a * (d0$Y[nrow(d0)] - b) * sig
        if (i < n3) {
            u = gd0(xx, d0, dat0, "III2", tmu, tsig, reso, ln, 
                iseed)
            d0 = u$d0
            dat0 = u$dat0
            ny = length(d0$Y)
            yy = d0$Y[ny]
            jvec[i + 1, ] = c(vv, xx, yy)
        }
        if (i == n3) {
            d0 = rbind(d0, d0[nrow(d0), ])
            d0[nrow(d0), 1:6] = c(0, 0, 0, round(xx, 5), 0, 0)
            d0$ID[nrow(d0)] = "III3"
            jvec[i + 1, ] = c(vv, xx, NA)
        }
    }
    jvec = data.frame(jvec)
    names(jvec) = c("j", "k", "v", "u", "a", "tau2", "nu", "b", 
        "x", "y")
    ret = list(d0, dat0, jvec)
    names(ret) = c("d0", "dat0", "jvec")
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
