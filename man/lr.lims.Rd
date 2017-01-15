\name{lr.lims}
\alias{lr.lims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lr.lims(dat, conf1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{conf1}{
%%     ~~Describe \code{conf1} here~~
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
function (dat, conf1) 
{
    chk = mixed(dat)
    dif = chk$dif
    muhat = chk$summ/2
    sighat = 1e-04
    min1 = chk$min1
    max0 = chk$max0
    con = chk$con
    conf2 = pchisq(qchisq(conf1, 1), 2)
    if (dif >= 0) 
        conf2 = (conf2 + 3)/4
    if (dif < 0) {
        rx = rep(dat$X, dat$COUNT)
        ry = rep(dat$Y, dat$COUNT)
        xglm = glm(ry ~ rx, family = binomial(link = probit), 
            maxit = 10, epsilon = 1e-06)
        ab = as.vector(xglm$coef)
        muhat = -ab[1]/ab[2]
        sighat = 1/ab[2]
    }
    uu = llik(dat, muhat, sighat)
    levs0 = uu + log(1 - conf2)
    levs1 = levs2 = uu - qnorm((1 - conf1)/2)^2/2
    options(warn = -1)
    degs = 180
    if (dif > 0) 
        degs = 360
    st = ct = cx = cy = tr = (0:360) * pi/degs
    x0 = as.vector(muhat)
    y0 = ru = as.vector(sighat)
    eps = 1e-06
    eps4 = 1e-04
    ibot = 2
    itop = 361
    if (dif > 0) {
        cy[1] = cy[361] = 0
        cx[1] = min1
        cx[361] = max0
        ibot = 2
        itop = 360
        ru = 1
    }
    for (i in 0:361) {
        st[i] = sin(tr[i])
        ct[i] = cos(tr[i])
    }
    for (i in 1:181) {
        xl = x0
        yl = y0
        xu = x0 + ru * ct[i]
        yu = y0 + ru * st[i]
        while (llik(dat, xu, yu) > levs0) {
            xl = xu
            yl = yu
            xu = xu + ct[i]
            yu = yu + st[i]
        }
        x = (xl + xu)/2
        y = (yl + yu)/2
        lval = llik(dat, x, y)
        zz = abs(lval - levs0)
        while (zz > eps) {
            if (lval > levs0) {
                xl = x
                yl = y
            }
            else {
                xu = x
                yu = y
            }
            x = (xl + xu)/2
            y = (yl + yu)/2
            lval = llik(dat, x, y)
            zz = abs(lval - levs0)
        }
        cx[i] = (xl + xu)/2
        cy[i] = (yl + yu)/2
    }
    for (i in 182:360) {
        dm = ct[i] * y0/st[i]
        i2 = 25
        i1 = i2 - 1
        xu = x0 + dm * (1 - i1/i2)
        yu = y0 * i1/i2
        while (llik(dat, xu, yu) > levs0) {
            i2 = i2 + 1
            xl = xu
            yl = yu
            xu = x0 + dm * (1 - i1/i2)
            yu = y0 * i1/i2
        }
        x = (xl + xu)/2
        y = (yl + yu)/2
        lval = llik(dat, x, y)
        zz = abs(lval - levs0)
        while (zz > eps) {
            if (lval > levs0) {
                xl = x
                yl = y
            }
            else {
                xu = x
                yu = y
            }
            x = (xl + xu)/2
            y = (yl + yu)/2
            lval = llik(dat, x, y)
            zz = abs(lval - levs0)
        }
        cx[i] = (xl + xu)/2
        cy[i] = (yl + yu)/2
    }
    cx = c(cx[1:181], cx[360:182])
    cy = c(cy[1:181], cy[360:182])
    cx[361] = cx[1]
    cy[361] = cy[1]
    a1 = c(1, 10, 100, 1000, 10000, 1e+05, 250000)/1e+06
    lrmat = matrix(rep(0, 90), ncol = 6)
    lrmat[, 5] = c(a1, 0.5, sort(1 - a1))
    lrmat[, 2] = muhat + qnorm(lrmat[, 5]) * sighat
    for (i in 1:15) {
        lrmat[i, 1] = min(cx + qnorm(lrmat[i, 5]) * cy, na.rm = T)
        lrmat[i, 3] = max(cx + qnorm(lrmat[i, 5]) * cy, na.rm = T)
        lrmat[i, 4] = min(pnorm((lrmat[i, 2] - cx)/cy), na.rm = T)
        lrmat[i, 6] = max(pnorm((lrmat[i, 2] - cx)/cy), na.rm = T)
    }
    ret = list(cx, cy, lrmat, muhat, sighat, dif, con)
    names(ret) = c("cx", "cy", "lrmat", "muhat", "sighat", "dif", 
        "con")
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
