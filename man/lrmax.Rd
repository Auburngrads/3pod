\name{lrmax}
\alias{lrmax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lrmax(w, plt = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
%%     ~~Describe \code{w} here~~
}
  \item{plt}{
%%     ~~Describe \code{plt} here~~
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
function (w, plt = F) 
{
    dat = w$d0
    title = w$title
    rx = dat$X
    ry = dat$Y
    nc = dat$COUNT
    rx = rep(rx, nc)
    ry = rep(ry, nc)
    nt = sum(nc)
    con = sum(ry)/length(ry)
    llc = sum(log(con^ry * (1 - con)^(1 - ry)))
    r0 = rx[ry == 0]
    r1 = rx[ry == 1]
    mix = length(r0) * length(r1)
    lux = length(unique(rx))
    if (mix == 0 | lux == 1) {
        cat(paste("Need to do more testing\n", sep = ""))
    }
    M0 = max(r0)
    m1 = min(r1)
    del = m1 - M0
    one23 = 2 + sign(del)
    switch(one23, {
        xglm = glm(ry ~ rx, family = binomial(link = probit), 
            maxit = 10, epsilon = 1e-06)
        ab = as.vector(xglm$coef)
        muhat = -ab[1]/ab[2]
        sighat = 1/ab[2]
        uu = xyllik(rx, ry, muhat, sighat)
    }, {
        muhat = (m1 + M0)/2
        sighat = 0
        mx = ry[rx == m1]
        s1 = sum(mx)
        s2 = length(mx) - s1
        uu = s1 * log(s1) + s2 * log(s2) - (s1 + s2) * log(s1 + 
            s2)
    }, {
        muhat = (m1 + M0)/2
        sighat = 0
        uu = 0
    })
    c2max = pchisq(2 * (uu - llc), 2)
    c1max = pchisq(qchisq(c2max, 2), 1)
    a = c(con, llc, c1max, c2max)
    con = round(con, 5)
    llc = round(llc, 5)
    c1max = round(c1max, 5)
    c2max = round(c2max, 5)
    wx = list(dat, title, one23, con, llc, c1max, c2max)
    names(wx) = c("d0", "title", "one23", "con", "llc", "c1max", 
        "c2max")
    if (plt) 
        picdat(wx)
    return(wx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
