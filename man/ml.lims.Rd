\name{ml.lims}
\alias{ml.lims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ml.lims(dat, conf, maxitt = 10, response = 1, rd = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{conf}{
%%     ~~Describe \code{conf} here~~
}
  \item{maxitt}{
%%     ~~Describe \code{maxitt} here~~
}
  \item{response}{
%%     ~~Describe \code{response} here~~
}
  \item{rd}{
%%     ~~Describe \code{rd} here~~
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
function (dat, conf, maxitt = 10, response = 1, rd = 6) 
{
    x = rep(dat$X, dat$COUNT)
    y = rep(dat$Y, dat$COUNT)
    if (response == 0) 
        y = abs(y - 1)
    xglm = glm(y ~ x, family = binomial(link = probit), maxit = maxitt, 
        epsilon = 1e-06)
    ab = as.vector(xglm$coef)
    muhat = -ab[1]/ab[2]
    sighat = 1/ab[2]
    a1 = c(1, 10, 100, 1000, 10000, 1e+05, 250000)/1e+06
    al = c(a1, 0.5, sort(1 - a1))
    qp = qnorm(al)
    vcov1 = yinfomat(dat, muhat, sighat)$vcov1 * sighat^2
    varq = vcov1[1, 1] + qp * (qp * vcov1[2, 2] + 2 * vcov1[1, 
        2])
    zscore = qnorm((1 + conf)/2)
    zdel = zscore * sqrt(varq)
    q0 = muhat + qnorm(al) * sighat
    qlo = muhat + qp * sighat - zdel
    qhi = muhat + qp * sighat + zdel
    dpda = 1/sqrt(2 * pi) * exp(-0.5 * qp^2)/sighat
    varp = dpda^2 * (vcov1[1, 1] + 2 * qp * vcov1[1, 2] + qp^2 * 
        vcov1[2, 2])
    zdel = zscore * sqrt(varp)
    plo = al - zdel
    plo[which(plo < 0)] = 0
    phi = al + zdel
    phi[which(phi > 1)] = 1
    qlo = round(qlo, rd)
    q0 = round(q0, rd)
    qhi = round(qhi, rd)
    plo = round(plo, rd)
    phi = round(phi, rd)
    return(matrix(c(qlo, q0, qhi, plo, al, phi), ncol = 6))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
