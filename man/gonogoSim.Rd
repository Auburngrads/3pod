\name{gonogoSim}
\alias{gonogoSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gonogoSim(mlo, mhi, sg, n2, n3, p, lam, dm = 0, ds = 0, ln = F, plt = 0, neyer = F, iseed = -1, IIgo = T, M = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mlo}{
%%     ~~Describe \code{mlo} here~~
}
  \item{mhi}{
%%     ~~Describe \code{mhi} here~~
}
  \item{sg}{
%%     ~~Describe \code{sg} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
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
  \item{dm}{
%%     ~~Describe \code{dm} here~~
}
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{ln}{
%%     ~~Describe \code{ln} here~~
}
  \item{plt}{
%%     ~~Describe \code{plt} here~~
}
  \item{neyer}{
%%     ~~Describe \code{neyer} here~~
}
  \item{iseed}{
%%     ~~Describe \code{iseed} here~~
}
  \item{IIgo}{
%%     ~~Describe \code{IIgo} here~~
}
  \item{M}{
%%     ~~Describe \code{M} here~~
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
function (mlo, mhi, sg, n2, n3, p, lam, dm = 0, ds = 0, ln = F, 
    plt = 0, neyer = F, iseed = -1, IIgo = T, M = 1) 
{
    reso = 0
    jvec = NULL
    if (M <= 0) 
        M = 1
    sgrem = sg = M * sg
    mlo1 = mlo = M * mlo
    mhi1 = mhi = M * mhi
    dm = M * dm
    ds = M * ds
    del5 = (mhi - mlo)/6
    epsi = del5/1000
    if (sg > (mhi - mlo)/6 + epsi) {
        cat(paste("sg is too big (sg <= ", round(del5, 4), ")\nTry again\n\n", 
            sep = ""))
        return()
    }
    if (p <= 0 | p >= 1 | lam <= 0) {
        cat(paste("p must be between 0 & 1 and lambda > 0.\nTry again\n\n", 
            sep = ""))
        return()
    }
    if (n2 <= 0 | n3 <= 0) {
        cat(paste("n2 & n3 must be positive integers.\nTry again\n\n", 
            sep = ""))
        return()
    }
    savinit = c(mlo, mhi, sg)
    if (ln) {
        v = fgs(mlo, mhi, sg)
        mlo = v[1]
        mhi = v[2]
        sg = v[3]
    }
    init = c(mlo, mhi, sg)
    tmu = (mlo + mhi)/2 + dm
    tsig = sg + ds
    dat0 = data.frame(numeric(0))
    if (!neyer) {
        w = pI1(mlo, mhi, sg, tmu, tsig, reso, ln, iseed)
        d0 = w[[1]]
        dat0 = w[[2]]
        w = pI2(d0, dat0, sg, tmu, tsig, reso, ln, iseed)
        d0 = w[[1]]
        dat0 = w[[2]]
        sg = w[[3]]
        n12 = 0
        n1 = n11 = nrow(d0)
        if (IIgo) {
            w = pI3(d0, dat0, sg, tmu, tsig, reso, ln, iseed)
            d0 = w[[1]]
            dat0 = w[[2]]
            n1 = nrow(d0)
            n12 = n1 - n11
            if (n2 > 0) {
                w = pII(d0, dat0, tmu, tsig, n2, reso, ln, iseed)
                d0 = w[[1]]
                dat0 = w[[2]]
            }
            if (n3 > 0 & p * (1 - p) > 0 & lam >= 0) {
                w = spIIIsim(d0, dat0, tmu, tsig, n3, p, lam, 
                  reso, ln, iseed)
                d0 = w[[1]]
                dat0 = w[[2]]
                jvec = w[[3]]
            }
        }
    }
    else {
        w = npI(mlo, mhi, sg, tmu, tsig, reso, ln, iseed)
        d0 = w[[1]]
        dat0 = w[[2]]
        n1 = n11 = nrow(d0)
        n12 = 0
        if (IIgo) {
            if (n2 > 0) {
                w = pII(d0, dat0, tmu, tsig, n2, reso, ln, iseed)
                d0 = w[[1]]
                dat0 = w[[2]]
            }
            if (n3 > 0 & p * (1 - p) > 0 & lam >= 0) {
                w = spIIIsim(d0, dat0, tmu, tsig, n3, p, lam, 
                  reso, ln, iseed)
                d0 = w[[1]]
                dat0 = w[[2]]
                jvec = w[[3]]
            }
        }
    }
    en = c(n11, n12, n2, n3)
    v = glmmle(d0)
    ret = list(d0, tmu, tsig, v$mu, v$sig, en)
    names(ret) = c("d0", "tmu", "tsig", "mhat", "shat", "en")
    if (!IIgo | n3 == 0) 
        mat2 = mat3 = 0
    if (is.element(plt, c(1, 2, 3))) {
        abo = wabout13(M, mlo1, mhi1, sgrem, p, n11, n12, n2, 
            n3, lam, reso)
        h1 = ""
        if (ln) 
            h1 = "log "
        h2 = "3pod"
        if (neyer) 
            h2 = "Neyer"
        titl = paste(h1, h2, sep = "")
        if (M == 1) 
            uni = "X"
        else uni = paste(M, "X", sep = "")
        ret = list(d0, jvec, tmu, tsig, v$mu, v$sig, en, abo, 
            titl, uni, p, reso, ln, lam, neyer, M, dm, ds, iseed)
        names(ret) = c("d0", "jvec", "tmu", "tsig", "mhat", "shat", 
            "en", "about", "title", "units", "p", "reso", "ln", 
            "lam", "neyer", "M", "dm", "ds", "iseed")
        pStest(ret, plt)
    }
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
