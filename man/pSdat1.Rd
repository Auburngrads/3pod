\name{pSdat1}
\alias{pSdat1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pSdat1(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
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
function (dat) 
{
    dt = dtt = dat$d0
    about = dat$about
    titl = dat$titl
    unit = dat$unit
    pee = dat$p
    ln = dat$ln
    neyer = dat$neyer
    tmu = dat$tmu
    tsig = dat$tsig
    M = dat$M
    dm = dat$dm
    ds = dat$ds
    iseed = dat$iseed
    rmzm = round(tmu, 4)
    rmzs = round(tsig, 4)
    if (iseed < 0) {
        titl1 = substitute(paste(titl, ": (", mu[t], ", ", sigma[t], 
            ") = (", rmzm, ", ", rmzs, "), ", delta[t], " = (", 
            dm, ", ", ds, ")", sep = ""))
    }
    else {
        titl1 = substitute(paste(titl, ": (", mu[t], ", ", sigma[t], 
            ") = (", rmzm, ", ", rmzs, "), ", delta[t], " = (", 
            dm, ", ", ds, "), ", i[seed], " = ", iseed, sep = ""))
    }
    if (length(pee) == 0) 
        pee = 0
    x = dt$X
    y = dt$Y
    id = dt$ID
    nid = length(id)
    fini = 0
    if (id[nid] == "III3") 
        fini = 1
    if (fini == 1) {
        dtt = dtt[-nid, ]
        x = x[-nid]
        y = y[-nid]
        id = id[-nid]
        nid = nid - 1
    }
    zee = tzee = x[1]
    if (pee * (1 - pee) > 0 & fini == 1) {
        yu = glmmle(dtt)
        zee = yu$mu + qnorm(pee) * yu$sig
        tzee = dat$tmu + qnorm(pee) * dat$tsig
    }
    if (M == 1) 
        about1 = expression(paste("{", mu[lo], ",", mu[hi], ",", 
            sigma[g], "|", n[11], ",", n[12], ",", n[2], ",", 
            n[3], "|p,", lambda, ",res}", sep = ""))
    else about1 = expression(paste("{", mu[lo], ",", mu[hi], 
        ",", sigma[g], "|", n[11], ",", n[12], ",", n[2], ",", 
        n[3], "|p,", lambda, ",res,M}", sep = ""))
    ens = 1:nid
    rd = which(y == 1)
    gr = which(y == 0)
    xtz = c(x, tzee, zee)
    ylm = range(pretty(c(xtz, max(xtz, na.rm = T) + diff(range(xtz))/80), 
        n = 10))
    lb = nid - 1
    if (lb > 30) 
        lb = ceiling(lb/2)
    if (nid == 1) 
        return()
    if (nid > 1) {
        par(mar = c(4, 4, 5, 2) + 0.1)
        lnum = 2.3
        if (!ln) 
            plot(c(ens, 1), c(x, zee), type = "n", xlab = "", 
                ylab = "", ylim = ylm, lab = c(lb, 5, 7))
        else {
            par(mar = c(4, 3, 5, 3) + 0.1)
            plot(c(ens, 1), c(x, zee), type = "n", xlab = "", 
                ylab = "", ylim = ylm, yaxt = "n")
            w7 = pretty(exp(x), n = 6)
            axis(2, at = log(w7), lab = round(w7, 1), srt = 90, 
                tcl = -0.4, mgp = c(1, 0.5, 0))
            w8 = pretty(x, n = 6)
            axis(4, at = w8, lab = round(w8, 1), srt = 90, tcl = -0.4, 
                mgp = c(1, 0.5, 0))
            mtext("Log Scale", side = 4, line = 1.6)
            lnum = 1.8
        }
        mtext(paste("Test Level (", unit, ")", sep = ""), side = 2, 
            line = lnum)
        mtext("Trial Number", side = 1, line = 2.2)
        points(ens[rd], x[rd], pch = 25, cex = 0.7, bg = 4)
        points(ens[gr], x[gr], pch = 24, cex = 0.7, bg = 3)
        if (neyer) 
            g7 = addneyr(dtt, ylm, sim = T)
        else g7 = add3pod(dtt, ylm, sim = T)
        kp = g7[2]
        mtext(titl1, side = 3, line = 3.4, cex = 1.2, col = 1)
        mtext(about1, side = 3, line = 1.8, cex = 1.2)
        mtext(about, side = 3, line = 0.5, cex = 1.2)
        if (fini == 1) {
            axis(4, label = F, at = dt$RX[nid + 1], tcl = 0.25, 
                lwd = 2)
            axis(4, label = F, at = zee, tcl = -0.25, lwd = 2)
            axis(4, label = F, at = tzee, tcl = -0.25, lwd = 2, 
                col = 8)
            axis(4, label = F, at = tzee, tcl = 0.25, lwd = 2, 
                col = 8)
        }
    }
    reset()
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
