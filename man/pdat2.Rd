\name{pdat2}
\alias{pdat2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pdat2(dat)
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
    ln = dat$ln
    if (is.null(about)) {
        cat("This function only works for lists created by gonogo\n\n")
        return()
    }
    pee = dat$p
    id = dt$ID
    nid = length(id)
    if (length(pee) == 0) 
        pee = 0
    fini = 0
    if (id[nid] == "III3") 
        fini = 1
    if (fini == 1) {
        dtt = dtt[-nid, ]
        id = id[-nid]
        nid = nid - 1
    }
    if (pee * (1 - pee) > 0 & fini == 1) {
        yu = glmmle(dtt)
        zee = yu$mu + qnorm(pee) * yu$sig
    }
    about1 = expression(paste("{", mu[lo], ",", mu[hi], ",", 
        sigma[g], "|", n[1], ",", n[2], ",", n[3], "|p,", lambda, 
        ",res}", sep = ""))
    kp = 0
    for (j in 1:nid) {
        jj = m.update(dtt[1:j, ])
        M0 = jj$M0
        m1 = jj$m1
        uv = c(M0, m1)
        if (!any(is.na(uv))) {
            if (M0 > m1) 
                kp = j
        }
        if (kp > 0) 
            break
    }
    mus = sigs = zee = zee0 = rep(0, nid - kp + 1)
    if (kp == 0) 
        cat("ptest(z,plt=2) option requires having completed Phase I2 (i.e., achieving overlap)\n")
    if (kp > 0) {
        for (j in kp:nid) {
            g = glmmle(dtt[1:j, ])
            mus[j - kp + 1] = g$mu
            sigs[j - kp + 1] = g$sig
        }
        if (pee > 0 & pee < 1) 
            zee = mus + qnorm(pee) * sigs
        par(mfrow = c(2, 1), mar = c(1.5, 2.5, 0.5, 0.5), oma = c(2, 
            2, 3.5, 2))
        lmu = pretty(mus)
        lsig = pretty(sigs)
        lx = pretty(c(kp, nid))
        rx = kp:nid
        rxx = range(rx)
        if (diff(rxx) == 0) 
            rxx = rxx + c(-1, 1)
        plot(kp:nid, mus, type = "l", xlab = "", ylab = "", xlim = rxx, 
            xaxt = "n", ylim = range(lmu), yaxt = "n")
        axis(1, at = kp:nid, labels = T, tck = -0.03, mgp = c(1, 
            0.4, 0), cex.axis = 0.8)
        axis(2, at = lmu, labels = T, tck = -0.02, mgp = c(1, 
            0.4, 0), las = 2, cex.axis = 0.8)
        if (ln) 
            mtext("Mean(Log)", side = 2, line = 3, cex = 1)
        else mtext("Mean", side = 2, line = 3, cex = 1)
        lt = 3
        abline(h = lmu, lty = lt)
        abline(v = lx, lty = lt)
        points(kp:nid, mus, pch = 16, cex = 0.8)
        if (kp == nid) 
            nlx = 2
        else nlx = nid - kp
        plot(kp:nid, sigs, type = "l", xlab = "", ylab = "", 
            ylim = range(lsig), yaxt = "n", xlim = rxx, xaxt = "n")
        axis(1, at = kp:nid, labels = T, tck = -0.03, mgp = c(1, 
            0.4, 0), cex.axis = 0.8)
        axis(2, at = lsig, labels = T, tck = -0.02, mgp = c(1, 
            0.4, 0), las = 2, cex.axis = 0.8)
        mtext("Cumulative Test Size (n)", side = 1, line = 0, 
            cex = 1, outer = T)
        if (ln) 
            mtext("SD(Log)", side = 2, line = 3, cex = 1)
        else mtext("SD", side = 2, line = 3, cex = 1)
        abline(h = lsig, lty = lt)
        abline(v = lx, lty = lt)
        points(kp:nid, sigs, pch = 16, cex = 0.8)
        par(mfrow = c(1, 1))
        els = c(2.5, 1)
        mtext(paste(titl, ": Sequence of MLE's", sep = ""), side = 3, 
            line = 2.8, cex = 1.1)
        mtext(about1, side = 3, line = 1.4, cex = 1.1)
        mtext(about, side = 3, line = 0.3, cex = 1.1)
    }
    reset()
    if (pee == 0) 
        return(matrix(c(mus, sigs), ncol = 2))
    else return(matrix(c(mus, sigs, zee), ncol = 3))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
