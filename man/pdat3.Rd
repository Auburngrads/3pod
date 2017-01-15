\name{pdat3}
\alias{pdat3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pdat3(dat)
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
    ln = dat$ln
    pee = dat$p
    if (is.null(about)) {
        cat("This function only works for lists created by gonogo\n\n")
        return()
    }
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
    if (kp == 0) 
        cat("ptest(z,plt=3) option requires having completed Phase I2 (i.e., achieving overlap)\n")
    if (kp > 0) {
        if (ln) 
            z = nyqrda(dtt, ln = T, response = 1, labx = unit)
        else z = nyqrda(dtt, response = 1, labx = unit)
        rmzm = round(z$mu, 3)
        rmzs = round(z$sig, 3)
        about2 = substitute(paste(titl, ", (", hat(mu), ",", 
            hat(sigma), ",n) = (", rmzm, ",", rmzs, ",", nid, 
            ")", sep = ""))
        mtext(about2, side = 3, line = 2.7, cex = 1.1)
        mtext(about1, side = 3, line = 1.4, cex = 1.1)
        mtext(about, side = 3, line = 0.3, cex = 1.1)
    }
    return()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
