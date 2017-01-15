\name{prd0}
\alias{prd0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prd0(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
%%     ~~Describe \code{z} here~~
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
function (z) 
{
    d00 = z$d0
    en = z$en
    pp = z$p
    llam = z$lam
    n2n3 = z$n2n3
    n = cumsum(en)
    n0 = nrow(d00)
    ln = z$ln
    u = d00$X
    if (ln) 
        u = round(exp(u), 5)
    cat(paste("Enter title (without quotes): ", z$title, "\n", 
        sep = ""))
    cat(paste("Enter units (without quotes): ", z$units, "\n\n", 
        sep = ""))
    if (n0 == 0) 
        return(n2n3)
    for (i in 1:n0) {
        buf = " "
        if (i > 9) 
            buf = ""
        xx = paste(buf, i, ". Test at X ~ ", d00$RX[i], ". Enter X & R: ", 
            u[i], " ", d00$Y[i], "\n", sep = "")
        cat(xx)
        if (i == en[1] & (en[2] != 0 | n2n3 == 2)) {
            gg = glmmle(d00[1:n[1], ])
            g1 = round(gg$mu, 5)
            g2 = round(gg$sig, 5)
            xx = paste("\nPhase I complete, (Mu, Sig) = (", g1, 
                ", ", g2, ").\n", sep = "")
            cat(xx)
            if (en[2] == 0 & i == en[1] & n2n3 == 2) {
                xx = paste("Enter Phase II (D-Optimal) size n2: ", 
                  en[2], "\n", sep = "")
                cat(xx)
            }
            if (en[2] == 0 & en[3] > 0) {
                xx = paste("Enter Phase II (D-Optimal) size n2: ", 
                  en[2], "\n", sep = "")
                cat(xx)
            }
            if (en[2] > 0) {
                xx = paste("Enter Phase II (D-Optimal) size n2: ", 
                  en[2], "\n\n", sep = "")
                cat(xx)
            }
        }
        if (en[2] != 0 & i == n[2]) {
            if (en[3] > 0) {
                gg = glmmle(d00[1:n[2], ])
                g1 = round(gg$mu, 5)
                g2 = round(gg$sig, 5)
                xx = paste("\nPhase II complete, (Mu, Sig) = (", 
                  g1, ", ", g2, ").\n", sep = "")
                cat(xx)
            }
            i7 = 0
            if (en[3] > 0) {
                xx = paste("Enter Phase III (RMJ) size n3: ", 
                  en[3], "\n", sep = "")
                cat(xx)
                i7 = 1
            }
            if (pp > 0 & pp < 1) {
                xx = paste("Enter Phase III (RMJ) size n3: ", 
                  en[3], "\n", sep = "")
                if (i7 == 0) 
                  cat(xx)
                xx = paste("Enter p lam: ", pp, " ", llam, "\n\n", 
                  sep = "")
                cat(xx)
            }
        }
        if (en[2] == 0 & en[3] > 0 & i >= n[2] & n2n3 != 5) {
            gg = glmmle(d00[1:n[2], ])
            g1 = round(gg$mu, 5)
            g2 = round(gg$sig, 5)
            xx = paste("\nPhase I complete, (Mu, Sig) = (", g1, 
                ", ", g2, ").\n", sep = "")
            cat(xx)
            xx = paste("Enter Phase II (D-Optimal) size n2: ", 
                en[2], "\n", sep = "")
            cat(xx)
            xx = paste("\n\nPhase II skipped, (Mu, Sig) = (", 
                g1, ", ", g2, ").\n", sep = "")
            cat(xx)
            xx = paste("Enter Phase III (RMJ) size n3: ", en[3], 
                "\n", sep = "")
            cat(xx)
            if (n2n3 == 4) {
                xx = paste("Enter p lam: ", pp, " ", llam, "\n\n", 
                  sep = "")
                cat(xx)
            }
            n2n3 = 5
        }
    }
    return(n2n3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
