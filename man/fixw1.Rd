\name{fixw1}
\alias{fixw1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fixw1(w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
%%     ~~Describe \code{w} here~~
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
function (w) 
{
    d0 = w$d0
    n = nrow(d0)
    about = w$about
    en = w$en
    p = w$p
    n2n3 = w$n2n3
    neyer = w$neyer
    l = about4(about)
    l0 = length(which(l == 0))
    if (l0 == 4) {
        cas = 1
        d0 = d0[-n, ]
        if (n > 0) 
            en[1] = n - 1
    }
    if (l0 == 3) {
        cas = 2
        d0 = d0[-n, ]
        en[1] = en[1] - 1
    }
    if (l0 == 2) {
        if (n > l[1] & n < l[1] + l[2]) {
            cas = 4
            d0 = d0[-n, ]
        }
        if (n == l[1] + l[2]) {
            cas = 5
            d0 = d0[-n, ]
        }
        if (n == l[1]) {
            cas = 3
            en[2] = 0
        }
    }
    if (l0 == 1) {
        cas = 6
        en[3] = 0
        n2n3 = 0
    }
    if (l0 == 0) {
        if (n == l[1] + l[2]) {
            cas = 7
            p = 0
            n2n3 = 6
        }
        if (n > l[1] + l[2] & n <= l[1] + l[2] + l[3]) {
            cas = 8
            d0 = d0[-n, ]
        }
        if (n > l[1] + l[2] + l[3]) {
            cas = 9
            d0 = d0[-n, ]
            d0 = d0[-(n - 1), ]
        }
    }
    w$d0 = d0
    w$en = en
    w$p = p
    w$n2n3 = n2n3
    nen1 = en[1]
    if (en[2] == 0) 
        nen1 = 0
    s47 = c(nen1, en[2:3], p)
    loc = 4:7
    if (cas > 1 | neyer) 
        w$about = chabout(about, s47, loc)
    rd0 = d0
    rd0$X = round(rd0$X, 5)
    names(rd0)[1] = "i,X"
    rd0$EX = round(rd0$EX, 5)
    rd0$TX = round(rd0$TX, 5)
    write.table(rd0, file = "fixw.txt", quote = F, sep = ",", 
        na = "i")
    return(w)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
