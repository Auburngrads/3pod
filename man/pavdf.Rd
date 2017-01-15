\name{pavdf}
\alias{pavdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pavdf(data.df, ln, plotit = F, response = 0, lineit = F, labx = "STIMULUS", laby = "PROBABILITY OF RESPONSE", titl = "PAV SOLUTION")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.df}{
%%     ~~Describe \code{data.df} here~~
}
  \item{ln}{
%%     ~~Describe \code{ln} here~~
}
  \item{plotit}{
%%     ~~Describe \code{plotit} here~~
}
  \item{response}{
%%     ~~Describe \code{response} here~~
}
  \item{lineit}{
%%     ~~Describe \code{lineit} here~~
}
  \item{labx}{
%%     ~~Describe \code{labx} here~~
}
  \item{laby}{
%%     ~~Describe \code{laby} here~~
}
  \item{titl}{
%%     ~~Describe \code{titl} here~~
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
function (data.df, ln, plotit = F, response = 0, lineit = F, 
    labx = "STIMULUS", laby = "PROBABILITY OF RESPONSE", titl = "PAV SOLUTION") 
{
    events = data.df$Y
    if (response == 0) 
        events = 1 - data.df$Y
    trials = data.df$COUNT
    x = data.df$X
    x = rep(x, trials)
    events = rep(events, trials)
    trials = rep(1, length(x))
    k = length(events)
    if (length(x) == 0) 
        x = 1:k
    else {
        events = events[order(x)]
        trials = trials[order(x)]
        x = sort(x)
        xuniq = unique(x)
        k = length(xuniq)
        evtmp = rep(0, k)
        tritmp = rep(0, k)
        for (i in 1:k) {
            evtmp[i] = sum(events[x == xuniq[i]])
            tritmp[i] = sum(trials[x == xuniq[i]])
        }
    }
    events = evtmp
    trials = tritmp
    p = matrix(0, k, 1)
    pp = matrix(0, k, k)
    for (i in 1:k) {
        sum1 = 0
        sum2 = 0
        for (j in i:k) {
            sum1 = sum1 + events[j]
            sum2 = sum2 + trials[j]
            pp[i, j] = sum1/sum2
        }
        temp = as.matrix(pp[(1:i), (i:k)])
        p[i] = ifelse(i > 1, max(apply(temp, 1, min)), min(temp))
    }
    puniq = unique(p)
    kk = length(puniq)
    xp = rep(0, kk)
    for (i in 1:kk) {
        xp[i] = min(xuniq[p == puniq[i]])
    }
    if (ln) {
        xp = exp(xp)
        xplt = c(xp[1], rep(xp[-1], rep(2, length(xp) - 1)), 
            max(exp(x)))
    }
    else xplt = c(xp[1], rep(xp[-1], rep(2, length(xp) - 1)), 
        max(x))
    pplt = c(rep(puniq, rep(2, length(puniq))))
    if (plotit) {
        if (!lineit) 
            plot(xplt, pplt, type = "n", xlab = labx, ylab = laby, 
                main = titl)
        lines(xplt, pplt, lwd = 2)
    }
    xx = list(full = cbind(xuniq, events/trials, p), unique = cbind(xp, 
        puniq), coords = cbind(xplt, pplt))
    return(xx)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
