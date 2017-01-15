\name{jlik}
\alias{jlik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jlik(rx, ry, levs0, ms, op, one23)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rx}{
%%     ~~Describe \code{rx} here~~
}
  \item{ry}{
%%     ~~Describe \code{ry} here~~
}
  \item{levs0}{
%%     ~~Describe \code{levs0} here~~
}
  \item{ms}{
%%     ~~Describe \code{ms} here~~
}
  \item{op}{
%%     ~~Describe \code{op} here~~
}
  \item{one23}{
%%     ~~Describe \code{one23} here~~
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
function (rx, ry, levs0, ms, op, one23) 
{
    ndeg = 361
    ang = 0
    h = 1
    if (one23 == 2) {
        d = op - ms
        d7 = (op + ms)/2
        ang = atan(d[1]/d[2]) + pi/2
    }
    if (one23 == 3) 
        h = 2
    cx = cy = tr = 2 * (0:(ndeg - 1)) * pi/(h * (ndeg - 1)) - 
        ang
    rl = 0
    if (one23 == 1) {
        ibot = 1
        itop = ndeg
        x0 = as.vector(ms[1])
        y0 = ru = as.vector(ms[2])
    }
    if (one23 == 2) {
        ibot = 2
        itop = ndeg - 1
        x0 = as.vector(d7[1])
        y0 = ru = as.vector(d7[2])
        cx[1] = cx[ndeg] = ms[1]
        cy[1] = cy[ndeg] = ms[2]
    }
    if (one23 == 3) {
        ibot = 2
        itop = ndeg - 1
        x0 = ms[1]
        y0 = 0
        ru = 1
        cx[1] = op[1]
        cx[ndeg] = op[2]
        cy[1] = cy[ndeg] = 0
    }
    eps = 1e-06
    for (i in ibot:itop) {
        st = sin(tr[i])
        ct = cos(tr[i])
        xl = x0 + rl * ct
        yl = y0 + rl * ct
        xu = x0 + ru * ct
        yu = y0 + ru * st
        while (xyllik(rx, ry, xu, yu) > levs0) {
            xu = xu + ct
            yu = yu + st
        }
        x = (xl + xu)/2
        y = (yl + yu)/2
        lval = xyllik(rx, ry, x, y)
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
            lval = xyllik(rx, ry, x, y)
            zz = abs(lval - levs0)
        }
        cx[i] = (xl + xu)/2
        cy[i] = (yl + yu)/2
    }
    return(list(cx, cy))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
