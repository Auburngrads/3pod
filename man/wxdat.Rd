\name{wxdat}
\alias{wxdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
wxdat(ic)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ic}{
%%     ~~Describe \code{ic} here~~
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
function (ic) 
{
    ad = ""
    switch(ic, {
        titl1 = "My SenTest Ex."
        xx = c(13, 15, 14, 15.814, 14.5, 13.2197, 15.0273, 13.6665)
        yy = c(0, 1, 0, 1, 1, 0, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "MIL-STD-331 Ex."
        xx = c(1, 1.2, 1.4, 1.8, 2.6, 4.2, 3.4, 3.8, 4, 4.1, 
            4.28, 4.52, 5.55, 5.24, 6.37, 6.08, 7.38, 7.09, 6.89, 
            6.74)
        yy = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 
            1, 1, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "No Overlap Ex."
        xx = c(14, 16, 15, 16.814, 15.5, 16.8058, 14.9669, 16.3314)
        yy = c(0, 1, 0, 1, 0, 1, 0, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "Neyer Data, n=30"
        xx = c(60, 70, 65, 68, 64, 60, 60, 68, 61, 67, 62, 70, 
            62, 69, 63, 69, 63, 63, 69, 71, 70, 70, 62, 62, 70, 
            62, 70, 61, 60, 58)
        yy = c(0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 
            0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0)
        n = rep(1, length(xx))
    }, {
        titl1 = "No ZMR, n=17"
        xx = c(23, 17, 14, 12.5, 9.75, 11.13, 12.57, 11.85, 12.21, 
            14.61, 13.41, 12.63, 11.19, 11.91, 12.66, 12.4, 12.3)
        yy = c(1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 
            0)
        n = rep(1, length(xx))
    }, {
        titl1 = "Infinite Sigma Case"
        S = 8
        xx = c(10, 12, 11, S)
        yy = c(0, 1, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "Velocity, n=15"
        xx = c(656, 900, 950, 984, 1000, 1022, 1145, 1164, 1305, 
            1313, 1450, 1457, 1500, 1625, 1750)/1000
        yy = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1)
        n = c(1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1)
        xx = rep(xx, n)
        yy = rep(yy, n)
        n = rep(1, length(xx))
    }, {
        titl1 = "VariDensity, n=24"
        xx = c(1.851, 1.8505, 1.8505, 1.553, 1.659, 1.758, 1.704, 
            1.756, 1.8, 1.659, 1.709, 1.757, 1.802, 1.704, 1.758, 
            1.7266, 1.74, 1.7472, 1.745, 1.754, 1.801, 1.7241, 
            1.708, 1.7311)
        yy = c(1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 
            0, 1, 0, 1, 1, 1, 0, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "VariGap, n=21"
        xx = c(4975, 6260, 5850, 5310, 6080, 5950, 5775, 5910, 
            5740, 5970, 5890, 5800, 5730, 5630, 5420, 5470, 5500, 
            5535, 5495, 5510, 5610)/10000
        yy = c(0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 
            0, 0, 0, 0, 0)
        n = rep(1, length(xx))
    }, {
        titl1 = "NO ZMR Example"
        xx = c(45, 23, 34, 29, 26, 31.5, 27.5, 23.5, 29.1, 25, 
            26.8, 24.9, 27.6, 26.4, 25.3, 26.9, 25.6, 25.8, 26.2)
        yy = c(1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 
            0, 0, 0)
        n = rep(1, length(xx))
    }, {
        titl1 = "Dror & Steinberg (Ref H, n=40)"
        xx = c(18, 19, 20, 21, 20, 19, 18, 19, 18, 18, 18.25, 
            18.5, 18.75, 19, 19.25, 19, 18.75, 19, 18.75, 19, 
            19.25, 19, 19.25, 19, 18.75, 19, 18.75, 19, 19.25, 
            19.5, 19.25, 19, 18.75, 18.5, 18.75, 19, 18.75, 18.5, 
            18.75, 18.5)
        yy = c(0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 
            0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 
            0, 0, 0, 0, 0, 0, 0)
        n = rep(1, length(xx))
    }, {
        titl1 = "Eli data (n=73)"
        xx = c(3.5, 4, 4, 4.5, 4.5, 5, 5, 5.5, 5.5, 6)
        fac = 1
        xx = fac * xx
        yy = c(0, 0, 1, 0, 1, 0, 1, 0, 1, 1)
        n = c(5, 23, 1, 6, 5, 2, 6, 3, 7, 15)
    }, {
        titl1 = "A Neyer Test"
        xx = c(800, 807, 884, 900, 910, 913, 923, 961, 968, 969, 
            972, 993, 1000, 1012, 1013, 1015, 1025, 1033, 1038, 
            1051, 1060, 1072, 1129, 1150, 1219)
        yy = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 
            1, 1, 0, 1, 1, 1, 1, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "JF's Data"
        xx = c(1.375, 1.53, 1.275, 1.32, 1.351, 1.334, 1.32, 
            1.34, 1.349, 1.327, 1.315, 1.344, 1.318, 1.337, 1.322, 
            1.336, 1.324, 1.345, 1.344, 1.348, 1.32, 1.376, 1.373, 
            1.371, 1.369, 1.387, 1.384, 1.382, 1.302, 1.379, 
            1.376, 1.307, 1.309, 1.285, 1.399, 1.289)
        yy = c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 
            0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 
            0, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "An n=3 Ex."
        xx = c(1, 3, 4, 5)
        yy = c(0, 1, 1, 0)
        n = rep(1, length(xx))
        xx = c(1, 3, 4)
        yy = c(0, 1, 0)
        n = rep(1, length(xx))
    }, {
        titl1 = "An n=4, con=.5 Ex."
        xx = c(1, 3, 4, 5)
        yy = c(0, 1, 1, 0)
        n = rep(1, length(xx))
    }, {
        titl1 = "No overlap (A)"
        xx = c(14, 15, 16)
        yy = c(0, 1, 1)
        tit1 = "No Overlap (n=3) Ex."
        n = rep(1, length(xx))
    }, {
        titl1 = "No overlap (B)"
        xx = c(14, 16)
        yy = c(0, 1)
        tit1 = "No Overlap (n=2) Ex."
        n = rep(1, length(xx))
    }, {
        titl1 = "No overlap (C)"
        xx = c(13, 14, 15, 16)
        yy = c(0, 0, 1, 1)
        tit1 = "No Overlap (n=4) Ex."
        n = rep(1, length(xx))
    }, {
        titl1 = "One point overlap (A)"
        xx = c(14, 14)
        yy = c(0, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "One point overlap (B)"
        xx = c(14, 16, 16)
        yy = c(0, 0, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "One point overlap (C)"
        xx = c(14, 16, 16, 16)
        yy = c(0, 0, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "One point overlap (D)"
        xx = c(14, 16, 16, 18)
        yy = c(0, 0, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "One point overlap (E)"
        xx = c(14, 16, 16, 16, 16)
        yy = c(0, 0, 0, 1, 1)
        n = rep(1, length(xx))
    }, {
        titl1 = "A Simulated Test"
        xx = c(5.5, 16.5, 9.52628, 7.23841, 6.5879, 10.46693, 
            6.79797, 10.14349, 8.30392, 6.94172, 8.65883, 7.03924, 
            7.4779, 6.81379, 8.16104, 8.8673, 9.73525, 9.5778, 
            10.63076, 10.45669, 11.68522, 23.80358, 19.33242, 
            17.14054, 15.87435, 15.04495, 14.45109, 17.11908, 
            16.67776, 16.31401, 16.0065, 15.74132, 15.50899, 
            15.30279, 15.1178)
        yy = c(0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 
            1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 
            1, 1)
        n = rep(1, length(xx))
    })
    titl1 = paste(ic, ad, ". ", titl1, sep = "")
    dat = matrix(c(xx, yy, n), ncol = 3)
    dat = data.frame(dat)
    names(dat) = c("X", "Y", "COUNT")
    w = list(dat, titl1)
    names(w) = c("d0", "title")
    g = lrmax(w, plt = T)
    return(g)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
