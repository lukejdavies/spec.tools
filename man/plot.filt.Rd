\name{plot.filt}
\alias{plot.filt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot filter transmissions
}
\description{
Easily plot single or multipule filter transmission curves. Available filters are: 'FUV'=GALEX-FUV, 'NUV'=GALEX-FUV, 'u'=SDSS-u, 'g'=SDSS-g, 'r'=SDSS-r, 'i'=SDSS-i, 'z'=SDSS-z, 'Z'=VIKING-Z, 'J'=VIKING-J, 'H'=VIKING-H, 'K'=VIKING-K, 'W1'=WISE-1, 'W2'=WISE-2, 'W3'=WISE-3, 'W4'=WISE-4, '100'=PACS-100, '160'=PACS-100, '250'=SPIRE-250, '350'=SPIRE-350, '450'=SPIRE-450.
}
\usage{
plot.filt(filters, oplot = F, xunit = "ang", xlim = c(1000, 5e+06), xlog = T, sc = 1, cols = "NA", labels=T, lab.cex=1.5, lab.sc=0.9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filters}{
Scalar or vector string indicating filters to plot
}
  \item{oplot}{
Should filters be overplot on current window? TRUE/FALSE
}
  \item{xunit}{
Desired xunit for plotting. Allowed units are 'ang', 'hz', 'm', 'micron','nm'. 
}
  \item{xlim}{
xrange for plotting in terms of xunit.
}
  \item{xlog}{
Should x-axis be logged. TRUE/FALSE
}
  \item{sc}{
Scaling applied to filters to overplot nicely on other figures. Filters are natively scaled to peak at 1.
}
  \item{cols}{
Colours to plot plot filters. If filters is a vector, one for each filter. If set to 'NA' plot.filt will use default colours.
}
  \item{labels}{
Should filter labels be added to plot? TRUE/FALSE
}
  \item{lab.cex}{
Text size of labels.
}
  \item{lab.sc}{
Decimal indicating where labeled should be plotted in relation to peak value in filters. 0.1=bottom of plot window, 1=top of filter.
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
Luke Davies
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

plot.filt(c('FUV','NUV','u','g','r','i','z','Z','J','H','K','W1','W2','W3','W4', '100', '160','250','350','450'), oplot = F)

plot.filt(c('FUV','NUV','u','g','r','i','z','Z','J','H','K','W1','W2','W3','W4', '100', '160','250','350','450'), oplot = F, lab.cex=1.0, lab.sc=0.1, xunit='hz',xlim = 10^c(11, 16))

spec<-get.spec(paste(.libPaths(),'/fourXPS/data/ExampleSpec.fits',sep=''))
spec.plot(spec, main='Example Spectrum', xlim=c(4500,9000), col='navy', lwd=0.5)
plot.filt(c('FUV','NUV','u','g','r','i','z','Z','J','H','K','W1','W2','W3','W4', '100', '160','250','350','450'), oplot = T, lab.cex=1.5, lab.sc=0.1,sc = 2000)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
