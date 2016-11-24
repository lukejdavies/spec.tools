\name{cenwave}
\alias{cenwave}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine central wavelength of a filter.
}
\description{
Easy determination of the central wavelength of common filters. Available filters are: 'FUV'=GALEX-FUV, 'NUV'=GALEX-FUV, 'u'=SDSS-u, 'g'=SDSS-g, 'r'=SDSS-r, 'i'=SDSS-i, 'z'=SDSS-z, 'Z'=VIKING-Z, 'J'=VIKING-J, 'H'=VIKING-H, 'K'=VIKING-K, 'W1'=WISE-1, 'W2'=WISE-2, 'W3'=WISE-3, 'W4'=WISE-4, '100'=PACS-100, '160'=PACS-100, '250'=SPIRE-250, '350'=SPIRE-350, '450'=SPIRE-450.
}
\usage{
cenwave(filt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filt}{
Scalar or vector string indicating filters to return 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Scalar or vector containing the central wavelength of each filter in Angstrom
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Aaron Robotham & Luke Davies
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cents<-cenwave(c('FUV','NUV','u','g','r','i','z','Z','J','H','K','W1','W2','W3','W4', '100', '160','250','350','450'))
magplot(cents, c(1:length(cents)), col=rainbow(length(cents)), pch=16, cex=1.5, log='x', xlab='Wavelength, Ang', ylab='Integer')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
