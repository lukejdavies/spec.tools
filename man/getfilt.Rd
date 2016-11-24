\name{getfilt}
\alias{getfilt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns common filter transmission curves  
}
\description{
Provides filter transmission curves for a number of common filters. Available filters are: 'FUV'=GALEX-FUV, 'NUV'=GALEX-FUV, 'u'=SDSS-u, 'g'=SDSS-g, 'r'=SDSS-r, 'i'=SDSS-i, 'z'=SDSS-z, 'Z'=VIKING-Z, 'J'=VIKING-J, 'H'=VIKING-H, 'K'=VIKING-K, 'W1'=WISE-1, 'W2'=WISE-2, 'W3'=WISE-3, 'W4'=WISE-4, '100'=PACS-100, '160'=PACS-100, '250'=SPIRE-250, '350'=SPIRE-350, '450'=SPIRE-450,
}
\usage{
getfilt(filter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filter}{
Scalar string indicating filter to return 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{out }{Filter transmission, array with format out[, c('index','wavelength (angstrom)','transmission')]}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Aaron Robotham
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
filter<-getfilt('r')
plot(filter[,c('Wavelength','Trans')], type='l', col='red')


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
