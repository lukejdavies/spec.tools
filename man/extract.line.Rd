\name{extract.line}
\alias{extract.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove line features in a spectrum
}
\description{
This function allows the removeal of a desired line from the spectrum and replaces it with and extimate of the continuum at the line position (using fit.cont). Lines can be extracted either using a sting of the line name from load.lines(), or from providing a wavelength. The line will be extracted over the width provided in units of spec$xunit.
}
\usage{
extract.line(spec, line, width, z = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
List with the form produced by get.spec
}
  \item{line}{
Either a string of the line name from load.lines() or a numeric value in terms of spec$xunit. Can be scalar or vector.
}
  \item{width}{
Line width to extract in terms of spec$xunit.
}
  \item{z}{
Provide redshift to map line names to wavelength. This is only required in no spec$z value is present, but will supersuede spec$z. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{spec }{List with the same form as input spec but with lines extracted}

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
spec<-get.spec(paste(.libPaths(),'/fourXPS/data/ExampleSpec.fits',sep=''))
spec.plot(spec, main='Example Spectrum', xlim=c(4500,9000), col='navy', lwd=0.5, ylim=c(-1000,3500))
spec2<-extract.line(spec,'OIIR', 40)
spec.plot(spec2, oplot=T, col='red' )
spec3<-extract.line(spec2,c(5500,7500), 100)
spec.plot(spec3, oplot=T, col='darkgreen' )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
