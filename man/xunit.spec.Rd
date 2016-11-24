\name{xunit.spec}
\alias{xunit.spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Changes the xunit of a spectrum
}
\description{
Converts the xunit of a spec list (as produced by get.spec) to another unit. Available units are: 'ang', 'hz', 'm', 'micron','nm'
}
\usage{
xunit.spec(spec, xunit = "hz")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
Input spectrum as a list similar to the get.spec outputs
}
  \item{xunit}{
X-unit to convert to.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{spec }{Same list as input spec but with spec$xunit changed}
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
spec<-get.spec(paste(.libPaths(),'/fourXPS/data/ExampleSpec.fits',sep=''))
spec.plot(spec, main='Example Spectrum - Ang',col='navy', lwd=0.5)

spec2<-xunit.spec(spec, xunit = "hz")
spec.plot(spec2, main='Example Spectrum - Hz', col='navy', lwd=0.5)

spec3<-xunit.spec(spec2, xunit = "micron")
spec.plot(spec3, main='Example Spectrum - Micron', col='navy', lwd=0.5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
