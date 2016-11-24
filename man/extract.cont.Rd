\name{extract.cont}
\alias{extract.cont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interatively extract a 4th order polynomial from a given spectrum to remove the continuum.
}
\description{
Performs an iterative subtraction of 4th order polynomials from an input spectrum as a proxy for continuum extraction.
}
\usage{
extract.cont(spec, iter = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
Input list with the form genrated by get.spec
}
  \item{iter}{
Number of interactions 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{spec }{List with the same properties and input spec, but with continuum exptracted}

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
spec.plot(spec, main='Example Spectrum', xlim=c(4500,9000), col='navy', lwd=0.5, ylim=c(-1000,3500))
spec2<-extract.cont(spec, iter = 4)
spec.plot(spec2,oplot=T, col='red')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
