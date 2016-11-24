\name{smooth.spec}
\alias{smooth.spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smooth a spectrum using loess.
}
\description{
Performs a smothing over a spectrum using loess and a desired smoothing scale
}
\usage{
smooth.spec(spec, sc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
List with similar struture to that generated bu gen.spec
}
  \item{sc}{
Smoothing scale
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{sepc }{List with the same structure as input spec, but with smoothing applied}

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
spec.plot(spec, main='Example Spectrum', xlim=c(4500,9000), col='navy', lwd=0.5)

spec2<-smooth.spec(spec,0.02)
spec.plot(spec2, col='red', oplot=T, lwd=2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
