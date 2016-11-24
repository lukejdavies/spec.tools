\name{spec.clip}
\alias{spec.clip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sigma clipping of sepctrum
}
\description{
Applies a sigma cliping to a spectrum, based on flux value above a 4th order polynomial continuum fit. 
}
\usage{
spec.clip(spec, sigclip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
Input spectrum as a list similar to the get.spec outputs
}
  \item{sigclip}{
Sigma clipping value
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{spec }{Same list as input spec but sigma clipped}
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
spec.plot(spec, main='Example Spectrum', xlim=c(4500,9000), col='navy', lwd=1)

spec2<-spec.clip(spec, 2)
spec.plot(spec2,  col='red', oplot=T, lwd=0.5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
