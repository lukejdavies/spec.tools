\name{add.line}
\alias{add.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add Emission/Absorption line to spectrum
}
\description{
The is function allows the users to add an Gaussian emission or absorption oien to the current spectrum based on either the line name (availble in load.lines) or a wavelength. The user must provide the line width and equivalent width.
}
\usage{
add.line(spec, line, sigma, EW, z = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
Input spectrum as a list similar to the get.spec outputs
}
  \item{line}{
Desired line to add, either a sting name matched to the output of load.lines or a wavelength corresponding to the spec$xunit value. Can be scalar or vector. If set to 'all' all of the lines from load.lines() are added.
}
  \item{sigma}{
Standard deviation width of the line in terms of spec$xunit. Can be scalar or vector (to match 'line').  
}
  \item{EW}{
Equivalent width of the line the line in terms of spec$xunit. To add and absorption line, make this value negative. Can be scalar or vector (to match 'line').  
}
  \item{z}{
Redshift to uses for line position when a line name is given. This overrides the inptu spec$z value.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{spec }{Same list as input spec but with line added}


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

spec2<-add.line(spec, 'OIIIR', 20,60) # add an OIII[5007] line with standard deviation 20Ang and EW=60Ang

spec.plot(spec2, col='red', oplot=T)

spec3<-add.line(spec, c(5245,6010, 8050), c(10,36,15), c(45,70,-30)) # add 2 emission and one absorption line at specific wavelengths.

spec.plot(spec3, col='darkgreen', oplot=T)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
