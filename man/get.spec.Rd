\name{get.spec}
\alias{get.spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to load in spectrum from FITS file
}
\description{
Reads in spectrumtrum from fits, reading most likely header keywords, but specifically designed for makeSimSpec.R outputs. Produces a list which is easily plotted by plot.spec.R 
}
\usage{
get.spec(file, row = 1, logw = F, sc = 1, xunit = "ang", yunit = "ang", z = NA, RA = NA, DEC = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
Location of input FITS file to be read
}
  \item{row}{
Row number with in the FITS file to read. For makeSimSpec.R outputs, this is 1.
}
  \item{logw}{
  Wavelength units in log space? TRUE/FALSE
  
}
  \item{sc}{
 Flux scaling to be applied to the y-axis values.

}
  \item{xunit}{
Units in the x-direction. Allowed units are 'ang', 'hz', 'm', 'micron','nm'. 
}
  \item{yunit}{
Units in the y-direction. Allowed units are 'ang'=ergs/sec/cm^2/Ang, 'hz'=ergs/sec/cm^2/Hz, 'Jy'=Jansky.
}
  \item{z}{
Redshift of spectrum. If set to NA, get.spec will attempt to read the redshift from the FITS header. If not NA, this will overwrite the value from the FITS header
}
  \item{RA}{
Right Asscention of spectrum. If set to NA, get.spec will attempt to read the RA from the FITS header. If not NA, this will overwrite the value from the FITS header
}
  \item{DEC}{
Declination of spectrum. If set to NA, get.spec will attempt to read the DEC from the FITS header. If not NA, this will overwrite the value from the FITS header
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{wave}{Spectral wavelength in units defined by xunit}
\item{flux}{Spectral flux in units defined by yunit}
\item{xunit}{The base xunit - 'ang', 'hz', 'm', 'micron', or 'nm'}
\item{yunit}{The base yunit - 'ang'=ergs/sec/cm^2/Ang, 'hz'=ergs/sec/cm^2/Hz, 'Jy'=Jansky}
\item{z}{The redshift of the spectrum}
\item{RA}{The right asscention of the spectrum}
\item{DEC}{The declination of the spectrum}

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

file=paste(.libPaths(),'/fourXPS/data/ppfx_SDSS_85450_z0_r20.fits',sep='')
spec<-get.spec(file, row=1, logw=F, sc=1, xunit='ang', yunit='ang', z=NA, RA=NA, DEC=NA) 
summary(spec)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }
\keyword{ }% __ONLY ONE__ keyword per line
