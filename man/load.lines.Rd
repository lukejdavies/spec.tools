\name{load.lines}
\alias{load.lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to load in common emission and absorption lines
}
\description{
Loads in a list of common emssion/absorption lines, with names wavelenghts and frequencies for ease of overplotting on spectra
}
\usage{
load.lines()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{names }{Emission line name}
\item{wave_ang }{Emission line wavelength in Angstrom}
\item{wave_m }{Emission line wavelength in Meters}
\item{wave_micron }{Emission line wavelength in Micron}
\item{wave_nm }{Emission line wavelength in Nano Meters}
\item{freq_hz }{Emission line frequency in Hz}
\item{stellar }{Is line a stellar absorption line. TRUE/FALSE}
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
load.lines()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
