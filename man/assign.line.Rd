\name{assign.line}
\alias{assign.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive redshifting tool using line assignment.
}
\description{
Allows the user to nominate a desired emission and absorption line (from load.lines()) and select said line in an interactive window to assign  a redshift. Users will be prompted to i) choose a line, ii) click on this line an intractive window, following this all line positions will be displayed assuming the line was correctly identified and the new redshift displayed, iii) decide to update spec$z using the new redshift.
}
\usage{
assign.line(spec, xlim = NA, ylim = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
Input spectrum to be redshifed in the same format as that produced by get.spec.
}
  \item{xlim}{
X-axis range for plotting in interactive window.
}
  \item{ylim}{
Y-axis range for plotting in interactive window.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{spec }{A list the same as input spec but poitentialy with spec$z value chenged to accomdate newly deifned redshift.}


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
specNewZ<-assign.line(spec)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
