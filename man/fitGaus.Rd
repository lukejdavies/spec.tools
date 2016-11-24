\name{fitGaus}
\alias{fitGaus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Gaussian line
}
\description{
Fit a Gaussian profile to an x, y dataset starting with an input mean, standard deviation and normalization. 
}
\usage{
fitGaus(x, y, mu, sig, scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x values of dataset
}
  \item{y}{
y values of dataset
}
  \item{mu}{
Starting mean
}
  \item{sig}{
starting standard deviation
}
  \item{scale}{
starting normalization
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

See Optim outputs for details
fitP$par contains output mean,sd,normalization.

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

x<-seq(0,15,0.1)
y<-dnorm(x, mean = 8.3454, sd = 1.5332, log = FALSE)
plot(x,y)
fitP<-fitGaus(x,y,8,1.5,1.1)   
print(fitP$par)
Gauss <- ((fitP$par[3]*dnorm(x,mean=fitP$par[1],sd=fitP$par[2], log = FALSE)))
lines(x, Gauss, col='red')

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
