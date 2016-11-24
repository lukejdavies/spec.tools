\name{fitGaus}
\alias{fitGaus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Double Component Gaussian line
}
\description{
Fit a Double Gaussian profile to an x, y dataset starting with a input means, standard deviations and normalizations. 
}
\usage{
fitGausdouble(x,y,mu1,sig1,scale1,mu2,sig2,scale2
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x values of dataset
}
  \item{y}{
y values of dataset
}
  \item{mu1}{
Starting mean of Gaussian 1
}
  \item{sig1}{
starting standard deviation of Gaussian 1
}
  \item{scale1}{
starting normalization of Gaussian 1
}

  \item{mu2}{
Starting mean of Gaussian 2
}
  \item{sig2}{
starting standard deviation of Gaussian 2
}
  \item{scale2}{
starting normalization of Gaussian 2
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

See Optim outputs for details
fitP$par contains output mean1,sd1,normalization1,mean2,sd2,normalization2.

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
y<-y+(0.5*dnorm(x, mean = 10.9655, sd = 0.9332, log = FALSE))
plot(x,y)

fitP<-fitGausdouble(x,y,8,1.5,1.1,10,1,0.4)   
print(fitP$par)
Gauss1 <- ((fitP$par[3]*dnorm(x,mean=fitP$par[1],sd=fitP$par[2], log = FALSE)))
Gauss2 <- ((fitP$par[6]*dnorm(x,mean=fitP$par[4],sd=fitP$par[5], log = FALSE)))
lines(x, Gauss1, col='red')
lines(x, Gauss2, col='blue')
lines(x, Gauss1+Gauss2, col='darkgreen', lwd=3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
