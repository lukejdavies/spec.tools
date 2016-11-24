\name{match_2d}
\alias{match_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Match a single 2d data point to the closest datapoint in an array, in terms of standard deviation in x and y.  
}
\description{
Function calculates the the x and y distance between a user supplied data point and all other data points in an array, in terms of SD(x) and SD(y). Then cacluated the closest match in distance.  
}
\usage{
match_2d(x1, y1, x2, y2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
Input sinlge data point x
}
  \item{y1}{
Input sinlge data point y
}
  \item{x2}{
Input data array x
}
  \item{y2}{
Input data array y
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{match }{The integer element in x2,y2 which is the closest match to x1,y1}

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
x1<-5.5
y1<-3.4
x2<-runif(20,1,10)
y2<-runif(20,1,10)
plot(x2,y2)
points(x1,y1,pch=21, bg='red', cex=1.5)

match<-match_2d(x1,y1,x2,y2)
points(x2[match],y2[match],pch=21, bg='blue', cex=1.5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{  }% __ONLY ONE__ keyword per line
