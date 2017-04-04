\name{fds}
\alias{fds}
\alias{fts}
\alias{sfts}
\title{Create functional objects}
\description{
The function \code{fds} is used to create general functional objects that are not ordered by time.
The function \code{fts} is used to create functional time series objects.
The function \code{sfts} is used to create sliced functional time series objects, where the \code{x} variable is 
also a time variable.
}
\usage{
fds(x, y, xname, yname)
fts(x, y, start = 1, frequency = 1, xname, yname)
sfts(data, period = frequency(data), start = tsp(data)[1], 
     frequency = 1, xname, yname)
}
\arguments{
\item{x}{Numeric vector of length \eqn{p}{p}.}
\item{y}{Matrix of size \eqn{p\times n}{p x n} representing \eqn{n}{n}
    functions of \eqn{x}{x} observed at points \eqn{1,\dots,p}{1,...,p}.}
\item{data}{An object of class \code{\link[stats]{ts}}.}
\item{period}{Time period of sliced functional data. For instance, \code{period = 12} is a monthly data.}    
\item{start}{The time of the first observation. Either a single number or a vector
    of two integers, which specify a natural time unit and a (1-based) number of
    samples into the time unit. See \code{\link[stats]{ts}} for details.}
\item{frequency}{The number of observations per unit of time.}
\item{xname}{Character string giving name of \code{x} vector. (optional)}
\item{yname}{Character string giving name of \code{y} vector. (optional)}
}
\value{An object of class \code{fds} or \code{fts} or \code{sfts}.}
\author{Rob J Hyndman and Han Lin Shang.  Please, report bugs and suggestions to hanlin.shang@anu.edu.au}
\examples{
fds(x = 1:20, y = Simulationdata$y, xname = "x", yname = "Simulated value")
fts(x = 15:49, y = Australiasmoothfertility$y, xname = "Age", 
    yname = "Fertility rate")
sfts(ts(as.numeric(ElNino$y), frequency = 12), xname = "Month", 
     yname = "Sea surface temperature")    
}
\keyword{ts}

