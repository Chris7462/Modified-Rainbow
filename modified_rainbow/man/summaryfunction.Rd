\name{summaryfunction}
\alias{summaryfunction}
\title{
Summary statistics for functional data
}
\description{
Display summary statistics (minimum, 1st quantile, median, mean, 3rd quantile, maximum) and quantiles of functional data
}
\usage{
summaryfunction(ftsdata, plot.type = c("summarystats", "quantilestats"),
 quantilepercent = seq(0.1, 0.9, by = 0.1), plot.legend = FALSE, 
  legendpos = "topright", cex = 0.9, lwd = 1, lty = 1, ncol = 2)
}
\arguments{
  \item{ftsdata}{An object of class \code{\link{fds}}.}
  \item{plot.type}{Summary statistics or quantiles.}
  \item{quantilepercent}{Percentage of quantiles.}
  \item{plot.legend}{Plot the legend.}
  \item{legendpos}{Position of the legend.}
  \item{cex}{Point size.}
  \item{lwd}{Width of line.}
  \item{lty}{Line type.}
  \item{ncol}{Number of columns in the legend.}
}
\details{
A function for displaying summary statistics or quantiles of functional data.
}
\value{
Return a plot of summary statistics of functional data or a plot of quantiles of functional data.
}
\author{
Han Lin Shang.  Please, report bugs and suggestions to hanlin.shang@anu.edu.au
}
\seealso{
\code{\link{fds}}
}
\examples{
summaryfunction(Australiasmoothfertility, plot.type="summarystats")
summaryfunction(Australiasmoothfertility, plot.type="quantilestats",plot.legend = TRUE)
}
\keyword{methods}
