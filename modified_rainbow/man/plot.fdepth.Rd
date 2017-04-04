\name{plot.fdepth}
\alias{plot.fdepth}
\title{Plot functional depth}
\description{
Plot functional depth.
}
\usage{
\method{plot}{fdepth}(x, show.legend = TRUE, pos.legend = "bottomleft", ...)
}
\arguments{
\item{x}{An object of class \code{\link[rainbow]{fdepth}}.}
\item{show.legend}{Is legend required?}
\item{pos.legend}{When \code{show.legend = TRUE}, users can specify the position of the legend.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}
\value{Function produces a plot.}
\references{
R. J. Hyndman and H. L. Shang. (2010) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{19}(1), 29-45.
}
\author{Rob J Hyndman, Han Lin Shang.  Please, report bugs and suggestions to hanlin.shang@anu.edu.au}
\seealso{\code{\link[rainbow]{fdepth}}}
\examples{
plot(fdepth(ElNino))
}
\keyword{hplot}

