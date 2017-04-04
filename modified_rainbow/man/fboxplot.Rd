\name{fboxplot}
\alias{fboxplot}
\title{Functional bagplot and functional HDR boxplot}
\description{
Compute bivariate bagplot, functional bagplot and bivariate HDR boxplot, functional HDR boxplot.
}
\usage{
fboxplot(data, plot.type = c("functional", "bivariate"),
	type = c("bag", "hdr"), alpha = c(0.05, 0.5), projmethod = c("PCAproj","rapca"),
	factor = 1.96, na.rm = TRUE, xlab = data$xname, ylab = data$yname, 
	shadecols = gray((9:1)/10), pointcol = 1, plotlegend = TRUE, 
	legendpos = "topright", ncol = 2, ...) 
}
\arguments{
  \item{data}{An object of class \code{\link[rainbow]{fds}} or \code{fts}.}
  \item{plot.type}{Version of boxplot. When \code{plot.type="functional"}, a functional plot is provided. When \code{plot.type="bivariate"}, a square bivariate plot is provided.}
  \item{type}{Type of boxplot. When \code{type="bag"}, a bagplot is provided. When \code{type="hdr"}, a HDR boxplot is provided.}
  \item{alpha}{Coverage probability for the functional HDR boxplot. \eqn{\alpha}{alpha} are the coverage percentages of the outliers and the central region.}
  \item{factor}{When \code{type="bag"}, the outer region of a bagplot is the convex hull obtained by inflating the inner region by the bagplot factor.}
  \item{na.rm}{Remove missing values.}
  \item{xlab}{A title for the x axis.}
  \item{ylab}{A title for the y axis.}
  \item{shadecols}{Colors for shaded regions.}
  \item{pointcol}{Color for outliers and mode.}
  \item{plotlegend}{Add a legend to the graph.}
  \item{legendpos}{Legend position. By default, it is the top right corner.}
  \item{ncol}{Number of columns in the legend.}
  \item{projmethod}{Method used for projection.}
  \item{...}{Other arguments.}
}
\details{
The functional curves are first projected into a finite dimensional subspace. 
For simiplicity, we choose the subspace as \eqn{R^2}. Based on Tukey (1974)'s halfspace bagplot and 
Hyndman (1996)'s HDR boxplot, we order each data point in \eqn{R^2} by data depth and data density. 
Outliers are those that have either lowest depth or lowest density.
}
\value{
Function produces a plot.
}
\references{
J. W. Tukey (1974) "Mathematics and the picturing of data", \emph{Proceedings of the International Congress of Mathematicians}, \bold{2}, 523-532, Canadian Mathematical Congress, Montreal.

P. Rousseeuw, I. Ruts and J. Tukey (1999) "The bagplot: A bivariate boxplot", \emph{The American Statistician}, \bold{53}(4), 382-387.

R. J. Hyndman (1996) "Computing and graphing highest density regions", \emph{The American Statistician}, \bold{50}(2), 120-126.

R. J. Hyndman and H. L. Shang. (2010) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{19}(1), 29-45.
}
\author{Rob J Hyndman, Han Lin Shang. Please, report bugs and suggestions to hanlin.shang@anu.edu.au}
\seealso{
\code{\link[rainbow]{SVDplot}}
}
\examples{
fboxplot(data = ElNino, plot.type = "functional", type = "bag", projmethod="PCAproj")
fboxplot(data = ElNino, plot.type = "bivariate", type = "bag", projmethod="PCAproj")
}
\keyword{multivariate}

