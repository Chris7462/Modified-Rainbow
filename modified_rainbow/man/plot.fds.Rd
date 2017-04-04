\name{plot.fds}
\alias{plot.fds}
\alias{lines.fds}
\alias{points.fds}
\title{Plot functional objects}
\description{
Plot functional curves.
}
\usage{
\method{plot}{fds}(x, plot.type = c("functions", "time", "depth", "density"),
  col = NULL, type = "l", lty = 1, xlab = x$xname, ylab = x$yname, 
  pch = c(1:9,0, letters, LETTERS), add = FALSE, index,     
  colorchoice = c("rainbow", "heat.colors", "terrain.colors", 
  "topo.colors", "cm.colors", "rainbow_hcl", "gray", "sequential_hcl", 
  "heat_hcl", "terrain_hcl", "diverge_hcl"), plotlegend = FALSE, 
  legendpos = "topright", ncol = 1, ...)
\method{lines}{fds}(x, type = "l", index, ...)
\method{points}{fds}(x, type = "p", index, ...)  
}
\arguments{
\item{x}{An object of class \code{\link[rainbow]{fds}} or \code{fts}.}
\item{plot.type}{Type of plot. See details for more explanations.}
\item{col}{Colors to use in plot. Default in \code{plot.fds} is to use a rainbow color palette with the number of colors equal to the number of functions.}
\item{type}{1-character string giving the type of plot desired.}
\item{lty}{The line type.}
\item{xlab}{A title for x axis.}
\item{ylab}{A title for y axis.}
\item{pch}{Either an integer specifying a symbol or a single character to be used as the default in plotting points.}
\item{add}{If \code{add = TRUE}, it plots a line or points.}
\item{index}{Index of a specific curve that is plotted as a line or points.}
\item{colorchoice}{Color palette used for drawing the rainbow plot.}
\item{plotlegend}{Add a legend to the graph.}
\item{legendpos}{Position of legend.}
\item{ncol}{Number of column in the legend.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}
\value{Function produces a plot.}
\details{
If \code{plot.type="functions"}, then functions are plotted using a rainbow color palette so the
first few functions are shown in red, followed by orange, yellow, green, blue
and indigo with the last few functions plotted in violet.

If \code{plot.type="time"}, then each value of x is shown as a separate time series in a time plot.

If \code{plot.type="depth"}, then functions are first ordered by depth and then plotted using a rainbow color palette.

If \code{plot.type="density"}, then functions are first ordered by density and then plotted using a rainbow color palette.
}
\references{
R. J. Hyndman and H. L. Shang. (2010) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{19}(1), 29-45.
}
\author{Rob J Hyndman, Han Lin Shang. Please, report bugs and suggestions to hanlin.shang@anu.edu.au}
\seealso{\code{\link[rainbow]{fds}}, \code{\link[rainbow]{lines.fds}}, \code{\link[rainbow]{points.fds}}}
\examples{
plot(x = ElNinosmooth, plot.type = "functions", legend = TRUE)
lines(x = ElNinosmooth, plot.type = "functions", index = 3)
points(x = ElNinosmooth, plot.type = "functions", index = 3)
}
\keyword{hplot}

