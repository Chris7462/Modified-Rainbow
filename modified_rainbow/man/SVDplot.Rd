\name{SVDplot}
\Rdversion{1.1}
\alias{SVDplot}
\title{
Singular value decomposition plot
}
\description{
The singular value decomposition (SVD) plot of Zhang et al. (2007) captures the changes in the 
singular columns as the number of curves gradually increases. 
}
\usage{
SVDplot(object, order = 3, plot = TRUE, plot.type = c("fts", "image"),
        mfrow = c(2, 3))
}
\arguments{
  \item{object}{An object of \code{\link[rainbow]{fds}}.}
  \item{order}{Number of SVD components. The maximum order is 4.}
  \item{plot}{Is graphical display required?}
  \item{plot.type}{Plot functional time series or images?}
  \item{mfrow}{Grid of graphics.}
}
\details{
By using the SVD, Zhang et al. (2007) proposed a plot for visualizing patterns
of functional time series. They considered a set of curves as a two-way \code{(p * n)} data matrix, where \code{p}
is the total number of covariates and \code{n} is the total number of curves. 

The main advantage of this dynamic plot is to visualize both column and row information of a two-way matrix
simultaneously, relate the matrix to the corresponding curves, show local variations, and highlight interactions
between columns and rows of a two-way matrix.
}
\value{
When \code{plot = TRUE}, it returns a plot.

When \code{plot = FALSE}, it returns the following:
 \item{svds}{A number of singular value decomposition ordered by the amount of explained variation.}
 \item{reconstruction}{Reconstruction of the original data using the SVD.}
 \item{residual}{Residual of the original data.}
}
\references{
L. Zhang, J. Marron, H. Shen and Z. Zhu (2007) "Singular value decomposition and its visualization", \emph{Journal of Computational and Graphical Statistics}, \bold{16}(4), 833-854.

A. Grahn (2011) "The animate Package", \url{http://ctan.unsw.edu.au/macros/latex/contrib/animate/animate.pdf}.
}
\author{
Han Lin Shang.  Please, report bugs and suggestions to hanlin.shang@anu.edu.au
}
\note{
MATLAB code is available at \url{http://www.unc.edu/~lszhang/research/network/SVDmovie/}.

Using the animate package of Grahn(2011), a set of dynamic movies can be created to visualize the changes in singular rows and singular columns. 
}
\seealso{
\code{\link[rainbow]{fboxplot}}, \code{\link[base]{svd}}
}
\examples{
SVDplot(ElNinosmooth)
SVDplot(ElNinosmooth, plot.type = "image")
}
\keyword{multivariate}
