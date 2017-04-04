\name{foutliers}
\alias{foutliers}
\title{Functional outlier detection methods.}
\description{
Functional outlier detection methods. 
}
\usage{
foutliers(data, method = c("robMah", "lrt", "depth.trim", "depth.pond", 
 "HUoutliers"), dfunc = depth.mode, nb = 200, suav = 0.05, trim = 0.1, 
  order = 2, lambda = 3.29,...)
}
\arguments{
  \item{data}{An object of class \code{\link[rainbow]{fds}} or \code{fts}.}
  \item{method}{Outlier detection method.}
  \item{dfunc}{When \code{method="lrt"} or \code{method="depth.trim"} or \code{method="depth.pond"},
               users can specify the type of depth functions with possible choices of depth.FM, depth.mode, depth.RP, depth.RPD.}
  \item{nb}{When \code{method="lrt"}, users can specify the number of bootstrap samples. }
  \item{suav}{When \code{method="lrt"}, users can specify the smoothing parameter used in the smoothed bootstrap samples to determine the cutoff value.}
  \item{trim}{When \code{method="lrt"} or \code{method="depth.trim"} or \code{method="depth.pond"}, 
              users can specify the trimming percentage.}
  \item{order}{When \code{method="HUoutliers"}, users can specify the number of principal components.}
  \item{lambda}{When \code{method="HUoutliers"}, users can specify the value of tuning parameter.}
  \item{\dots}{Other arguments.}
}
\details{
When \code{method="lrt"}, the outlier detection method corresponds to the approach of Febrero et al. (2007) using the likelihood ratio test.

When \code{method="depth.trim"}, the outlier detection method corresponds to the approach of Febrero et al. (2008) using the functional depth with trimmed curves.

When \code{method="depth.pond"}, the outlier detection method corresponds to the approach of Febrero et al. (2008) using the functional depth with all curves.

When \code{method="HUoutliers"}, the outlier detection method corresponds to the approach of Hyndman and Ullah (2008) using the integrated square forecast errors.

When \code{method="robMah"}, the outlier detection method corresponds to the approach of Rousseeuw and Leroy (1987) using the robust Mahalanobis distance. 
}
\value{
A list containing the following components is returned.
  \item{outliers}{Detected outliers.}
  \item{cutoff}{Threshold value to separate outliers from non-outliers, when \code{method="lrt"}, \code{method="depth.trim"}, and \code{method="depth.pond"}.} 
  \item{depth.total}{Depth measure of each functional curve.}
  \item{depth.out}{Depth measure of functional outliers.}
}
\references{
P. Rousseeuw and A. Leroy (1987) \emph{Robust regression and outlier detection}, John Wiley and Sons, New York.

A. Atkinson (1994) "Fast very robust methods for the detection of multiple outliers", \emph{Journal of the American Statistical Association}, \bold{89}(428), 1329-1339.

R. J. Hyndman and M. S. Ullah (2007) "Robust forecasting of mortality and fertility rates: A functional data approach", \emph{Computational Statistics and Data Analysis}, \bold{51}(10), 4942-4956.

M. Febrero and P. Galeano and W. Gonzalez-Manteiga (2007) "A functional analysis of NOx levels: location and scale estimation and outlier detection", \emph{Computational Statistics}, \bold{22}(3), 411-427.

M. Febrero and P. Galeano and W. Gonzalez-Manteiga (2008) "Outlier detection in functional data by depth measures, with application to identify abnormal NOx levels", \emph{Environmetrics}, \bold{19}(4), 331-345.

R. J. Hyndman and H. L. Shang. (2010) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{19}(1), 29-45.
}
\author{Han Lin Shang.  Please, report bugs and suggestions to hanlin.shang@anu.edu.au}
\examples{
foutliers(data = ElNino, method = "depth.trim")
}
\keyword{multivariate}

