\name{ElNino2011}
\alias{ElNino2011}
\alias{ElNino2011smooth}
\docType{data}
\title{Sea surface temperature data set from January 1950 to December 2011}
\description{
Original monthly sea surface temperatures have been restricted from January 1950 to December 2011. 

The monthly sea surface tmeperatures can be smoothed using smoothing spline with the smoothing parametric 
determined by generalized cross validation. 
}
\usage{
data(ElNino2011)
data(ElNino2011smooth)
}
\format{
An object of class \code{sfts}.
}
\details{
These averaged monthly sea surface temperatures are measured by the different moored buoys in the "Nino region" 
defined by the coordinates 0-10 degree South and 90-80 degree West.
}
\source{
National Weather Service Climate Prediction Center website at \url{http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices}. 
The data is the third column with the title NINO1+2.

This data set can also be found at the NonParametric Functional Data Analysis website (\url{http://www.lsp.ups-tlse.fr/staph/npfda/}). 
}
\references{
A. Antoniadis and T. Sapatinas (2003) "Wavelet methods for continuous-time prediction using Hilbert-valued autoregressive processes", \emph{Journal of Multivariate Analysis}, \bold{87}(1), 133-158.

P. C. Besse, H. Cardot and D. B. Stephenson (2000) "Autoregressive forecasting of some functional climatic variations", \emph{Scandinavian Journal of Statistics}, \bold{27}(4), 673-687.

F. Ferraty, A. Rabhi and P. Vieu (2005) "Conditional quantiles for dependent functional data with application to the climate EL Nino Phenomenon", \emph{Sankhya: The Indian Journal of Statistics}, \bold{67}(2), 378-398.

F. Ferraty and P. Vieu (2007) \emph{Nonparametric functional data analysis}, New York: Springer.

R. J. Hyndman and H. L. Shang (2010) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{19}(1), 29-45.

E. Moran, R. Adams, B. Bakoyema, S. Fiorini and B. Boucek (2006) "Human strategies for coping with El Nino related drought in Amazonia", \emph{Climatic Change}, \bold{77}(3-4), 343-361.

A. Timmermann, J. Oberhuber, A. Bacher, M. Esch, M. Latif and E. Roeckner (1999) "Increased El Nino frequency in a climate model forced by future greenhouse warming", \emph{Nature}, \bold{398}(6729), 694-697.   
}
\examples{
plot(ElNino2011)
plot(ElNino2011smooth)
}
\keyword{datasets}
