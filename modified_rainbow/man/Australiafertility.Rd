\name{Australiafertility}
\alias{Australiafertility}
\alias{Australiasmoothfertility}
\docType{data}
\title{
Australian fertility data
}
\description{
Age-specific fertility rates between ages 15 and 49 in Australia from 1921 to 2006. 

The age-specific fertility rates can be smoothed using a weighted median smoothing B-splines, constrained to be concave. 
}
\usage{
data(Australiafertility)
data(Australiasmoothfertility)
}
\format{
An object of class \code{fts}.
}
\details{
Australian fertility rates and populations (1921-2006) for age groups (15-49) were obtained from the Australian
Bureau of Statistics (Cat.No.3105.0.65.001, Table 38). These are defined as the number of live births during 
the calendar year, according to the age of the mother, per 1000 of the female resident population of the same
age at 30 June. 

Australiasmoothfertility is the smoothed version of Australiafertility data. The smoothing technique is the penalized regression spline with concave constraint, described in Hyndman and Ullah (2007). 
}
\source{
The Australian Demographic Data Bank (courtesy of Len Smith). 
}
\references{
R. J. Hyndman and M. S. Ullah (2007) "Robust forecasting of mortality and fertility rates: A functional data approach", \emph{Computational Statistics and Data Analysis}, \bold{51}(10), 4942-4956.

R. J. Hyndman and H. Booth (2008) "Stochastic population forecasts using functional data models for mortality, fertility and migration", \emph{International Journal of Forecasting}, \bold{24}(3), 323-342.

R. J. Hyndman and H. L. Shang (2009) "Forecasting functional time series (with discussion)", \emph{Journal of the Korean Statistical Society}, \bold{38}(3), 199-221.
}
\author{Han Lin Shang}
\examples{
plot(Australiafertility)
plot(Australiasmoothfertility)
}
\keyword{datasets}
