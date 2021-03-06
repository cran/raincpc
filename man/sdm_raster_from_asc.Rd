% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdm_raster_from_asc.R
\name{sdm_raster_from_asc}
\alias{sdm_raster_from_asc}
\title{Raster conversion functions}
\usage{
sdm_raster_from_asc(x)
}
\arguments{
\item{x}{is an object of class 'asc'}
}
\value{
Returns an object of class requested.
}
\description{
Raster conversion functions
}
\details{
\code{sdm_raster_from_asc} is an adaptation of
\code{raster.from.asc} from SDMTools; creates an object of
class 'RasterLayer' (raster package) from an object of class 'asc'.
}
\examples{
\dontrun{
#create a simple object of class 'asc'
tasc = sdm_as_asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
str(tasc)

#convert to RasterLayer
traster = sdm_raster_from_asc(tasc)
str(traster)
}
}
\author{
Gopi Goteti
}
