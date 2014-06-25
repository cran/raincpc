\name{cpc_read_rawdata}
\alias{cpc_read_rawdata}
\title{Read downloaded raw rainfall data from CPC}
\usage{
  cpc_read_rawdata(yr, mo, day, raw_data_path = "")
}
\arguments{
  \item{yr}{Year associated with the downloaded file, 1979
  - present}

  \item{mo}{Month associated with the downloaded file, 1 -
  12}

  \item{day}{Day associated with the downloaded file, 1 -
  28/29/30/31}

  \item{raw_data_path}{location of downloaded cpc files}
}
\value{
  RasterLayer
}
\description{
  Read downloaded raw rainfall data from CPC
}
\details{
  The output matrix has 360 rows (latitudes) and 720
  columns (longitudes) of rainfall/precipitation in units
  of mm/day. The first data point has the lat, lon values
  of -89.75 and 0.25 degrees, respectively. Spatial
  resolution of the data is 0.5 degrees.
}
\examples{
\dontrun{
# CPC data for Jun 17 2014
rain1 <- cpc_read_rawdata(2014, 6, 17)
print(rain1)
# CPC data for Jun 18 2014
rain2 <- cpc_read_rawdata(2014, 6, 18)
print(rain2)
}
}

