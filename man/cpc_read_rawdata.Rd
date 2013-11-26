\name{cpc_read_rawdata}
\alias{cpc_read_rawdata}
\title{Read downloaded raw rainfall data from CPC}
\usage{
  cpc_read_rawdata(yr, mo, day, remove_input = TRUE,
    write_output = FALSE)
}
\arguments{
  \item{yr}{year associated with the downloaded file, 1979
  - 2012}

  \item{mo}{month associated with the downloaded file, 1 -
  12}

  \item{day}{day associated with the downloaded file, 1 -
  28/29/30/31}

  \item{remove_input}{logical, whether or not to remove the
  raw CPC file after reading in}

  \item{write_output}{logical, whether or not to write the
  data to a binary output file}
}
\description{
  This function reads raw data from CPC, downloaded using
  `cpc_get_rawdata` and outputs a matrix of values.
}
\details{
  Function checks for the existence of the raw files from
  CPC and only then attempts to process the data.

  The output matrix has 360 rows (latitudes) and 720
  columns (longitudes) of rainfall/precipitation in units
  of mm/day. The first data point has the lat, lon values
  of -89.75 and 0.25 degrees, respectively. Spatial
  resolution of the data is 0.5 degrees. Refer to the
  project home page for plotting the data with proper
  North-South orientation.

  By default, output matrix is NOT written to a binary file
  and raw input files will be DELETED. Change the input
  function arguments as desired.
}
\examples{
# CPC data for Jul 11 2005
cpc_1day <- cpc_read_rawdata(2005, 7, 11)
# CPC data for Jul 12 2005
cpc_1day <- cpc_read_rawdata(2005, 7, 12)
}

