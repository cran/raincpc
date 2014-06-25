\name{cpc_get_rawdata}
\alias{cpc_get_rawdata}
\title{Download global rainfall data from CPC for the time period of interest}
\usage{
  cpc_get_rawdata(begYr, begMo, begDay, endYr, endMo,
    endDay)
}
\arguments{
  \item{begYr}{beginning year of the time period of
  interest, 1979 - present}

  \item{begMo}{beginning month of the time period of
  interest, 1 - 12}

  \item{begDay}{beginning day of the time period of
  interest, 1 - 28/29/30/31}

  \item{endYr}{ending year of the time period of interest,
  1979 - present}

  \item{endMo}{ending month of the time period of interest,
  1 - 12}

  \item{endDay}{ending day of the time period of interest,
  1 - 28/29/30/31}
}
\value{
  downloads either a ".gz" file (1979 - 2008) or a ".bin"
  file (2009 - present)
}
\description{
  Download global rainfall data from CPC for the time
  period of interest
}
\examples{
\dontrun{
# CPC data for two days, Jun 17-18 2014
cpc_get_rawdata(2014, 6, 17, 2014, 6, 18)
}
}
\author{
  Gopi Goteti
}

