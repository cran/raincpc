#' Read downloaded raw rainfall data from CPC
#'
#' This function reads raw data from CPC, downloaded using `cpc_get_rawdata` and
#' outputs a matrix of values.
#' 
#' Function checks for the existence of the raw files from CPC and only then
#' attempts to process the data. 
#' 
#' The output matrix has 360 rows (latitudes) and 720 columns (longitudes) of 
#' rainfall/precipitation in units of mm/day. The first data point has the lat, 
#' lon values of -89.75 and 0.25 degrees, respectively. Spatial resolution of 
#' the data is 0.5 degrees. Refer to the project home page for plotting the 
#' data with proper North-South orientation. 
#' 
#' By default, output matrix is NOT written to a binary file and raw input files
#' will be DELETED. Change the input function arguments as desired.
#' 
#' @param yr year associated with the downloaded file, 1979 - 2012
#' @param mo month associated with the downloaded file, 1 - 12
#' @param day day associated with the downloaded file, 1 - 28/29/30/31
#' @param remove_input logical, whether or not to remove the raw CPC file 
#' after reading in
#' @param write_output logical, whether or not to write the data to a binary 
#' output file
#' @export
#' @examples
#' # CPC data for Jul 11 2005
#' cpc_1day <- cpc_read_rawdata(2005, 7, 11)
#' # CPC data for Jul 12 2005
#' cpc_1day <- cpc_read_rawdata(2005, 7, 12)

cpc_read_rawdata <- function(yr, mo, day, remove_input = TRUE, write_output = FALSE) {
  
  stopifnot(!(any(c(yr, mo, day) %in% c(""))))
  
  # file name
  dateStr <- paste0(yr, sprintf("%.2d", mo), sprintf("%.2d", day))
  cpcFile <- paste0("raw_", dateStr, ".bin")
  if (yr <= 2008) {
    cpcFile <- paste0("raw_", dateStr, ".gz")
  }
  if(!(file.exists(cpcFile))) {
    stop("Raw file from CPC doesnt exist! First run cpc_get_rawdata()!")
  }
  
  # data attributes, from PRCP_CU_GAUGE_V1.0GLB_0.50deg_README.txt
  cpcNumLat   <- 360 # number of lats
  cpcNumLon   <- 720 # number of lons
  cpcNumBytes <- cpcNumLat * cpcNumLon * 2 # 2 fields, precipitation and num gages
  
  # read data
  # open file connection
  if (yr <= 2008) {
    fileCon <- gzcon(file(cpcFile, "rb"))
  } else {
    fileCon <- file(cpcFile, "rb")
  }
  
  inData  <- readBin(con = fileCon, 
                     what = numeric(), 
                     n = cpcNumBytes, 
                     size = 4)
  close(fileCon)
  # remove input
  if (remove_input) {
    file.remove(cpcFile)
  }
  
  # extract first field (precipitation), ignore second field (num gages)
  inData <- inData[1:(cpcNumBytes/2)]
  
  # CPC raw data goes from South to North; need to flip rows for plotting
  prcpData <- array(0, dim = c(cpcNumLat, cpcNumLon))
  for(eachRow in 1:cpcNumLat) {
    index1 <- 1 + (eachRow - 1) * cpcNumLon
    index2 <- eachRow * cpcNumLon
    prcpData[eachRow, ] <- inData[index1:index2]
  }
  # remove -ve (missing) values
  prcpData[prcpData < 0] <- NA
  # convert tenths of mm to mm
  prcpData <- ifelse(prcpData > 0, prcpData * 0.1, prcpData)
  
  # write data to file
  if (write_output) {
    outCon <- file(paste0(dateStr, ".bin"), "wb")
    writeBin(as.numeric(prcpData), con = outCon, size = 4)
    close(outCon)
  }
  
  return (prcpData)
}
