#' @title Read downloaded raw rainfall data from CPC
#'
#' @details The output matrix has 360 rows (latitudes) and 720 columns 
#' (longitudes) of rainfall/precipitation in units of mm/day. The first data 
#' point has the lat, lon values of -89.75 and 0.25 degrees, respectively. 
#' Spatial resolution of the data is 0.5 degrees. 
#' 
#' @param yr Year associated with the downloaded file, 1979 - present
#' @param mo Month associated with the downloaded file, 1 - 12
#' @param day Day associated with the downloaded file, 1 - 28/29/30/31
#' @param raw_data_path location of downloaded cpc files
#' 
#' @return RasterLayer
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # CPC data for Jun 17 2014
#' rain1 <- cpc_read_rawdata(2014, 6, 17)
#' print(rain1)
#' # CPC data for Jun 18 2014
#' rain2 <- cpc_read_rawdata(2014, 6, 18)
#' print(rain2)
#' }

cpc_read_rawdata <- function(yr, mo, day, raw_data_path = "") {
  
  stopifnot(!(any(c(yr, mo, day) %in% c(""))))
  
  # construct file name
  dateStr <- paste0(yr, sprintf("%.2d", mo), sprintf("%.2d", day))
  cpcFile <- paste0("raw_", dateStr, ".bin")
  if (yr <= 2008) {
    cpcFile <- paste0("raw_", dateStr, ".gz")
  }
  # append location of directory to file names
  if (raw_data_path == "") {
    raw_data_path <- getwd()
  }
  cpcFile <- file.path(raw_data_path, cpcFile)
  
  # check for files
  if(!(file.exists(cpcFile))) {
    stop("Raw file from CPC doesnt exist! First run cpc_get_rawdata()!")
  }
  
  # data attributes, from PRCP_CU_GAUGE_V1.0GLB_0.50deg_README.txt
  cpcNumLat   <- 360 # number of lats
  cpcNumLon   <- 720 # number of lons
  cpcNumBytes <- cpcNumLat * cpcNumLon * 2 # 2 fields, precipitation and num gages
  cpc_xlc     <- 0.25
  cpc_ylc     <- -89.75
  cpc_res     <- 0.5
  
  # open file connection
  if (yr <= 2008) {
    fileCon <- gzcon(file(cpcFile, "rb"))
  } else {
    fileCon <- file(cpcFile, "rb")
  }
  
  # read data
  inData  <- readBin(con = fileCon, 
                     what = numeric(), 
                     n = cpcNumBytes, 
                     size = 4)
  close(fileCon)
  
  # extract first field (precipitation), ignore second field (num gages)
  inData <- inData[1:(cpcNumBytes/2)]
    
  # convert data to matrix
  prcpData <- matrix(inData, ncol = cpcNumLat, nrow = cpcNumLon)

  # remove -ve (missing) values
  prcpData[prcpData < 0] <- NA
  # convert tenths of mm to mm
  prcpData <- ifelse(prcpData > 0, prcpData * 0.1, prcpData)

  # write data to file
  outCon <- file(paste0(dateStr, ".bin"), "wb")
  writeBin(as.numeric(prcpData), con = outCon, size = 4)
  close(outCon)
  
  # define prcpData's class attribs, consistent with SDMTools and adehabitat
  attr(prcpData, "xll") <- cpc_xlc
  attr(prcpData, "yll") <- cpc_ylc
  attr(prcpData, "cellsize") <- cpc_res
  attr(prcpData, "type") <- 'numeric'
  class(prcpData) <- "asc"
  
  # convert to raster
  prcpData <- raster.from.asc(prcpData)
  
  return (prcpData)
}
