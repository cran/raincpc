#' Download raw rainfall data from CPC for time period of interest
#'
#' This function obtains data from the CPC ftp site for any time period from
#' 1979/1/1 to the present day.
#' 
#' It is assumed that the ending year, month and day follow the beginning
#' year, month and day. Although CPC data is revised each day, a buffer
#' of two days is created - i.e., this function will only let you download
#' data till the day before yesterday! 
#' 
#' Output is either a ".gz" file (1979 - 2008) or a ".bin" file (2009 - 2013). 
#' 
#' @param begYr beginning year of the time period of interest, 1979 - 2012
#' @param begMo beginning month of the time period of interest, 1 - 12
#' @param begDay beginning day of the time period of interest, 1 - 28/29/30/31
#' @param endYr ending year of the time period of interest, 1979 -2012
#' @param endMo ending month of the time period of interest, 1 - 12
#' @param endDay ending day of the time period of interest, 1 - 28/29/30/31
#' @export
#' @examples
#' # CPC data for two days, Jul 11-12 2005
#' cpc_get_rawdata(2005, 7, 11, 2005, 7, 12)

cpc_get_rawdata <- function(begYr, begMo, begDay, endYr, endMo, endDay) {                            
  
  # check year validity
  if (!(begYr %in% seq(1979, 2013) & endYr %in% seq(1979, 2013))) {
    stop("Beginning and ending year should be between 1979 to 2013!")
  }
  # check dates validity
  check_dates <- try(seq(as.Date(paste(begYr, begMo, begDay, sep = "-")), 
                         as.Date(paste(endYr, endMo, endDay, sep = "-")), 
                         by = "day"), 
                     silent = TRUE)
  if (class(check_dates) == "try-error") {
    stop("Date range appears to be invalid!")
  }
  # check dates validity - ensure end year, month and day are before present date
  check_dates <- try(seq(as.Date(paste(endYr, endMo, endDay, sep = "-")),
                         Sys.Date() - 2, 
                         by = "day"), 
                     silent = TRUE)
  if (class(check_dates) == "try-error") {
    stop("End date should be two days before the present day!")
  }
    
  # url and file prefixes
  urlHead  <- "ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/"
  fileHead <- "PRCP_CU_GAUGE_V1.0GLB_0.50deg.lnx."
  
  for (eachYr in c(begYr:endYr)) {
    # generate dates of all the days in the format yyyy-mm-dd
    allDates <- seq(as.Date(paste(eachYr, begMo, begDay, sep = "-")), 
                    as.Date(paste(eachYr, endMo, endDay, sep = "-")), 
                    by = "day")
    # date string used in the filenames below
    dateStr  <- paste0(substr(allDates, 1, 4), 
                       substr(allDates, 6, 7), 
                       substr(allDates, 9, 10))
    yrStr    <- substr(allDates, 1, 4)
    
    # identify file names and extensions
    if (eachYr %in% seq(1979, 2005)) {
      urlTag  <- "V1.0/"
      fileTag <- ".gz"
    } else if (eachYr %in% c(2006)) {
      urlTag  <- "RT/"
      fileTag <- "RT.gz"
    } else if (eachYr %in% c(2007, 2008)) {
      urlTag  <- "RT/"
      fileTag <- ".RT.gz"
    } else { #if (eachYr %in% seq(2009, 2013)) {
      urlTag  <- "RT/"
      fileTag <- ".RT"
    } 
    
    # download
    for (eachDay in 1:length(allDates)) {
      # construct url
      fileUrl <- paste0(urlHead, urlTag, yrStr[eachDay], "/", fileHead, 
                        dateStr[eachDay], fileTag)
      
      # out file name; gzipped file prior to 2008 otherwise binary
      outFile <- ifelse(eachYr <= 2008, 
                        paste0("raw_", dateStr[eachDay], ".gz"), 
                        paste0("raw_", dateStr[eachDay], ".bin")) 
      
      # download file only if it doesnt exist
      # internet connection could be "intermittent" - could be due to the CPC server?
      # hence files are not sometimes downloaded; hence the quieted while loop below
      # max tries limited to 20 to avoid infinite loops
      if (!file.exists(outFile)) {
        fileError <- TRUE
        max_tries <- 0
        while (fileError & max_tries < 20) {
          max_tries <- max_tries + 1
          if (eachYr <= 2008) {
            fileStatus <- try(download.file(url = fileUrl, 
                                            destfile = outFile, 
                                            quiet = TRUE), 
                              silent = TRUE)
          } else {
            fileStatus <- try(download.file(url = fileUrl, 
                                            destfile = outFile, 
                                            mode = "wb", 
                                            quiet = TRUE), 
                              silent = TRUE)    
          }
          fileError  <- ifelse(class(fileStatus) == "try-error", TRUE, FALSE)
        }
      }
      
    }
  }
}
