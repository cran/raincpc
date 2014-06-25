#' @title Download global rainfall data from CPC for the time period of interest
#' 
#' @param begYr beginning year of the time period of interest, 1979 - present
#' @param begMo beginning month of the time period of interest, 1 - 12
#' @param begDay beginning day of the time period of interest, 1 - 28/29/30/31
#' @param endYr ending year of the time period of interest, 1979 - present
#' @param endMo ending month of the time period of interest, 1 - 12
#' @param endDay ending day of the time period of interest, 1 - 28/29/30/31
#' 
#' @return downloads either a ".gz" file (1979 - 2008) or a ".bin" file 
#' (2009 - present)
#' 
#' @author Gopi Goteti
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # CPC data for two days, Jun 17-18 2014
#' cpc_get_rawdata(2014, 6, 17, 2014, 6, 18)
#' }
cpc_get_rawdata <- function(begYr, begMo, begDay, endYr, endMo, endDay) {                            
  
  # get current year
  curr_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # check year validity
  if (!all(c(begYr, endYr) %in% seq(1979, curr_year))) {
    stop("Beginning and ending year should be within 1979 and ", curr_year)
  }
  
  # check month validity
  if (!all(c(begMo, endMo) %in% seq(1, 12))) {
    stop("Beginning and ending month should be within 1 and 12!")
  }

  # check day validity
  if (!all(c(begDay, endDay) %in% seq(1, 31))) {
    stop("Beginning and ending day should be within 1 and 31!")
  }
  
  # check dates validity - sequential
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
    } else { #if (eachYr %in% seq(2009, curr_year)) {
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
