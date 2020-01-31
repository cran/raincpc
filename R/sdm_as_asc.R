#' @title Raster conversion functions
#'
#' @details \code{sdm_as_asc} is an adaptation of \code{as.asc} from
#' SDMTools; extracts data from objects of class 'RasterLayer' (raster package)
#' into an object of class 'asc'.
#'
#' @param x is an object of class 'matrix'
#' @param xll is the lat center (not corner!) of lower-left grid
#' @param yll is the lon center (not corner!) of lower-left grid
#' @param cellsize is the resolution of the raster in decimal degrees
#'
#' @return Returns an object of class 'asc'.
#'
#' @author Gopi Goteti
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #create a simple object of class 'asc'
#' tasc = sdm_as_asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
#' str(tasc)
#' }
sdm_as_asc <- function(x, xll = 1, yll = 1, cellsize = 1) {

  if (!inherits(x, "matrix")) {
    stop("x should be a matrix")
  }

  # creates the attributes
  mode(x) <- "numeric"
  attr(x, "xll") <- xll
  attr(x, "yll") <- yll
  attr(x, "cellsize") <- cellsize
  attr(x, "type") <- 'numeric'
  class(x) <- "asc"

  return (x)
}
