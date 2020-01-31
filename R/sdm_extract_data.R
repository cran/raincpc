#' @title Spatial Join of Points with Raster Grids
#'
#' @details \code{sdm_extract_data} is an adaptation of \code{extract.data} from
#' SDMTools; extracts data from raster object of class 'asc' or
#' RasterLayer' (raster package) at specified locations.
#'
#' @param pts a two-column data frame or matrix with the x and y coordinates of
#' the locations of interest.
#' @param x a raster matrix of class 'asc' (this and the adehabitat package),
#' 'RasterLayer' (raster package) or 'SpatialGridDataFrame' (sp package)
#'
#' @return Returns a vector equal in length to the number of locations in pnts.
#'
#' @author Gopi Goteti
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #create a simple object of class 'asc'
#' tasc = as.asc(matrix(1:50,nr=50,nc=50)); print(tasc)
#'
#' #define some point locations
#' points = data.frame(x=runif(25,1,50),y=runif(25,1,50))
#'
#' #extract the data
#' points$values = sdm_extract_data(points, tasc)
#'
#' #show the data
#' print(points)
#' }
sdm_extract_data <- function(pts, x) {
  #check if raster from sp or raster package and convert if necessary
  if (any(class(x) %in% 'RasterLayer')) {
    x <- sdm_asc_from_raster(x)
  }

  #check to ensure x is of class asc
  if (class(x) != 'asc') {
    stop('matrix must be of class "asc"')
  }

  xy <- sdm_getXYcoords(x)

  cellsize <- attr(x, "cellsize")

  xy$x <- xy$x + (cellsize / 2)
  xy$x <- c(xy$x[1] - cellsize, xy$x)

  xy$y <- xy$y + (cellsize / 2)
  xy$y <- c(xy$y[1] - cellsize, xy$y)

  xf <- as.numeric(cut(pts[, 1], xy$x))
  yf <- as.numeric(cut(pts[, 2], xy$y))

  return (x[cbind(xf, yf)])
}
