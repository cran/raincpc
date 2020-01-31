#' @title Raster conversion functions
#'
#' @details \code{sdm_raster_from_asc} is an adaptation of
#' \code{raster.from.asc} from SDMTools; creates an object of
#' class 'RasterLayer' (raster package) from an object of class 'asc'.
#'
#' @param x is an object of class 'asc'
#'
#' @return Returns an object of class requested.
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
#'
#' #convert to RasterLayer
#' traster = sdm_raster_from_asc(tasc)
#' str(traster)
#' }
sdm_raster_from_asc <- function(x) {
  if (class(x) != 'asc') {
    stop('x must be of class asc')
  }

  cellsize = attr(x, "cellsize")
  nrows <- dim(x)[2]
  ncols <- dim(x)[1]
  xmin <- attr(x, "xll") - (0.5 * cellsize)
  ymin <- attr(x, "yll") - (0.5 * cellsize)
  xmax <- xmin + (ncols * cellsize)
  ymax <- ymin + (nrows * cellsize)

  r <- raster::raster(ncols = ncols, nrows = nrows, xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax)

  tvals = as.vector(t(t(unclass(x))[nrows:1,]))
  tvals[which(is.na(tvals))] <- r@file@nodatavalue

  r <- raster::setValues(r, as.vector(t(t(unclass(x))[nrows:1,])))

  return(r)
}

