#' @title Raster conversion functions
#'
#' @details \code{sdm_asc_from_raster} is an adaptation of
#' \code{asc.from.raster} from SDMTools; extracts data from objects of
#' class 'RasterLayer' (raster package) into an object of class 'asc'.
#'
#' @param x is an object of class 'RasterLayer'
#'
#' @return Returns an object of class requested.
#'
#' @author Gopi Goteti
#'
#' @export
#'
sdm_asc_from_raster <- function(x) {
  if (!any(class(x) %in% 'RasterLayer')) {
    stop('x must be of class raster or RasterLayer')
  }

  cellsize = (x@extent@ymax - x@extent@ymin) / x@nrows
  yll = x@extent@ymin + (0.5 * cellsize)
  xll = x@extent@xmin + (0.5 * cellsize)
  tmat = t(matrix(raster::getValues(x), nrow = x@nrows, ncol = x@ncols, byrow = TRUE)[x@nrows:1,])
  tmat[which(tmat == x@file@nodatavalue)] <- NA

  return(sdm_as_asc(tmat, yll = yll, xll = xll, cellsize = cellsize))
}
