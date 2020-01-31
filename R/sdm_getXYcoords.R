#' @title Computes the X and Y Coordinates of the Pixels of a Raster Map
#'
#' @details \code{sdm_getXYcoords} is an adaptation of \code{getXYcoords} from
#' SDMTools; computes the geographical coordinates of the rows and
#' columns of pixels of a raster map of class \code{asc}.
#'
#' @param w an object of class \code{asc}.
#'
#' @return Returns a list with two components: \item{x}{the x coordinates of
#' the columns of pixels of the map} \item{y}{the y coordinates of the rows of
#' pixels of the map}
#'
#' @author Gopi Goteti
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tasc = sdm_as_asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
#' sdm_getXYcoords(tasc)
#' }
sdm_getXYcoords <- function(w) {
  # check if raster, convert if necessary
  if (any(class(w) %in% 'RasterLayer')) {
    w <- sdm_asc_from_raster(w)
  }

  if (!inherits(w, "asc")) {
    stop("must be of class asc")
  }

  # Gets the attributes
  cs <- attr(w, "cellsize")
  xll <- attr(w, "xll")
  yll <- attr(w, "yll")

  # Computation of the number of rows and columns of the matrix
  nr <- nrow(w)
  nc <- ncol(w)

  # The results
  xcoords <- xll + (c(0:(nr - 1)) * cs)
  ycoords <- yll + (c(0:(nc - 1)) * cs)

  return(list(x = xcoords, y = ycoords))
}

