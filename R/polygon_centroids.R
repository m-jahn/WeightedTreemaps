#' poly_area
#' 
#' Mainly for internal use. Determines the area of a polygon based on 
#' its x and y coordinates.
#' This function is a reimplementation of `soiltexture::TT.polygon.area()`
#' and only re-implemented to avoid extra dependencies.
#' 
#' @param poly_x (numeric)  X coordinates of each vertices of the polygon
#' @param poly_y (numeric)  Y coordinates of each vertices of the polygon
#' 
#' @return A numeric, the area of the polygon.
#' 
#' @seealso soiltexture::TT.polygon.area()
#' 
#' @export
poly_area <- function (poly_x, poly_y) 
{
  i <- 1:length(poly_x)
  poly_x <- c(poly_x, poly_x[1])
  poly_y <- c(poly_y, poly_y[1])
  A <- 0.5 * sum(poly_x[i] * poly_y[i + 1] - poly_x[i + 1] * poly_y[i])
  return(A)
}

#' poly_centroid
#' 
#' Mainly for internal use. Determines the centroids of a polygon based on 
#' its x and y coordinates.
#' This function is a reimplementation of `soiltexture::TT.polygon.centroids()`
#' and only re-implemented to avoid extra dependencies.
#' 
#' @param poly_x (numeric)  X coordinates of each vertices of the polygon
#' @param poly_y (numeric)  Y coordinates of each vertices of the polygon
#' 
#' @return A numeric, the area of the polygon.
#' 
#' @seealso soiltexture::TT.polygon.centroids()
#' 
#' @export
poly_centroid <- function(poly_x, poly_y) 
{
  i <- 1:length(poly_x)
  A <- poly_area(poly_x, poly_y)
  poly_x <- c(poly_x, poly_x[1])
  poly_y <- c(poly_y, poly_y[1])
  Cx <- (1/(6 * A)) * sum((poly_x[i] + poly_x[i + 1]) * (poly_x[i] * 
    poly_y[i + 1] - poly_x[i + 1] * poly_y[i]))
  Cy <- (1/(6 * A)) * sum((poly_y[i] + poly_y[i + 1]) * (poly_x[i] * 
    poly_y[i + 1] - poly_x[i + 1] * poly_y[i]))
  return(c(x = Cx, y = Cy))
}

#' poly_transform_shape
#' 
#' Mainly for internal use. The function transforms ('projects') arbitrary input 
#' coordinates that are used as parent polygon or shape of the treemap.
#' By default it scales and centers polygon coordinates on a square
#' of 0 to 2000 units.
#' 
#' @param poly (list)  named list of 'x' and 'y' coordinates of the polygon
#' @param xy_max (numeric)  maximum desired expansion of the polygon (default: 2000)
#' 
#' @return A list with slots 'x' and 'y' containing numeric coordinates
#' 
#' @export
poly_transform_shape <- function(poly, xy_max = 2000) {
  
  # check input list
  if(!all(names(poly) %in% c("x", "y"))) {
    stop("Supplied list for 'shape' does not contain 'x' and 'y' slots")
  }
  
  # determine the range for X and Y direction
  range <- list(x = range(poly$x), y = range(poly$y))
  large_side <- ifelse(diff(range$x) >= diff(range$y), "x", "y")
  small_side <- switch(large_side, x = "y", y = "x")
  
  # rescale and shift to center
  scale_factor <- xy_max/diff(range[[large_side]])
  poly[[large_side]] <- (poly[[large_side]]-min(range[[large_side]]))*scale_factor
  poly[[small_side]] <- (poly[[small_side]]-min(range[[small_side]]))*scale_factor
  poly[[small_side]] <- poly[[small_side]]+(xy_max-max(poly[[small_side]]))/2
  return(poly)
}