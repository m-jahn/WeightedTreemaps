#' poly_area
#' 
#' Mainly for internal use. Determines the area of a polygon based on 
#' its x and y coordinates.
#' This function is a reimplementation of soiltexture::TT.polygon.area()
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
#' Mainly for internal use Determines the centroids of a polygon based on 
#' its x and y coordinates.
#' This function is a reimplementation of soiltexture::TT.polygon.area()
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
