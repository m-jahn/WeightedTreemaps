#' poly_sortpoints
#' 
#' Sort a set of points/coordinates in a clockwise or anti-clockwise
#' direction. For internal use. This function is now deprecated as 
#' its functionality has moved to the C++ core function, \code{cropped_voronoi()}.
#' @param df (data frame) A data.frame with X and Y coordinate columns
#' @param x (character) name of the X coordinate column
#' @param y (character) name of the Y coordinate column
#' @param clockwise (logical) sort points clockwise (default, TRUE) or 
#'   counter-clockwise.
#' @param vertex (numeric) Vector of length 2 giving the centroid of the polygon.
#'   Default is NULL, centroid is calculated from cordinates.
#' 
#' @author original author Stuart K. Grange, modified by Michael Jahn
#'      
#' @importFrom dplyr %>%
#' @export
poly_sortpoints <- function(df, x = "x", y = "y", clockwise = TRUE, vertex = NULL) {
  
  # Get centre (-oid) point of points
  if (is.null(vertex)) {
    x_centre <- mean(df[, x])
    y_centre <- mean(df[, y])
  } else {
    x_centre <- vertex[1]
    y_centre <- vertex[2]
  }
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  
  
  # check for straight lines as polygon border. These can not be ordered from
  # center and have random boundary positions. Simply order by y value
  if (length(unique(round(df$angle, 2))) == 2) {
    df <- df[order(df[[y]], decreasing = TRUE), ]
  } else if (clockwise) {
  # Otherwise arrange by angle
    df <- df[order(df$angle, decreasing = TRUE), ]
  } else {
    df <- df[order(df$angle, decreasing = FALSE), ]
  }
  
  # Drop intermediate variables
  df <- df[c(x, y)]

  # If this is a polygon touching the boundaries, rearrange points
  # starting with the first point on boundary, but without sorting df.
  # First find positions of boundary points
  boundary_pos <- apply(df, 1, function(r) {
    any(r %in% c(4000, -4000))
  }) %>% which
  
  # reorder only if boundary positions are not at the ends of the vector.
  # If several ordering possibilities exist, choose the one with lower ranked minimum
  if (!all(boundary_pos %in% c(1, nrow(df)))) {
    c(tail(boundary_pos, 1):nrow(df), 1:(tail(boundary_pos, 1)-1)) %>%
    df[., ]
  } else {
    df
  }

}
