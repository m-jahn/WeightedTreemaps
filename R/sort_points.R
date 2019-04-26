# Function to sort a set of points/coordinates in a clockwise or anti-clockwise
# direction.
# 
# original author: Stuart K. Grange
# modified by Michael Jahn

# function to rearrange df column wise without reordering
reorder_points <- function(s, test) {
  if (any(s %in% test)) {
    pos <- which(s %in% test)
    if (!all(pos %in% c(1, length(s)))) {
      c(tail(pos, 1):length(s), 1:(tail(pos, 1)-1))
    }
  } else {
    NULL
  }
}

# main sorting function
sort_points <- function(df, y = "y", x = "x", clockwise = TRUE, vertex) {
  
  # Get centre (-oid) point of points
  x_centre <- mean(df[, x])
  y_centre <- mean(df[, y])
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  # d$angle_degrees <- d$angle * 180 / pi
  
  # Arrange by angle
  if (clockwise) {
    
    df <- df[order(df$angle, decreasing = TRUE), ]
    
  } else {
    
    df <- df[order(df$angle, decreasing = FALSE), ]
    
  }
  
  # Drop intermediate variables
  df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  # If this is a 'polygon' touching the boundaries, rearrange points
  # starting with the first point on boundary, but without sorting df
  # 
  #df <- data.frame(x = seq(4000, -1000, length.out = 20), y = seq(500, -4000, length.out = 20))
  #df <- df[c(7:20, 1:6), ]
  
  # try to find order for all 4 combinations of x and y and
  # positive and negative boundary
  new_order <- mapply(FUN = reorder_points,
    s = list(df[[1]], df[[2]]),
    test = c(-4000, -4000, 4000, 4000))
  
  # apply new order only if boundary points are not at the ends of the vector.
  # If several ordering possibilities exist, choose the one with lower ranked minimum
  if (any(!sapply(new_order, FUN = is.null))) {
    ind <- new_order %>% {sapply(., FUN = function(x) if (is.null(x)) NA else which.min(x))} %>% 
      unlist %>% which.min
    df[new_order[[ind]], ]
  } else {
    df
  }
}
