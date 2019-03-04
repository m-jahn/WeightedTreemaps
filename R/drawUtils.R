

#  Copyright (C) 2012 Paul Murrell
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.gnu.org/licenses/gpl.txt
#
#' @importFrom grid gpar
#' @importFrom grid grid.polygon
#' @importFrom grid grid.text
#' @importFrom scales rescale 

# function to coerce and rescale different types of input to 
# numeric range between 1 and 100 (for color coding)
convertInput <- function(x, from = NULL, to = c(1, 100)) {
  if (is.character(x)) {
    if (all(!is.na(suppressWarnings(as.numeric(x))))) {
      x = as.numeric(x)
    } else {
      x = as.factor(x) %>%
        as.numeric
    }
  }
  if (is.numeric(x)) {
    scales::rescale(x, 
      from = {if (!is.null(from)) from else range(x)}, 
      to = to) %>%
      round %>%
      replace(., . > to[2], to[2]) %>%
      replace(., . < to[1], to[1])
  } else {
    stop("Input data is not of type numeric, factor, or character. Color-coding impossible.")
  }
}

drawPoly <- function(p, name, fill, lwd, col) {
  if (length(p@pts)) {
    pts <- getpts(p)
    grid::grid.polygon(
      pts$x,
      pts$y,
      default = "native",
      gp = gpar(col = col, lwd = lwd, fill = fill),
      name = name)
  }
}

polyRangeX <- function(p) {
  if (length(p@pts)) {
    pts <- getpts(p)
    range(pts$x)
  } else {
    NA
  }
}

polyRangeY <- function(p) {
  if (length(p@pts)) {
    pts <- getpts(p)
    range(pts$y)
  } else {
    NA
  }
}

drawRegions <- function(
  result,
  debug = FALSE,
  label = TRUE,
  label.col = grey(0.5),
  lwd = 2, col = grey(0.8), 
  fill = NA)
{
  names <- result$names
  k <- result$k
  sites <- result$s
  
  # draw polygon, pass graphical parameters to drawPoly function
  mapply(drawPoly, k, names, fill = fill,
    SIMPLIFY=FALSE,
    MoreArgs = list(lwd = lwd, col = col)
  )
  
  if (label) {
    
    # function to determine label sizes for each individual cell
    # based on cell dimension and label character length
    cex = sqrt(unlist(result$a)) * 0.01 / nchar(names)  %>%
      round(1)
    grid::grid.text(names,
      sites$x,
      sites$y,
      default = "native",
      gp = gpar(cex = cex, col = label.col)
    )
    
  }
}
