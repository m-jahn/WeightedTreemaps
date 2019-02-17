

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
#' @importFrom grid polygonGrob
#' @importFrom grid textGrob
#' @importFrom grid grid.draw

drawPoly <- function(p, name, gp = gpar()) {
  if (length(p@pts)) {
    pts <- getpts(p)
    polygonGrob(
      pts$x,
      pts$y,
      default = "native",
      gp = gp,
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
  lwd=2, col=grey(0.8), 
  fill=NA)
{
  names <- result$names
  k <- result$k
  sites <- result$s
  weights <- result$w
  
  # draw polygon, pass graphical parameters to drawPoly function
  polylist <- mapply(drawPoly, k, names,
    SIMPLIFY=FALSE,
    MoreArgs = list(gp = gpar(
      lwd = lwd, 
      col = col,
      fill = fill
    ))
  )
  names(polylist) = names
  
  if (label) {
    
    # function to determine label sizes for each individual cell
    # based on cell dimension and label character length
    cex = sqrt(unlist(result$a)) * 0.05 / nchar(names)  %>%
      round(1)

    polylist$labels <- textGrob(names,
      sites$x,
      sites$y,
      default = "native",
      gp = gpar(cex = cex, col = label.col)
    )
    
  }
  # return list of polygons; optionally draw them with debug=TRUE
  if (debug) {lapply(polylist, grid.draw)}
  polylist
}
