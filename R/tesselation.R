

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
#' @import gpclib
#' @importFrom sp point.in.polygon
#' @importFrom dplyr filter
#' @importFrom dplyr filter_all
#' @importFrom dplyr any_vars

# Generate one iteration of the Additively Weighted Voronoi diagram
awv <- function(
  s, w, region, debug = FALSE,
  debugCell = FALSE) 
{
  # combine X, Y coordinates and weights as input for
  # C++ tesselation function
  sites <- cbind(s$x, s$y, w)
  roughCells <- SysbioTreemaps::cropped_voronoi(sites)
  if (is.null(roughCells)) {
    return(NULL)
  }
  tolerance <- 0.0015
  # tolerance <- max(diff(range(s$x)), diff(range(s$y)))*.000001
  tidyCells <- tidyCells(roughCells, tolerance, debug, debugCell)
  trimCells(tidyCells, region)
}


# Tidy the cell information
# Return a list of lists with x/y
tidyCells <-
  function(
    cells,
    tolerance,
    debug = FALSE,
    debugCell = FALSE) 
  { 
    lapply(cells, tidyCell, tolerance, debug, debugCell)
  }

tidyCell <-
  function(
    cell,
    tolerance,
    debug = FALSE,
    debugCell = FALSE) 
  {
    if (debug && debugCell)
      print(cell$vertex)
    
    # Handle empty cells
    if (is.null(cell$border)) {
      print("One cell has NULL borders")
      return(NULL)
    }
    
    # clean borders by rounding and removing duplicate points
    # as well as NA entries, and sort points in clockwise direction
    border <- rbind(
      setNames(cell$border[, 1:2], c("x", "y")),
      setNames(cell$border[, 3:4], c("x", "y"))
    )
    clean_border <- border %>% round(4) %>%
      as.data.frame %>% na.omit %>%
      dplyr::filter(!(duplicated.data.frame(.))) %>%
      sort_points(y = "y", x = "x", clockwise = TRUE, vertex = cell$vertex)
    
    # if cell touches the border, we need to close it
    # this does not apply if both points lie on same border? Or if there is no end side
    if (any(apply(clean_border, 2, function(x) any(x %in% c(4000, -4000))))) {
      
      result <- closeCell(clean_border, cell$vertex, tol = tolerance)
      
    } else {
      
      # return a list of the polygon
      result <- list(
        x = clean_border$x,
        y = clean_border$y,
        #x = c(clean_border$x, clean_border$x[1]),
        #y = c(clean_border$y, clean_border$y[1]),
        end = "boundary"
      )
      
    }
    
    result
  }

# SIDES

# 1 = left
# 2 = top
# 3 = right
# 4 = bottom

# CORNERS

# 1 = top-left
# 2 = top-right
# 3 = bottom-right
# 4 = bottom-left

side <- function(x, y, scale = 2000) {
  if (x == -2 * scale)
    return(1)
  if (y == 2 * scale)
    return(2)
  if (x == 2 * scale)
    return(3)
  if (y == -2 * scale)
    return(4)
}

clockCorner <- function(side) {
  switch(side, 1, 2, 3, 4)
}

clockSide <- function(corner) {
  switch(corner, 2, 3, 4, 1)
}

antiCorner <- function(side) {
  switch(side, 4, 1, 2, 3)
}

antiSide <- function(corner) {
  switch(corner, 1, 2, 3, 4)
}



closeClock <- function(x, y, start, end, scale = 2000) {
  cornerX <- c(-2 * scale, 2 * scale, 2 * scale,-2 * scale)
  cornerY <- c(2 * scale, 2 * scale,-2 * scale,-2 * scale)
  
  side <- end
  repeat {
    corner <- clockCorner(side)
    x <- c(x, cornerX[corner])
    y <- c(y, cornerY[corner])
    side <- clockSide(corner)
    if (side == start) {
      break
    }
  }
  x <- c(x, x[1])
  y <- c(y, y[1])
  list(x = x, y = y)
}

closeAnti <- function(x, y, start, end, scale = 2000) {
  cornerX <- c(-2 * scale, 2 * scale, 2 * scale,-2 * scale)
  cornerY <- c(2 * scale, 2 * scale,-2 * scale,-2 * scale)
  
  side <- end
  repeat {
    corner <- antiCorner(side)
    x <- c(x, cornerX[corner])
    y <- c(y, cornerY[corner])
    side <- antiSide(corner)
    if (side == start) {
      break
    }
  }
  x <- c(x, x[1])
  y <- c(y, y[1])
  list(x = x, y = y)
}

closeCell <- function(cell, vertex, tol) {
  UseMethod("closeCell")
}

stretchX <- function(x, y, N, side, scale = 2000) {
  switch(side,
         c(x[1], max(x), x[N]),
         c(max(-2 * scale, x[1] - scale * .05),
           x[which.min(y)],
           min(2 * scale, x[N] + scale * .05)),
         c(x[1], min(x), x[N]),
         c(max(-2 * scale, x[1] - scale * .05),
           x[which.max(y)],
           min(2 * scale, x[N] + scale * .05)))
}

stretchY <- function(x, y, N, side, scale = 2000) {
  switch(side,
         c(max(-2 * scale, y[1] - scale * .05),
           y[which.max(x)],
           min(2 * scale, y[N] + scale * .05)),
         c(y[1], max(y), y[N]),
         c(max(-2 * scale, y[1] - scale * .05),
           y[which.min(x)],
           min(2 * scale, y[N] + scale * .05)),
         c(y[1], min(y), y[N]))
}

closeCell.default <- function(cell, vertex, tol, scale = 2000) {
  # Two options:  go round clip region boundary clockwise or anit-clockwise
  # Try first one, check if vertex is "inside" the result
  # If not, do second one (and check that vertex is "inside" that result!)
  x <- cell$x
  y <- cell$y
  
  # It is possible to get here with a cell that STARTS on a
  # boundary but does not end on a boundary (because voronoiDiagram
  # is capable of producing this sort of thing sometimes;  I have seen it!)
  # This case is characterised by cell$end being FALSE
  # In that case, just hail mary and join start to end
  # (so that end is also on boundary)
  if (is.logical(cell$end)) {
    if (!cell$end) {
      x <- c(x, x[1])
      y <- c(y, y[1])
    }
  }
  
  # ASSUME that both first and last vertices are on boundary!
  N <- length(x)
  startSide <- side(x[1], y[1])
  endSide <- side(x[N], y[N])

  print(c(startSide, endSide))
  # Start and end on same side
  if (startSide == endSide) {
    
    # Special case:  startPOINT and endPOINT same
    # if (sim(x[1], x[N], tol) &&
    #     sim(y[1], y[N], tol)) {
    #   
    #   newx <- stretchX(x, y, N, startSide)
    #   newy <- stretchY(x, y, N, startSide)
    #   x <- newx
    #   y <- newy
    # }
    # Just join start to end
    #cell <- list(x = c(x, x[1]), y = c(y, y[1]))
    cell <- list(x = x, y = y)
    if (sp::point.in.polygon(vertex[1], vertex[2],
                         cell$x, cell$y) == 0) {
      boundRect <- suppressWarnings(as(list(
        x = c(-2 * scale,-2 * scale,
              2 * scale, 2 * scale),
        y = c(-2 * scale, 2 *
                scale,
              2 * scale,-2 * scale)
      ),
      "gpc.poly"))
      # "Subtract" smallCell from bound rect to get largeCell
      cellPoly <- suppressWarnings(as(cell, "gpc.poly"))
      cellPoly <-
        gpclib::intersect(gpclib::append.poly(cellPoly, boundRect), boundRect)
      pts <- getpts(cellPoly)
      cell <- list(x = c(pts$x, pts$x[1]),
                   y = c(pts$y, pts$y[1]))
      if (sp::point.in.polygon(vertex[1], vertex[2],
                           cell$x, cell$y) == 0) {
        stop("Failed to close cell")
      }
    }
    
  } else {
    
    cell <- closeClock(x, y, startSide, endSide)
    if (sp::point.in.polygon(vertex[1], vertex[2],
                         cell$x, cell$y) == 0) {
      cell <- closeAnti(x, y, startSide, endSide)
      if (sp::point.in.polygon(vertex[1], vertex[2],
                           cell$x, cell$y) == 0) {
        stop("Failed to close cell")
      }
    }
  }
  cell
}

closeCell.multipleBorders <- function(cell, vertex, tol) {
  result <- lapply(cell, closeCell.default, vertex, tol)
  class(result) <- "multipleCells"
  result
}

# Does this constant need adjusting if 'scale' in init.R
# is altered??
# The .0015 is fairly sensitive
# Examples seen where .001 is (just) too small
# Examples seen where .005 is too big
sim <- function(a, b, tol = .0015) {
  abs(a - b) < tol
}

stopping <- function(endX, endY, startX, startY, tol, scale = 2000) {
  if (endX == -2 * scale || endX == 2 * scale ||
      endY == -2 * scale || endY == 2 * scale) {
    # cat("Hit boundary\n")
    return("boundary")
  }
  if (sim(endX, startX, tol) && sim(endY, startY, tol)) {
    # cat("Back to start\n")
    return("start")
  }
  return(FALSE)
}

startsAt <- function(border, x, y, direction, tol) {
  if (direction > 0) {
    x1 <- 1
    y1 <- 2
  } else {
    x1 <- 3
    y1 <- 4
  }
  which(sim(border[, x1], x, tol) &
          sim(border[, y1], y, tol))
}

# Take polygons from Voronoi cells and intersect them with
# outer polygon (e.g., circle radius 1000)
# Return list of "gpc.poly"
convertCell <- function(cell) {
  # Handle empty cells
  if (is.null(cell)) {
    new("gpc.poly")
  } else if (inherits(cell, "multipleCells")) {
    polys <- lapply(cell, function(p) {
      class(p) <- "list"
      suppressWarnings(as(p, "gpc.poly"))
    })
    Reduce(gpclib::intersect, polys)
  } else {
    suppressWarnings(as(cell, "gpc.poly"))
  }
}


trimCells <- function(cells, region) {
  polys <- lapply(cells, convertCell)
  lapply(polys, gpclib::intersect, region)
}


# Extracting coordinates from "gpc.poly"

# It is possible for us to end up with a cell containing a hole
# (because when the cell boundary is either zero area or
#  contains a zero-area indent, gpclib::intersect() can
#  produce isolated islands)

# To handle these cases, we just ignore the holes (which should
# be zero-area anyway) and take the outer boundary.

# There is defensive code in there to warn if the unexpected
# happens and there is more than one outer boundary.
getpts <- function(x) {
  pts <- gpclib::get.pts(x)
  nonholes <- sapply(pts, function(y) !y$hole)
  if (sum(nonholes) < 1 || sum(nonholes) > 1) {
    warning("Cell or region with more than one boundary")
  }
  border <- which(nonholes)[1]
  list(x=pts[[border]]$x,
       y=pts[[border]]$y)
}


# generate starting coordinates within the boundary polygon
# using sp package's spsample function. The set.seed() is important
# here because it makes the sampling reproducible
# (same set of coordinates for same query) which will
# lead to similar map layout for similar input data
samplePoints <- function(ParentPoly, n, seed, positioning) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # This loop keeps repeating until the correct number of coordinates 
  # is sampled. The reason is that sp::spsample() does not always samples
  # the correct number of coordinates, but too few or too many
  repeat {
    
    sampled <- sp::Polygon(coords = ParentPoly) %>%
      sp::spsample(n = n, 
        type = ifelse(positioning == "random", "random", "nonaligned")
      ) %>% { .@coords }
      
    if (nrow(sampled) != n) {
      next
    } else {
      if (positioning %in% c("clustered", "clustered_by_area")) {
        sampled <- sampled %>% scale %>%
          apply(1, function(x) sum(abs(x))) %>%
          order(decreasing = TRUE) %>% sampled[., ]
      }
      return(sampled)
    }
  }
}


