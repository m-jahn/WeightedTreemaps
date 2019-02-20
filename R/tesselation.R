

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


# Generate one iteration of the Additively Weighted Voronoi diagram
awv <- function(
  s, w, region, debug = FALSE,
  debugCell = FALSE) 
{
  # combine X, Y coordinates and weights as input for
  # C++ tesselation function
  sites <- cbind(s$x, s$y, w)
  write.csv(sites, "lastrun.csv")
  roughCells <- SysbioTreemaps::cropped_voronoi(sites)
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

getCellBorder <- function(
  border,
  seenBefore = numeric(),
  tolerance,
  debug,
  debugCell = FALSE,
  scale = 2000)
{
  N <- nrow(border)
  direction <- 1
  vx <- numeric(N)
  vy <- numeric(N)
  vcount <- 1
  complete <- FALSE
  endCondition <- FALSE
  returning <- length(seenBefore)
  visited <- rep(FALSE, N)
  # Two start cases:
  # Segment that starts on boundary
  # No segments that start on boundary (just start with first segment)
  start <- which(
    border[, 1] == -2 * scale |
    border[, 1] == 2 * scale |
    border[, 2] == -2 * scale |
    border[, 2] == 2 * scale)
  # If this is a return visit, it must be a start point
  # we have not seen before
  start <- start[!start %in% seenBefore]
  if (length(start)) {
    index <- start[1]
    startingX <- border[index, 1]
    startingY <- border[index, 2]
    seenBefore <- c(seenBefore, index)
  } else {
    # Look for segment that *ends* on boundary
    start <- which(
      border[, 3] == -2 * scale |
      border[, 3] == 2 * scale |
      border[, 4] == -2 * scale |
      border[, 4] == 2 * scale)
    start <- start[!start %in% seenBefore]
    if (length(start)) {
      index <- start[1]
      direction <- -1
      startingX <- border[index, 3]
      startingY <- border[index, 4]
      seenBefore <- c(seenBefore, index)
    } else if (returning) {
      # If we're returning for another look
      # (means we ended on the boundary last time)
      # AND we have not found another boundary to start from
      # break straight back out
      complete <- TRUE
      vx <- numeric()
      vy <- numeric()
      endCondition <- "breakout"
    } else {
      # Start at first point
      index <- 1
      startingX <- border[index, 1]
      startingY <- border[index, 2]
    }
  }
  # Following segments:
  # First check whether next segment follows on from current one
  # If not, search all other segments for segment that follows on
  # If no match, find segment that *ends* at current end and
  #   follow that segment *backwards*
  while (!complete) {
    if (debug && debugCell)
      cat(paste(index, direction, "\n"))
    if (direction > 0) {
      x1 <- 1
      y1 <- 2
      x2 <- 3
      y2 <- 4
    } else {
      x1 <- 3
      y1 <- 4
      x2 <- 1
      y2 <- 2
    }
    vx[vcount] <- border[index, x1]
    vy[vcount] <- border[index, y1]
    vcount <- vcount + 1
    visited[index] <- TRUE
    if (all(visited)) {
      complete <- TRUE
    } else {
      # Try next segment
      nextIndex <- index + direction
      endX <- border[index, x2]
      endY <- border[index, y2]
      startX <- border[nextIndex, x1]
      startY <- border[nextIndex, y1]
      if (nextIndex > 0 && nextIndex <= N &&
          sim(endX, startX, tolerance) &&
          sim(endY, startY, tolerance)) {
        index <- nextIndex
      } else {
        # Try segment that starts where we ended
        newIndex <-
          startsAt(border, endX, endY, direction, tolerance)
        # Do NOT use visited segment
        newIndex <- newIndex[!visited[newIndex]]
        if (length(newIndex)) {
          index <- newIndex[1]
        } else {
          # Try segment that ends where we ended
          direction <- -direction
          newIndex <-
            startsAt(border, endX, endY, direction,
                     tolerance)
          # Do NOT use visited segment
          newIndex <- newIndex[!visited[newIndex]]
          if (length(newIndex)) {
            index <- newIndex[1]
          } else {
            # Run out of segments
            # cat("Found no next edge\n")
            complete <- TRUE
            warning("Failed to trace cell")
            # recover()
          }
        }
      }
    }
    # Stopping conditions:
    # Hit boundary
    # Got back to start
    endCondition <-
      stopping(border[index, x2], border[index, y2],
               startingX, startingY, tolerance)
    if (endCondition == "boundary") {
      # Add boundary nodes to border
      vx[vcount] <- border[index, x1]
      vx[vcount + 1] <- border[index, x2]
      vy[vcount] <- border[index, y1]
      vy[vcount + 1] <- border[index, y2]
      vcount <- vcount + 2
      seenBefore <- c(seenBefore, index)
      complete <- TRUE
    } else if (endCondition == "start") {
      complete <- TRUE
    }
  }
  list(
    x = vx[1:(vcount - 1)],
    y = vy[1:(vcount - 1)],
    end = endCondition,
    seen = seenBefore
  )
}

tidyCell <-
  function(cell,
           tolerance,
           debug = FALSE,
           debugCell = FALSE) {
    if (debug && debugCell)
      print(cell$vertex)
    border <- cell$border
    # Handle empty cells
    if (is.null(border))
      return(NULL)
    ok <- !apply(cell$border, 1, function(x)
      any(is.na(x)))
    border <- border[ok,]
    
    result <- getCellBorder(border,
                            tolerance = tolerance,
                            debug = debug,
                            debugCell = debugCell)
    if (result$end == "boundary") {
      # Check for other boundaries
      results <- list(result)
      repeat {
        result <- getCellBorder(
          border,
          seenBefore = result$seen,
          tolerance = tolerance,
          debug = debug,
          debugCell = debugCell
        )
        if (result$end == "breakout")
          break
        results <- c(results, list(result))
      }
      if (length(results) == 1) {
        result <- results[[1]]
      } else {
        result <- results
        class(result) <- "multipleBorders"
      }
    }
    
    if (inherits(result, "multipleBorders") ||
        result$end == "boundary") {
      # Need to close the cell
      cellBorder <- closeCell(result, cell$vertex, tolerance)
    } else {
      # if (end == "start") {
      # This covers two cases:
      # end == "start" means that we have completed a loop
      # end == FALSE means that we ran out of points
      # The latter case may produce some unpredictable results
      cellBorder <- list(x = c(result$x, result$x[1]),
                         y = c(result$y, result$y[1]))
    }
    cellBorder
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
  
  # Start and end on same side
  if (startSide == endSide) {
    # Special case:  startPOINT and endPOINT same
    if (sim(x[1], x[N], tol) &&
        sim(y[1], y[N], tol)) {
      newx <- stretchX(x, y, N, startSide)
      newy <- stretchY(x, y, N, startSide)
      x <- newx
      y <- newy
    }
    # Just join start to end
    cell <- list(x = c(x, x[1]), y = c(y, y[1]))
    if (point.in.polygon(vertex[1], vertex[2],
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