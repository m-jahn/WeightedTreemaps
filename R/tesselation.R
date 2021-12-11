#' @importFrom sp point.in.polygon
#' @importFrom sf st_difference
#' @importFrom sf st_polygon
#' @importFrom sf st_intersection
#' @importFrom sf st_cast
#' @importFrom dplyr %>%
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
  roughCells <- cropped_voronoi(sites)
  if (is.null(roughCells)) {
    return(NULL)
  }
  tidyCells <- lapply(roughCells, tidyCell, tolerance = 0.0015)
  if (sapply(tidyCells, is.null) %>% any) {
    return(NULL)
  }
  trimCells(tidyCells, region)
}


tidyCell <-
  function(cell, tolerance) {
    
    # if cell touches the border at two points, we need to close it
    # this is not necessary if cell touches border at 4 points (like a stripe)
    if (sum(
      cell$border$x %in% c(4000, -4000), 
      cell$border$y %in% c(4000, -4000)) == 2
    ) {
      
      closeCell(cell$border, cell$vertex, tol = tolerance)
      
    } else {
      
      # return a list of the polygon
      list(
        x = cell$border$x,
        y = cell$border$y,
        end = "boundary"
      )
      
    }
    
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

# convert sf polygon to coordinates
to_coords <- function(sfpoly) {
  list(x = sfpoly[[1]][, 1], y = sfpoly[[1]][, 2])
}

# convert coordinates to sf polygon
to_sfpoly <- function(coords) {
  # close polygon
  coords <- round(c(coords$x, coords$y), 4)
  coords <- matrix(coords, ncol = 2)
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords <- rbind(coords, coords[1, ])
  }
  sf::st_polygon(list(coords))
}


closeCell <- function(cell, vertex, tol, scale = 2000) {
  # Two options:  go round clip region boundary clockwise or anti-clockwise
  # Try first one, check if vertex is "inside" the result
  # If not, do second one (and check that vertex is "inside" that result!)
  x <- cell$x
  y <- cell$y
  
  # ASSUME that both first and last vertices are on boundary!
  N <- length(x)
  startSide <- side(x[1], y[1])
  endSide <- side(x[N], y[N])
  
  # exit if not both end points lie on boundary
  if (length(startSide) != 1 | length(endSide) != 1) {
    return(NULL)
  }
    
  # Start and end on same side
  if (startSide == endSide) {
    
    cell <- list(x = x, y = y)
    if (sp::point.in.polygon(vertex[1], vertex[2],
                         cell$x, cell$y) == 0) {
      boundRect <- to_sfpoly(list(
        x = c(-2 * scale,-2 * scale, 2 * scale, 2 * scale),
        y = c(-2 * scale, 2 * scale, 2 * scale,-2 * scale)
      ))
      # "Subtract" smallCell from bound rect to get largeCell
      cellPoly <- to_sfpoly(cell)
      cellPoly <- sf::st_difference(boundRect, cellPoly)
      
      pts <- to_coords(cellPoly)
      #cell <- list(x = c(pts$x, pts$x[1]),
      #             y = c(pts$y, pts$y[1]))
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


# convert cell from awv core function to sf polygon
convertCell <- function(cell) {
  # Handle empty cells
  if (is.null(cell)) {
    sf::st_polygon()
  } else {
    to_sfpoly(cell)
  }
}

# Take polygons from Voronoi cells and intersect them with
# outer polygon (e.g., circle radius 1000)
trimCells <- function(cells, region) {
  polys <- lapply(cells, convertCell)
  polys <- lapply(polys, function(poly) {
    poly <- sf::st_intersection(poly, region)
    if ("MULTIPOLYGON" %in% class(poly)) {
      poly <- suppressWarnings(sf::st_cast(poly, to = "POLYGON"))
    }
    poly
  })
  return(polys)
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
  # is sampled. The reason is that sp::spsample() does not always sample
  # the correct number of coordinates, but too few or too many
  repeat {
    
    sampled <- tryCatch({
      points <- sp::spsample(
        sp::Polygon(coords = ParentPoly),
        n = n, 
        type = ifelse(positioning == "random", "random", "nonaligned")
      )
      points@coords}, error = function(e) NULL
    )
    
    if (is.null(sampled) || nrow(sampled) != n) {
      next
    } else {
      if (positioning %in% c("clustered", "clustered_by_area")) {
        ord <- sampled %>% scale %>%
          apply(1, function(x) sum(abs(x))) %>%
          order(decreasing = TRUE)
        sampled <- sampled[ord, ]
      }
      return(sampled)
    }
  }
}


