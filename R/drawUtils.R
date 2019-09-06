#' @importFrom grid gpar
#' @importFrom grid grid.polygon
#' @importFrom grid grid.text
#' @importFrom scales rescale
#' @importFrom gpclib area.poly
#' @importFrom methods as
#' @importFrom methods new

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

# calculate sector polygon from boundary input
draw_sector <- function(
  level,
  lower_bound,
  upper_bound,
  diameter_inner,
  diameter_sector,
  name) {
  
  # compute_sector from lower and upper bounds and diameter arguments
  segment <- c(lower_bound, upper_bound) * 2 * pi
  a <- diameter_inner + (diameter_sector * (level - 1))
  z <- seq(segment[1], segment[2], by = pi/400)
  xx <- c(a * cos(z), rev((a + diameter_sector) * cos(z)))
  yy <- c(a * sin(z), rev((a + diameter_sector) * sin(z)))
  # rescale for canvas dimensions [0, 4000] and convert into gpclib polygon
  poly = suppressWarnings(as(list(x = (xx+1)*1000, y = (yy+1)*1000), "gpc.poly"))
  
  # return list of polygon properties
  list(
    names = name,
    k = poly,
    a = gpclib::area.poly(poly),
    level = level
  )
  
}

# # draw labels for sunburst treemap
# # here are some possible arguments
# lwd = 1, 
# labels = NA,
# label.col = grey(0.5), 
# label.thresh = NA, 
#
# if (draw.label) {
#   if (a * cos(median(z)) >= 0)
#     side = 1
#   else
#     side = -1
#   sinz <- sin(median(z))
#   cosz <- cos(median(z))
#   
#   #draw label arcs
#   z <- z[-c(1, length(z))]
#   lines(c(0.72 * cos(z[1]), 0.75 * cos(z), 0.72 * cos(tail(z, 1))),
#         c(0.72 * sin(z[1]), 0.75 * sin(z), 0.72 * sin(tail(z, 1))),
#         col = label.col)
#   
#   #draw label lines
#   lines(
#     x = c(0.75 * cosz, 0.75 * cosz + 0.25 * cosz * abs(sinz), 0.8 * side),
#     y = c(0.75 * sinz, 0.75 * sinz + 0.25 * sinz * abs(sinz), 
#       0.75 * sinz + 0.25 * sinz * abs(sinz)),
#     col = label.col
#   )
#   
#   #draw label text
#   text(
#     x = 0.8 * side,
#     y = 0.75 * sinz + 0.25 * sinz * abs(sinz),
#     labels = substring(sector.name, 1, 18),
#     col = label.col,
#     pos = 3 + side,
#     offset = 0.2,
#     cex = 0.7
#   )
# }