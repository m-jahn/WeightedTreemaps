#' @importFrom grid gpar
#' @importFrom grid grid.polygon
#' @importFrom grid grid.text
#' @importFrom grid grid.lines
#' @importFrom sf st_area
#' @importFrom scales rescale
#' @importFrom methods as
#' @importFrom methods new
#' @importFrom stats median
#' @importFrom colorspace lighten
#' @importFrom colorspace rainbow_hcl

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
    res <- scales::rescale(x,
      from = {if (!is.null(from)) from else range(x)},
      to = to) %>% round
    res <- replace(res, res > to[2], to[2])
    res <- replace(res, res < to[1], to[1])
    res
  } else {
    stop("Input data is not of type numeric, factor, or character. Color-coding impossible.")
  }
}

drawPoly <- function(sfpoly, name, fill, lwd, col) {
  if (length(sfpoly)) {
    pts <- to_coords(sfpoly)
    grid::grid.polygon(
      pts$x,
      pts$y,
      default = "native",
      gp = gpar(col = col, lwd = lwd, fill = fill),
      name = name)
  }
}

polyRangeX <- function(sfpoly) {
  if (length(sfpoly)) {
    pts <- to_coords(sfpoly)
    range(pts$x)
  } else {
    NA
  }
}

polyRangeY <- function(sfpoly) {
  if (length(sfpoly)) {
    pts <- to_coords(sfpoly)
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
    SIMPLIFY = FALSE,
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
  name,
  custom_color) {

  # compute_sector from lower and upper bounds and diameter arguments
  segment <- c(lower_bound, upper_bound) * 2 * pi
  a <- diameter_inner + (diameter_sector * (level - 1))
  z <- seq(segment[1], segment[2], by = pi/400)
  xx <- c(a * cos(z), rev((a + diameter_sector) * cos(z)))
  yy <- c(a * sin(z), rev((a + diameter_sector) * sin(z)))
  # rescale for canvas dimensions [0, 2000] and convert into sfpoly polygon
  poly = to_sfpoly(list(x = (xx+1)*1000, y = (yy+1)*1000))

  # return list of polygon properties
  list(
    name = name,
    poly = poly,
    area = sf::st_area(poly),
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    level = level,
    custom_color = custom_color
  )

}

# function to draw labels for voronoi treemap
draw_label_voronoi <- function(
  cells,
  label_level,
  label_size,
  label_color
) {

  for (tm_slot in rev(cells)) {

    if (tm_slot$level %in% label_level) {

      # determine label sizes for each individual cell
      # based on cell dimension and label character length
      label_cex <- sqrt(tm_slot$area) / (100 * nchar(tm_slot$name)) %>% round(1)

      # additionally scale labels size and color from supplied options
      if (length(label_size) == 1) {
        label_cex <- label_cex * label_size
      } else {
        label_cex <- label_cex * label_size[which(label_level %in% tm_slot$level)]
      }

      # determine label color
      if (length(label_color) == 1) {
        label_col <- label_color
      } else {
        label_col <- label_color[which(label_level %in% tm_slot$level)]
      }

      # draw labels
      grid::grid.text(
        tm_slot$name,
        tm_slot$site[1],
        tm_slot$site[2],
        default = "native",
        gp = gpar(cex = label_cex, col = label_col)
      )

    }
  }

}


# function to draw labels for sunburst treemap
draw_label_sunburst <- function(
  cells,
  label_level,
  label_size,
  label_color,
  diameter
) {

  lapply(cells, function(tm_slot) {

    if (tm_slot$level %in% label_level) {

      # determine label size and color from supplied options
      if (length(label_size) > 1) {
        label_cex <- label_size[1]
        warning("'label_size' should only have length 1. Using first argument.")
      } else {
        label_cex <- label_size
      }

      if (length(label_color) > 1) {
        label_col <- label_color[1]
        warning("'label_color' should only have length 1. Using first argument.")
      } else {
        label_col <- label_color
      }

      # compute_sector from lower and upper bounds and diameter arguments
      segment <- c(tm_slot$lower_bound, tm_slot$upper_bound) * 2 * pi
      z <- seq(segment[1], segment[2], by = pi/400)
      if (diameter * cos(stats::median(z)) >= 0) side = 1 else side = -1
      sinz <- sin(median(z))
      cosz <- cos(median(z))
      d1 <- diameter+0.02
      d2 <- diameter+0.05
      d3 <- diameter+0.10

      # draw label arcs
      z <- z[-c(1, length(z))]
      grid::grid.lines(
        (c(d1 * cos(z[1]), d2 * cos(z), d1 * cos(tail(z, 1)))+1)*1000,
        (c(d1 * sin(z[1]), d2 * sin(z), d1 * sin(tail(z, 1)))+1)*1000,
        default.units = "native",
        gp = gpar(lwd = label_cex, col = label_col)
      )

      # draw label lines
      grid::grid.lines(
        x = (c(d2 * cosz, d2 * cosz + 0.15 * cosz * abs(sinz), d3 * side)+1)*1000,
        y = (c(d2 * sinz, d2 * sinz + 0.15 * sinz * abs(sinz),
          d2 * sinz + 0.15 * sinz * abs(sinz))+1)*1000,
        default.units = "native",
        gp = gpar(lwd = label_cex, col = label_col)
      )

      #draw label text
      grid::grid.text(
        label = substr(tm_slot$name, 1, 18),
        x = ((d3+0.02) * side+1)*1000,
        y = ((d2 * sinz + 0.15 * sinz * abs(sinz))+1)*1000,
        just = ifelse(side == 1, "left", "right"),
        default.units = "native",
        gp = gpar(cex = label_cex, col = label_col)
      )

    }
  }) %>% invisible
}


# function to add colors to a treemap object
add_color <- function(treemap, color_palette = NULL,
  color_type = "categorical", color_level = 1,
  color_steps = 10, custom_range = NULL) {

  # CASE 1: CATEGORICAL
  if (color_type %in% c("categorical", "both")) {
    # determine number of required colors
    if (length(color_level) == 1) {
      color_list <- unique(treemap@data[[treemap@call$levels[color_level]]])
    } else {
      color_list <- apply(treemap@data[treemap@call$levels[color_level]], 2, unique) %>%
        unlist
    }
  }

  # CASE 2: CELL AREA
  # determine total area
  total_area <- lapply(treemap@cells, function(tm_slot) {
    if (tm_slot$level %in% color_level) tm_slot$area
  }) %>% unlist %>% sum
  # determine number of required colors
  if (color_type == "cell_size") {
    cell_sizes <- lapply(treemap@cells, function(tm_slot) {
      if (tm_slot$level %in% color_level) tm_slot$area/total_area
    }) %>% unlist
    color_list <- cell_sizes %>% pretty(n = color_steps)
  }

  # CASE 3: CUSTOM COLOR
  # 'custom_color' to use a color index supplied during treemap generation
  if (color_type == "custom_color") {

    # determine number of required colors
    color_list <- lapply(treemap@cells, function(tm_slot) {
        if (tm_slot$level %in% color_level) tm_slot$custom_color
      }) %>% unlist %>% pretty(n = 10)
  }

  # DEFINE PALETTE
  # generate palette with defined number of colors
  # use a custom data range if supplied by user (does not work for categorical)
  if (!is.null(custom_range) & !(color_type %in% c("categorical", "both"))) {
    color_list <- custom_range %>% pretty(n = 10)
  }
  if (is.null(color_palette)) {
    pal <- colorspace::rainbow_hcl(length(color_list), start = 60)
  } else {
    pal <- colorRampPalette(color_palette)(length(color_list))
  }
  pal <- setNames(pal, color_list)

  # ADD COLORS TO TREEMAP OBJECT
  treemap@cells <- lapply(treemap@cells, function(tm_slot) {
    if (tm_slot$level %in% color_level) {
      if (color_type %in% c("categorical", "both")) {
        tm_slot$color <- pal[[tm_slot$name]]
      } else if (color_type == "cell_size") {
        area <- tm_slot$area/total_area
        tm_slot$color <- pal[[findInterval(area, as.numeric(names(pal)))]]
      } else if (color_type == "custom_color") {
        if (tm_slot$custom_color < as.numeric(names(pal))[[1]]) {
          tm_slot$color <- pal[[1]]
        } else {
          tm_slot$color <- pal[[findInterval(tm_slot$custom_color, as.numeric(names(pal)))]]
        }
      }
    }
    tm_slot
  })

  # SPECIAL CASE "BOTH": DARKEN OR LIGHTEN LOWEST CELL LEVEL
  if (color_type == "both") {
    # get range of cell areas for lowest level
    cell_area <- lapply(treemap@cells, function(tm_slot) {
      if (tm_slot$level == length(treemap@call$levels)) tm_slot$area
    }) %>% unlist
    # based on that, calculate individual lightness adjustment per cell
    treemap@cells <- lapply(treemap@cells, function(tm_slot) {
      # only adjust lightness for cells of lowest level
      if (tm_slot$level == length(treemap@call$levels)) {
        area <- tm_slot$area/total_area
        corr_factor <- scales::rescale(area, from = range(cell_area/total_area), to = c(-0.2, 0.2))
        # if lowest level is also chosen color_level, adjust lightness of cell
        if (tm_slot$level %in% color_level) {
          tm_slot$color <- colorspace::lighten(tm_slot$color, corr_factor)
        # else add a semitransparant color to the cell (parental cell is main color)
        } else {
          tm_slot$color <- grey(0.5+(2*corr_factor), alpha = (1.5*abs(corr_factor)))
        }
      }
      tm_slot
    })
  }

  # return treemap with colors and palette
  treemap@call$palette <- pal
  treemap

}
