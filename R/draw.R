#' drawTreemap
#'
#' Draws the treemap from a list obtained by running [voronoiTreemap()] or
#' [sunburstTreemap()]. Many graphical parameters can be customized but some
#' settings that determine the appearance of treemaps are already made 
#' during treemap generation. Such parameters are primarily cell size and
#' and initial shape of the treemap.
#'
#' @param treemap (list) A list of grid polygons and text objects that is the
#'   output from treemap generation.
#' @param add (logical) Defaults to \code{FALSE}, creating a new page when drawing
#'   a treemap. When multiple treemaps should be plotted on the same page, this should be
#'   set to TRUE, and position of treemaps specified by \code{layout} and \code{position} arguments.
#' @param width (numeric) The width (0 to 0.9) of the viewport that the treemap will occupy.
#' @param height (numeric) The height (0 to 0.9) of the viewport that the treemap will occupy.
#' @param layout (numeric) Vector of length 2 indicationg the number of rows and columns
#'   that the plotting area is supposed to be subdivided in. Useful only together with
#'   \code{position}, which indicates the position of the specific treemap. Use \code{add = TRUE}
#'   to omit starting a new page every time you call \code{drawTreemap()}.
#' @param position (numeric) Vector of length 2 indicationg the position where the current
#'   treemap should be drawn. Useful only together with \code{layout}, which indicates 
#'   the number of rows and columns the plotting area is subdivided into. Use \code{add = TRUE}
#'   to omit starting a new page every time you call \code{drawTreemap()}.
#' @param cell_color (numeric or character) A number representing the hierarchical level
#'   that should be used for cell coloring (categorical), or \code{cell_size} for using relative
#'   area to determine colors, or \code{custom_color} to use a color index supplied during
#'   treemap generation, or \code{NULL} to omit drawing colors.
#' @param color_palette (character) A character vector of colors used to fill cells.
#'   The default is to use \code{\link{rainbow_hcl}} from package \code{colorspace}
#' @param border_size (numeric) The initial line width of the highest level 
#'   cells. Is reduced each level. Default is 6 pts.
#' @param border_color (character) Color for cell borders, default is a light grey.
#' @param labels (numeric) A number representing the hierarchical level that should be
#'   used for drawing cell labels, or NULL to omit drawing labels. The default is the
#'   lowest level (every cell has a label).
#' @param label_size (numeric) Expansion factor for labels. A simple multiplier
#'   that shrinks or expands all labels, regardless of their relative size.
#' @param label_color (character) Color for cell labels.
#' @param title (character) An optional title, default to \code{NULL}.
#' 
#' @return Creates a grid viewport and draws the treemap.
#' 
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap (\code{list}) that is
#'   the input for the drawing function
#'
#' @examples
#' library(SysbioTreemaps)
#' 
#' #generate data frame
#' df <- data.frame(
#'   A = rep(c("a", "b", "c"), each=15),
#'   B = sample(letters[4:12], 45, replace = TRUE),
#'   C = sample(10:100, 45)
#' )
#' 
#' # generate treemap
#' tm <- voronoiTreemap(
#'   data = df,
#'   levels = c("A", "B", "C"),
#'   cell_size = "C",
#'   shape = "rounded_rect"
#' )
#' 
#' # draw treemap
#' drawTreemap(tm)
#' 
#' # draw different variants of the same treemap on one page using
#' # the 'layout' and 'position' arguments (indicating rows and columns)
#' drawTreemap(tm, 
#'   cell_color = 1, title = "treemap 1", 
#'   layout = c(1,3), position = c(1, 1))
#' 
#' drawTreemap(tm, title = "treemap 2", 
#'   cell_color = 2, border_size = 6, 
#'   add = TRUE, layout = c(1,3), position = c(1, 2))
#' 
#' drawTreemap(tm, title = "treemap 3",
#'   cell_color = 3, border_color = grey(0.4), 
#'   label_color = grey(0.4),
#'   color_palette = heat.colors(10),
#'   add = TRUE, layout = c(1,3), position = c(1, 3))
#' 
#' @import tidyr
#' @importFrom grid grid.newpage
#' @importFrom grid grid.text
#' @importFrom grid grid.polygon
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid popViewport
#' @importFrom colorspace rainbow_hcl
#' @importFrom scales rescale
#' 
#' @export drawTreemap
# 
# 
# DRAWING FUNCTION
# this function draws the polygons as polygonGrob objects
# from grid package
drawTreemap <- function(
  treemap, 
  add = FALSE,
  width = 0.9,
  height = 0.9,
  layout = c(1, 1),
  position = c(1, 1),
  cell_color = 1,
  color_palette = NULL,
  border_size = 6,
  border_color = grey(0.9),
  labels = lapply(treemap, function(x) x$level) %>% unlist %>% max,
  label_size = 1,
  label_color = grey(0.9),
  title = NULL)
{
  
  # ERROR HANDLING
  #
  # check layout options
  if (width > 0.9 | height > 0.9) {
    stop("'width' or 'height' should not exceed 0.9.")
  }
  if ({c(layout, position) > 1} %>% any & !add) {
    warning("Use 'add = TRUE' if you want to add more treemaps to this page.", immediate. = TRUE)
  }
  
  # check graphical parameter input
  if (!is.null(color_palette)) {
    if (!is.character(color_palette)) {
      stop("'color_palette' must be a character vector with RGB colors, like '#FFFFFF'")
    }
  }
  
  if (!is.null(border_color)) {
    if (!is.character(border_color)) {
      stop("'border_color' must be a character with RGB color code, like '#FFFFFF'")
    }
  }
  
  if (!is.null(label_color)) {
    if (!is.character(label_color)) {
      stop("'border_color' must be a character with RGB color code, like '#FFFFFF'")
    }
  }
  
  if (!is.null(title)) {
    if (!is.character(title)) {
      stop("'title' must be a character of length 1")
    }
  }
  
  # check labels
  if (is.null(labels)) {
     warning("Drawing of labels disabled.", immediate. = TRUE)
  } else if (!is.numeric(labels)) {
    stop("'labels' must be numeric vector indicating the level(s) for which labels should be drawn.")
  } else if (is.numeric(labels)) {
    all_levels <- lapply(treemap, function(x) x$level) %>% unlist %>% unique
    if (!all(labels %in% all_levels)) {
      stop("'labels' indicated levels that are not contained in this treemap")
    }
  }
  
  # new grid page is the default
  if (!add) {
    grid::grid.newpage()
  }
  
  
  # generate main grid viewport
  # optionally subdividing the plot area by layout argument
  grid::pushViewport(
    grid::viewport(
      layout = grid::grid.layout(
        nrow = layout[1], 
        ncol = layout[2]
      )
    )
  )
  
  # generate child viewport for drawing one treemap
  grid::pushViewport(
    grid::viewport(
      layout.pos.row = position[1],
      layout.pos.col = position[2]
    )
  )
  
  # generate viewport to draw treemap in, with some user-specified margins
  grid::pushViewport(
    grid::viewport(
      y = 0.475, 
      xscale = c(0,2000),
      yscale = c(0,2000),
      width = width,
      height = height
    )
  )
  
  # define color palette with 100 steps
  if (is.null(color_palette)) {
    pal <- colorspace::rainbow_hcl(100, start = 60)
  } else {
    pal <- colorRampPalette(color_palette)(100)
  }
  
    
  # DRAWING POLYGONS
  # the treemap object is a nested list with two
  # levels; in order to draw the map we can use apply functions
  # There are different possible cases to determine the cell color
  # depending on user specified methods
  
  # CASE 1: number representing the hierarchical level (categorical)
  if (is.numeric(cell_color)) {
    
    # generate a color code depending on number of categories per level
    color_list <- lapply(treemap, function(tm_slot) {
      if (tm_slot$level == cell_color) {
        tm_slot$names
      }
    }) %>% 
      unlist %>% 
      unique %>%
      setNames(convertInput(.), .)
    
    # go through list of treemap polygon and draw only the ones
    # supposed to be colored
    lapply(treemap, function(tm_slot) {
      if (tm_slot$level == cell_color) {
        fill = pal[color_list[tm_slot$names]]
        mapply(drawPoly, tm_slot$k, tm_slot$names, fill = fill,
          SIMPLIFY=FALSE, MoreArgs = list(lwd = NA, col = NA)
        )
      }
    }) %>% invisible
  }
  
  # CASE 2: 'cell_size' for using target area to determine colors
  if (cell_color == "cell_size") {
    
    range_area <- lapply(treemap, function(tm_slot) {
      if (tm_slot$level == lapply(treemap, function(x) x$level) %>% unlist %>% max)
        tm_slot$a 
      }) %>% unlist %>% range
    
    lapply(treemap, function(tm_slot) {
      # check that we are on the lowest level
      if (tm_slot$level == lapply(treemap, function(x) x$level) %>% unlist %>% max) {
        fill = tm_slot$a %>%
          scales::rescale(
            from = c(range_area[1], range_area[2]), 
            to = c(1, 100)
          ) %>% pal[.]
        mapply(drawPoly, tm_slot$k, tm_slot$names, fill = fill,
          SIMPLIFY=FALSE, MoreArgs = list(lwd = NA, col = NA)
        )
      }
    }) %>% invisible
  }
  
  # CASE 3: 'custom_color' to use a color index supplied during treemap generation
  if (cell_color == "custom_color") {
    
    lapply(treemap, function(tm_slot) {
      # check that we are on the lowest level
      if (tm_slot$level == lapply(treemap, function(x) x$level) %>% unlist %>% max) {
        stopifnot(is.numeric(tm_slot$custom_color))
        fill = pal[tm_slot$custom_color]
        mapply(drawPoly, tm_slot$k, tm_slot$names, fill = fill,
          SIMPLIFY=FALSE, MoreArgs = list(lwd = NA, col = NA)
        )
      }
    }) %>% invisible
  }
  
  
  # DRAWING BORDERS
  if (!is.null(border_color) & !is.null(border_size)) {
    lapply(treemap, function(tm_slot) {
      mapply(drawPoly, tm_slot$k, tm_slot$names, fill = NA,
        SIMPLIFY=FALSE,
        MoreArgs = list(
          lwd = border_size / tm_slot$level, 
          col = border_color
        )
      )
    }) %>% invisible
  }
  
  
  # DRAWING LABELS
  if (!is.null(labels) & !is.null(label_size) & !is.null(label_color)) {
    
    lapply(treemap, function(tm_slot) {
    
      if (tm_slot$level %in% labels) {
        # function to determine label sizes for each individual cell
        # based on cell dimension and label character length
        cex = sqrt(unlist(tm_slot$a)) * label_size / (100 * nchar(tm_slot$names)) %>%
          round(1)
        
        grid::grid.text(
          tm_slot$names,
          tm_slot$s$x,
          tm_slot$s$y,
          default = "native",
          gp = gpar(cex = cex, col = label_color)
        )
      }
    }) %>% invisible
  }
  
    
  # DRAW TITLE AND LEGEND
  if (!is.null(title)) {
    
    # pop viewport back to parent
    grid::popViewport()
    
    # generate viewport for optional title and legend
    grid::pushViewport(
      grid::viewport(y = 0.95, height = 0.1)
    )
    grid::grid.text(title, y = 0.75, gp = grid::gpar(cex=0.8))
    
  }
  
  # Finally pop the viewport back to the parent viewport
  # in order to allow adding more plots
  popViewport(3)
  
}

