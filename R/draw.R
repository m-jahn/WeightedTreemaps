#' drawTreemap
#'
#' Draws the treemap from a list obtained by running \code{\link{voronoiTreemap}} or
#' \code{\link{sunburstTreemap}}. Many graphical parameters can be customized but some
#' settings that determine the appearance of treemaps are already made 
#' during treemap generation. Such parameters are primarily cell size and
#' and initial shape of the treemap.
#'
#' @param treemap (list) A list of grid polygons and text objects that is the
#'   output from treemap generation.
#' @param levels (numeric) A numeric vector representing the hierarchical levels 
#'   that are drawn. The default is to draw all levels.
#' @param color_type (character) One of "categorical", "cell_size" or "custom_color".
#'   For "categorical", each cell is colored with a unique color from the current palette,
#'   but colors will repeat if there are many cells. For "cell_size", the cells
#'   are colored according to their relative area. For "custom_color", a color 
#'   index is used that was specified by \code{custom_color} during
#'   treemap generation. Use \code{NULL} to omit drawing colors. 
#' @param color_level (numeric) A numeric of length 1 representing the hierarchical level 
#'   that should be used for cell coloring. Must be one of \code{levels}.
#' @param color_palette (character) A character vector of colors used to fill cells.
#'   The default is to use \code{\link{rainbow_hcl}} from package \code{colorspace}
#' @param border_level (numeric) A numeric of length \code{levels} representing the 
#'   hierarchical level where border should be drawn. Default is to draw all borders.
#' @param border_size (numeric) The initial line width of the highest level 
#'   cells. Is reduced each level. Default is 6 pts.
#' @param border_color (character) Color for cell borders, default is a light grey.
#' @param label_level (numeric) A numeric vector representing the hierarchical level that should be
#'   used for drawing cell labels, or NULL to omit drawing labels. The default is the
#'   deepest level (every cell has a label).
#' @param label_size (numeric) Expansion factor for labels. A simple multiplier
#'   that shrinks or expands all labels, regardless of their relative size.
#' @param label_color (character) Color for cell labels.
#' @param title (character) An optional title, default to \code{NULL}.
#' @param title_size (numeric) The size (or 'character expansion') of the title.
#' @param title_color (character) Color for title.
#' @param legend (logical) Set to TRUE if a color key should be drawn. Default is FALSE.
#' @param width (numeric) The width (0 to 0.9) of the viewport that the treemap will occupy.
#' @param height (numeric) The height (0 to 0.9) of the viewport that the treemap will occupy.
#' @param layout (numeric) Vector of length 2 indicating the number of rows and columns
#'   that the plotting area is supposed to be subdivided in. Useful only together with
#'   \code{position}, which indicates the position of the specific treemap. Use \code{add = TRUE}
#'   to omit starting a new page every time you call \code{drawTreemap()}.
#' @param position (numeric) Vector of length 2 indicationg the position where the current
#'   treemap should be drawn. Useful only together with \code{layout}, which indicates 
#'   the number of rows and columns the plotting area is subdivided into. Use \code{add = TRUE}
#'   to omit starting a new page every time you call \code{drawTreemap()}.
#' @param add (logical) Defaults to \code{FALSE}, creating a new page when drawing
#'   a treemap. When multiple treemaps should be plotted on the same page, this should be
#'   set to TRUE, and position of treemaps specified by \code{layout} and \code{position} arguments.
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
#' drawTreemap(tm, title = "treemap 1", 
#'   color_type = "categorical", color_level = 1, 
#'   layout = c(1,3), position = c(1, 1))
#' 
#' drawTreemap(tm, title = "treemap 2",
#'   color_type = "categorical", color_level = 2, border_size = 3,
#'   add = TRUE, layout = c(1,3), position = c(1, 2))
#' 
#' drawTreemap(tm, title = "treemap 3",
#'   color_type = "cell_size", color_level = 3,
#'   color_palette = heat.colors(10),
#'   border_color = grey(0.4), label_color = grey(0.4),
#'   add = TRUE, layout = c(1,3), position = c(1, 3),
#'   title_color = "black")
#' 
#' @import tidyr
#' @importFrom grid grid.newpage
#' @importFrom grid grid.text
#' @importFrom grid grid.polygon
#' @importFrom grid grid.draw
#' @importFrom grid grid.layout
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid popViewport
#' @importFrom colorspace rainbow_hcl
#' @importFrom scales rescale
#' @importFrom lattice draw.colorkey
#' 
#' @export drawTreemap
# 
# 
# DRAWING FUNCTION
# this function draws the polygons as polygonGrob objects
# from grid package
drawTreemap <- function(
  treemap, 
  levels = lapply(treemap, function(x) x$level) %>% unlist %>% unique, 
  color_type = "categorical",
  color_level = min(levels),
  color_palette = NULL,
  border_level = levels,
  border_size = 6,
  border_color = grey(0.9),
  label_level = max(levels),
  label_size = 1,
  label_color = grey(0.9),
  title = NULL,
  title_size = 1,
  title_color = grey(0.5),
  legend = FALSE,
  width = 0.9,
  height = 0.9,
  layout = c(1, 1),
  position = c(1, 1),
  add = FALSE)
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
  
  # check level input
  if (!all(levels %in% (lapply(treemap, function(x) x$level) %>% unlist %>% unique))) {
    stop("Not all indicated 'levels' are contained in this treemap.")
  }
  
  if (!is.null(color_level)) {
    if (!all(color_level %in% levels)) {
      stop("The values in 'color_level' must be contained in 'levels'")
    }
  }
  if (!is.null(border_level)) {
    if (!all(border_level %in% levels)) {
      stop("The values in 'border_level' must be contained in 'levels'")
    }
  }
  if (!is.null(label_level)) {
    if (!all(label_level %in% levels)) {
      stop("The values in 'label_level' must be contained in 'levels'")
    }
  }
  
  # check graphical parameter input
  if (!is.null(color_palette)) {
    if (!is.character(color_palette)) {
      stop("'color_palette' must be a character vector that can be interpreted as colors")
    }
  }
  
  if (!is.null(border_color)) {
    if (!is.character(border_color)) {
      stop("'border_color' must be a character that can be interpreted as colors")
    }
  }
  
  if (!is.null(label_color)) {
    if (!is.character(label_color)) {
      stop("'border_color' must be a character that can be interpreted as colors")
    }
  }
  
  if (!is.null(title)) {
    if (!is.character(title)) {
      stop("'title' must be a character of length 1")
    }
  }
  
  # check labels
  if (is.null(label_level)) {
     warning("Drawing of labels disabled.", immediate. = TRUE)
  } else if (!is.numeric(label_level)) {
    stop("'label_level' must be numeric vector indicating the level(s) for which labels should be drawn.")
  } else if (is.numeric(label_level)) {
    if (!all(label_level %in% levels)) {
      stop("'label_level' indicated levels that are not contained in this treemap")
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
  
  # generate child viewport for drawing one treemap 'object' 
  # including title and legend
  grid::pushViewport(
    grid::viewport(
      layout.pos.row = position[1],
      layout.pos.col = position[2]
    )
  )
  
  # generate viewport to draw treemap in, with some user-specified margins
  # plus optional margins if legend or title is drawn
  grid::pushViewport(
    grid::viewport(
      x = {if (legend) 0.45 else 0.5},
      y = {if (!is.null(title)) 0.475 else 0.5},
      xscale = c(0, 2000),
      yscale = c(0, 2000),
      width = {if (legend) width - 0.1 else width},
      height = {if (!is.null(title)) height - 0.05 else height}
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
  
  # CASE 1: coloring by hierarchical level (categorical)
  if (color_type == "categorical") {
    
    # generate a color code depending on number of categories for selected level
    # used for drawing legend
    color_list <- lapply(treemap, function(tm_slot) {
      if (tm_slot$level == color_level) {
        tm_slot$names
      }
    }) %>% 
      unlist %>% unique %>% sort %>%
      setNames(convertInput(.), .)
    
    # go through list of treemap polygon and draw only the ones
    # supposed to be colored (= the correct color_level)
    lapply(treemap, function(tm_slot) {
      if (tm_slot$level == color_level) {
        fill = pal[color_list[tm_slot$names]]
        mapply(drawPoly, tm_slot$k, tm_slot$names, fill = fill,
          SIMPLIFY=FALSE, MoreArgs = list(lwd = NA, col = NA)
        )
      }
    }) %>% invisible
    
  }
  
  # CASE 2: coloring by cell size/area
  if (color_type == "cell_size") {
    
    range_area <- lapply(treemap, function(tm_slot) {
      if (tm_slot$level == color_level) tm_slot$a 
      }) %>% unlist %>% range
    
    total_area <- lapply(treemap, function(tm_slot) {
      if (tm_slot$level == color_level) tm_slot$a 
      }) %>% unlist %>% sum
    
    # used for drawing legend
    color_list <- seq(range_area[1], range_area[2], length.out = 7) %>%
      {. / total_area * 100} %>% round(1) %>%
      setNames(scales::rescale(., to = c(1, 100)), . )
    
    # draw only polygons for the correct level
    lapply(treemap, function(tm_slot) {
      if (tm_slot$level == color_level) {
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
  if (color_type == "custom_color") {
    
    # used for drawing legend
    color_list <- c(1, 20, 40, 60, 80, 100) %>% setNames(., .)
    
    # draw only polygons for the correct level
    lapply(treemap, function(tm_slot) {
      if (tm_slot$level == color_level) {
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
    
    # draw only borders for the correct level
    lapply(treemap, function(tm_slot) {
      if (tm_slot$level %in% border_level) {
        mapply(drawPoly, tm_slot$k, tm_slot$names, fill = NA,
          SIMPLIFY=FALSE,
          MoreArgs = list(
            lwd = border_size / tm_slot$level, 
            col = border_color
          )
        )
      }
    }) %>% invisible
    
  }
  
  
  # DRAWING LABELS
  if (!is.null(label_level) & !is.null(label_size) & !is.null(label_color)) {
    
    lapply(treemap, function(tm_slot) {
    
      if (tm_slot$level %in% label_level) {
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
  
    
  # DRAW OPTIONAL TITLE
  if (!is.null(title)) {
    
    # pop viewport back to parent
    grid::popViewport()
    
    # generate viewport for title
    grid::pushViewport(
      grid::viewport(y = 0.95, height = 0.1)
    )
    grid::grid.text(
      title, 
      y = 0.5, 
      gp = grid::gpar(cex = title_size, col = title_color)
    )
    
  }
  
  # DRAW OPTIONAL LEGEND
  if (legend) {
    
    # pop viewport back to parent
    grid::popViewport()
    
    # generate viewport for legend; viewport for legend is also scaled
    # depending on title and height arguments
    grid::pushViewport(
      grid::viewport(
        x = 0.95,
        y = {if (!is.null(title)) 0.475 else 0.5},
        width = 0.1,
        height = {if (!is.null(title)) height - 0.05 else height}
      )
    )
    
    # make legend which is a list and 
    colorkey <- list(
      space = "right",
      col = pal[color_list],
      at = 1:(length(color_list)+1),
      labels = c("", names(color_list) %>% substr(1, 3)),
      width = 0.8,
      axis.line = list(alpha = 1, col = border_color, lwd = 1, lty = 1),
      axis.text = list(alpha = 1, cex = 0.8, col = title_color, font = 1, lineheight = 1)
    )
    
    # draw using the drawKey function from lattice::levelplot
    grid.draw(
      lattice::draw.colorkey(key = colorkey)
    )
    
  }
  
  # Finally pop the viewport back to the parent viewport
  # in order to allow adding more plots
  popViewport(3)
  
}

