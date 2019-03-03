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
#' @param width (numeric) The width (0 to 1) of the viewport that the treemap will occupy.
#' @param height (numeric) The height (0 to 1)of the viewport that the treemap will occupy.
#' @param cell_color (numeric or character) A number representing the hierarchical level
#'   that should be used for cell coloring (categorical), or 'cell_size' for using relative
#'   area to determine colors, or 'custom_color' to use a color index supplied during
#'   treemap generation, or \code{NULL} to omit drawing colors.
#' @param color_palette (character) A character vector of colors used to fill cells.
#'   The default is to use \code{\link{rainbow_hcl}} from package\code{colorspace}
#' @param border_size (numeric) The initial line width of the highest level 
#'   cells. Is reduced each level. Default is 6 pts.
#' @param border_color (character) Color for cell borders, default is a light grey.
#' @param labels (numeric) A number representing the hierarchical level that should be
#'   used for cell labels, or NULL to omit drawing labels. The default is the
#'   lowest level (every cell has a label).
#' @param label_size (numeric) Expansion factor for labels. A simple multiplier
#'   that shrinks or expands all labels, regardless of their relative size.
#' @param label_color (character) Color for cell labels.
#' @param title (character) A title, optional.
#' 
#' @return Creates a grid viewport and draws the treemap.
#' 
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap (\code{list}) that is
#'   the input for the drawing function
#'
#' @examples
#' library(SysbioTreemaps)
#' 
#' # generate example data
#' df <- data.frame(
#'   A=rep(c("a", "b", "c"), each=15),
#'   B=sample(letters[4:13], 45, replace=TRUE),
#'   C=sample(1:100, 45)
#' )
#' 
#' # generate treemap
#' tm <- voronoiTreemap(
#'   data = df,
#'   levels = c("A", "B", "C"),
#'   cell_size = "C",
#'   cell_color = "A",
#'   maxIteration = 50,
#'   )
#' 
#' # plot treemap
#' drawTreemap(tm)
#' 
#' @import tidyr
#' @importFrom grid grid.newpage
#' @importFrom grid grid.text
#' @importFrom grid grid.draw
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom grid pushViewport
#' @importFrom grid viewport
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
  width = 0.9, 
  height = 0.9,
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
  # check labels
  # if (is.null(labels)) {
  #   warning("Drawing of labels disabled.", immediate. = TRUE)
  # } else if (!all(labels %in% levels)) {
  #   warning("'labels' is not a colname of 'data'. Drawing of labels disabled.", immediate. = TRUE)
  #   labels = NULL
  # }
  
  
  # new grid page
  grid::grid.newpage()
  
  # optional drawing of title
  if (!is.null(title)) {
    grid::grid.text(
      title, 
      x = grid::unit(0.5, "npc"), 
      y = grid::unit(0.98, "npc"),
      gp=grid::gpar(cex=0.8)
    )
  }
  
  # generate the grid viewport for the treemap
  grid::pushViewport(grid::viewport(
    width = width,
    height = height,
    xscale = c(0,2000),
    yscale = c(0,2000)
  ))
  
  
  # define color palette with 100 steps
  if (is.null(color_palette)) {
    pal <- colorspace::rainbow_hcl(100, start = 60)
  } else {
    pal <- colorRampPalette(color_palette)(100)
  }
  
  # generate a color code depending on number of categories per level
  if (is.numeric(cell_color)) {
    l <- lapply(treemap, function(tm_slot) {
      if (tm_slot$level == cell_color) {
        tm_slot$names
      }
    }) %>% unlist %>% unique
    color_list <- 1:length(l) %>% 
      scales::rescale(to = c(1, 100)) %>%
      round
    names(color_list) <- l
    print(color_list)
    print(l)
  }
  
    
  # DRAWING POLYGONS
  # the treemap object is a nested list with two
  # levels; in order to draw the map we can use apply functions
  lapply(treemap, function(tm_slot) {
    
    # determine the cell colors depending on the specified level
    # case 1: number representing the hierarchical level (categorical)
    if (is.numeric(cell_color)) {
      if (tm_slot$level == cell_color) {
        fill = pal[color_list[tm_slot$names]]
      } else {
        fill = NA
      }
    }
    # case 2: 'cell_size' for using relative area to determine colors
    
    
    # case 3: 'custom_color' to use a color index supplied during treemap generation
    if (cell_color == "custom_color") {
      fill = pal[tm_slot$custom_color]
    }
    # case 4: NULL to omit drawing colors
    if (is.null(cell_color)) fill = NA
    
    # call drawPoly with all graphical parameters for each polygon
    # and draw the grid objects
    mapply(drawPoly, tm_slot$k, tm_slot$names, fill = fill,
      SIMPLIFY=FALSE,
      # arguments that are not vectorized
      MoreArgs = list(lwd = NA, col = NA)
    ) %>% lapply(grid.draw)
  }) %>% invisible
  
  
  # DRAWING BORDERS
  lapply(treemap, function(tm_slot) {
    mapply(drawPoly, tm_slot$k, tm_slot$names, fill = NA,
      SIMPLIFY=FALSE,
      # arguments that are not vectorized
      MoreArgs = list(
        lwd = border_size / tm_slot$level, 
        col = border_color
      )
    ) %>% lapply(grid.draw)
  }) %>% invisible
  
  
  # DRAWING LABELS
  if (!is.null(labels)) {
    
    lapply(treemap, function(tm_slot) {
    
      if (tm_slot$level %in% labels) {
        
        # function to determine label sizes for each individual cell
        # based on cell dimension and label character length
        cex = sqrt(unlist(tm_slot$a)) * label_size / (100 * nchar(tm_slot$names)) %>%
          round(1)
        
        textGrob(
          tm_slot$names,
          tm_slot$s$x,
          tm_slot$s$y,
          default = "native",
          gp = gpar(cex = cex, col = label_color)
        ) %>% grid.draw
        
      }
    }) %>% invisible
  }
}

