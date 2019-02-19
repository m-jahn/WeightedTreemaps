#' drawTreemap
#'
#' Draws the treemap from a list obtained by running [voronoiTreemap()] or
#' [sunburstTreemap()]. Some graphical parameters can be tweaked but most
#' settings are made during treemap generation.
#'
#' @param treemap (list) A list of grid polygons and text objects that is the
#'   output from treemap generation.
#' @param width (numeric) The width (0 to 1) of the viewport that the treemap will occupy.
#' @param height (numeric) The height (0 to 1)of the viewport that the treemap will occupy.
#' @param title (character) A title, optional.
#' 
#' @return Creates a grid viewport and draws the treemap.
#' 
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap (\code{list}) that is
#'   the input for the drawing function
#'
#' @examples
#' library(SysbioTreemaps)
#' library(dplyr)
#' 
#' # generate example data
#' data <- tibble(
#'   A=rep(c("a", "b", "c"), each=15),
#'   B=sample(letters[4:13], 45, replace=TRUE),
#'   C=sample(1:100, 45)
#' ) %>% arrange(A, B, C)
#' 
#' # generate treemap
#' tm <- voronoiTreemap(
#'   data = data,
#'   levels = c("A", "B", "C"),
#'   cell.size = "C",
#'   cell.color = "A",
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
#' 
#' @export drawTreemap
drawTreemap <- function(
  treemap, 
  width=0.9, 
  height=0.9,
  title=NULL)
{
  
  # new grid page
  grid::grid.newpage()
  
  # draw some additional elements, like a title
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
    xscale = c(0,1000),
    yscale = c(0,1000)
  ))
  
  # the treemap object is a nested list with two
  # levels, in order to draw we just wrap two lapply functions
  lapply(tm, function(a) {
    lapply(a, function(b) {
      grid::grid.draw(b)
    })
  }) %>% invisible
  
}

