#' drawTreemap
#'
#' Draws the treemap from a list obtained by running [voronoiTreemap()]
#'
#' @param data (data.frame) A data.frame with one column for each hierarchical level
#' 
#' @return Creates a grid viewport and draws the treemap.
#' 
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap (\code{list}) that is
#'   the input for the drawing function
#'
#' @examples
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
  main=NULL)
{
  
  # new grid page
  grid::grid.newpage()
  
  # draw some additional elements, like a title
  if (!is.null(main)) {
    grid::grid.text(
      main, 
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

