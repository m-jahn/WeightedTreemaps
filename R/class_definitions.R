#
# S4 class definitions for sunburst and voronoi treemaps
# ------------------------------------------------------
sunburstResult <- setClass("sunburstResult",
  contains = "list",
  slots = c(
    cells = "list",
    data = "data.frame",
    call = "list"
  )
)

voronoiResult <- setClass("voronoiResult",
  contains = "list",
  slots = c(
   cells = "list",
   data = "data.frame",
   call = "list"
  )
)

# generic methods for these classes
# ------------------------------------------------------
#
#' as.data.frame.voronoiResult
#'
#' Coerces a voronoiResult object to data frame, omitting polygon data.
#'
#' @param x (voronoiResult) A voronoi treemap results object
#' @param ... (none) Not used
#' @param stringsAsFactors (logical) Transform strings to factors, default FALSE
#' @return Returns a data.frame
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap that is
#'   the input for this function
#' @export
as.data.frame.voronoiResult <- function(x, ..., stringsAsFactors = FALSE) {
  list_cells <- lapply(x@cells, function(y) {
    y <- y[c("level", "name", "target", "weight", "area", "count", "custom_color")]
    as.data.frame(y, stringsAsFactors = stringsAsFactors)
  })
  do.call(rbind, list_cells)
}

#' as.data.frame.sunburstResult
#'
#' Coerces a sunburstResult object to data frame.
#'
#' @param x (sunburstResult) A sunburst treemap results object
#' @param ... (none) Not used
#' @param stringsAsFactors (logical) Transform strings to factors, default FALSE
#' @return Returns a data.frame
#' @seealso \code{\link{sunburstTreemap}} for generating the treemap that is
#'   the input for this function
#' @export
as.data.frame.sunburstResult <- function(x, ..., stringsAsFactors = FALSE) {
  list_cells <- lapply(x@cells, function(y) {
    y <- y[c("level", "name", "area", "lower_bound", "upper_bound", "custom_color")]
    as.data.frame(y, stringsAsFactors = stringsAsFactors)
  })
  do.call(rbind, list_cells)
}

#' summary.voronoiResult
#'
#' Summary method for voronoiResult.
#'
#' @param object (voronoiResult) A voronoi treemap results object
#' @param ... (none) Not used
#' @return Returns a data.frame
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap that is
#'   the input for this function
#' @importFrom utils head
#' @export
summary.voronoiResult <- function(object, ...) {
  df <- as.data.frame(object)
  message(paste0(
    "A treemap object of class 'voronoiResult' with ",
    nrow(df), " cells on ",
    length(unique(df$level)), " levels.\n",
    "The first six cells are:"
  ))
  head(df)
}

#' summary.sunburstResult
#'
#' Summary method for sunburstResult
#'
#' @param object (sunburstResult) A sunburst treemap results object
#' @param ... (none) Not used
#' @return Returns a data.frame
#' @seealso \code{\link{sunburstTreemap}} for generating the treemap that is
#'   the input for this function
#' @export
summary.sunburstResult <- function(object, ...) {
  df <- as.data.frame(object)
  message(paste0(
    "A treemap object of class 'sunburstResult' with ",
    nrow(df), " cells on ",
    length(unique(df$level)), " levels.\n",
    "The first six cells are:"
  ))
  head(df)
}

#' print.voronoiResult
#'
#' Print method for voronoiResult
#'
#' @param x (voronoiResult) A voronoi treemap results object
#' @param ... (none) Not used
#' @return Returns a data.frame
#' @seealso \code{\link{voronoiTreemap}} for generating the treemap that is
#'   the input for this function
#' @export
print.voronoiResult <- function(x, ...) {
  summary(x)
}

#' print.sunburstResult
#'
#' Print method for sunburstResult
#'
#' @param x (sunburstResult) A sunburst treemap results object
#' @param ... (none) Not used
#' @return Returns a data.frame
#' @seealso \code{\link{sunburstTreemap}} for generating the treemap that is
#'   the input for this function
#' @export
print.sunburstResult <- function(x, ...) {
  summary(x)
}
