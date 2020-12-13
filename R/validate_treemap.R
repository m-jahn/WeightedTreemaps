#' @importFrom dplyr %>%
#
# check input before treemap drawing
validate_treemap <- function(treemap, 
  width, height, layout, position, add,
  levels, color_level, border_level, 
  label_level, color_palette,
  border_color, label_color, 
  custom_range, title) {
  
  # check treemap object
  stopifnot(class(treemap) %in% c("sunburstResult", "voronoiResult"))
  
  # check layout options
  if (width > 0.9 || height > 0.9) {
    stop("'width' or 'height' should not exceed 0.9.")
  }
  if ({c(layout, position) > 1} %>% any & !add) {
    cat("Note: use 'add = TRUE' if you want to add more treemaps to this page.")
  }
  
  # check level input
  if (!all(levels %in% 1:length(treemap@call$levels))) {
    stop("Not all indicated 'levels' are contained in this treemap.")
  }
  
  # determine color levels to draw
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
      stop("'color_palette' must be a character that can be interpreted as colors")
    }
  }
  
  if (!is.null(border_color)) {
    if (!is.character(border_color)) {
      stop("'border_color' must be a character that can be interpreted as colors")
    } else {
      if (!length(border_color) %in% c(1, length(border_level)))
        stop("'border_color' must be length = 1 or length(border_level)")
    }
  }
  
  if (!is.null(label_color)) {
    if (!is.character(label_color)) {
      stop("'label_color' must be a character that can be interpreted as colors")
    } else {
      if (!length(label_color) %in% c(1, length(label_level)))
        stop("'label_color' must be length=1 or length(label_level)")
    }
  }
  
  if (!is.null(title)) {
    if (!is.character(title)) {
      stop("'title' must be a character of length 1")
    }
  }
  
  if (!is.null(custom_range)) {
    if (!(is.numeric(custom_range) & length(custom_range) == 2))
      stop("'custom_range' is not a numeric of length 2.")
  }
  
  # check labels
  if (!is.null(label_level)) {
    if (!is.numeric(label_level)) {
      stop("'label_level' must be numeric vector indicating the level(s) for which labels should be drawn.")
    } else if (is.numeric(label_level)) {
      if (!all(label_level %in% levels)) {
        stop("levels in 'label_level' must be contained in this treemap")
      }
    }
  }
  
}