#' @importFrom dplyr mutate_if
#' @importFrom dplyr %>%
#
# ERROR HANDLING AND DATA VALIDATION
# ---------------------------------

validate_input <- function(
  data,
  levels,
  fun,
  sort,
  filter,
  cell_size,
  custom_color) {
  
  # check input data frame
  if (!is.data.frame(data)) {
    if (!is.matrix(data))
      stop("'data' must be a matrix or a data frame.")
    else if (is.null(colnames(data)))
      stop("'data' must have column names.")
    else
      data <- as.data.frame(data)
  }
  
  # NAs are not allowed in any input column
  if (is.na(data[levels]) %>% any) {
    stop("NAs are not allowed in level columns.")
  }
  
  # check variable controlling cell size
  if (is.null(cell_size)) {
    cat("No cell size column supplied, cell size encoded by number of members.\n")
  } else {
    
    if (!(cell_size %in% colnames(data))) {
      stop("'cell_size' is not a colname of 'data'.")
    }
    if (is.na(data[cell_size]) %>% any) {
      stop("'cell_size' contains NAs which is not allowed.")
    }
    if ((data[cell_size] <= 0) %>% any) {
      stop("'cell_size' contains negative values or zero, only positive target areas allowed.")
    }
    
    # apply a threshold to filter out small target areas
    if (!is.null(filter)) {
      
      filtered <- data[[cell_size]] %>% {. / sum(.)} > filter
      
      if (sum(!filtered) > 0) {
        data <- subset(data, filtered)
        cat(sum(!filtered), "out of", length(filtered), 
            "cells were filtered due to target area falling below treshold.\n")
      }
    }
  }
  
  # check levels/hierarchies and level options
  if (!all(levels %in% colnames(data))) {
    stop("Not all given levels are column names of 'data'.")
  }
  
  # check that no level columns are factors and coerce 
  # to character if necessary
  if (lapply(data, is.factor) %>% unlist %>% any) {
    data <- data %>% dplyr::mutate_if(is.factor, as.character)
  }
  
  if (!is.null(custom_color)) {
    if(!(custom_color %in% colnames(data)))
      stop("'custom_color' is not a colname of 'data'.")
  }
  
  if (!is.function(fun)) {
    stop("'fun' must be a function.")
  }
  # sort data in case it is unsorted
  if (sort) {
    data <- data[do.call("order", data[levels]), ]
    # coerce back to data.frame if sorting has changed class
    if (is.vector(data)) data <- data.frame(data) %>% setNames(levels)
  } else {
    warning("Sorting is FALSE, it is expected that the input data is sorted.", immediate. = TRUE)
  }
  
  return(data)
}


