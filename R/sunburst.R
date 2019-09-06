#' sunburstTreemap
#'
#' Create sunburst treemaps where variables are encoded by size of circular sectors.
#'
#' This function returns a treemap object instead of a plot. In order 
#' to actually draw the treemap, use \code{\link{drawTreemap}}.
#'
#'
#' @param data (data.frame) A data.frame with one column for each hierarchical level
#' @param levels (character) Character vector indicating the column names to 
#'   be used. The order of names must correspond to the hierarchical levels, 
#'   going from broad to fine 
#' @param fun (function) Function to be used to aggregate cell sizes of parental cells
#' @param sort (logical) Should the columns of the data.frame be sorted before?
#' @param filter (logical) Filter the supplied data frame to remove very small
#'   sectors that may not be visible. The default is to not remove any sectors.
#' @param cell_size (character) The name of the column used to control sector size. 
#'   Can be one of \code{levels} or any other column with numerical data. NA or
#'   values equal or less than zero are not allowed.
#'   The values in this column are aggregated by the function specified by \code{fun}.
#'   If \code{sector_size = NULL}, sector size is simply computed by the number of members
#'   for the respective sector (corresponding to rows in the data.frame).
#' @param custom_color (character) An optional column that can be specified to
#'   control cell color. Cell colors are determined when drawing the treemap
#'   using \code{\link{drawTreemap}}, but the default is to use one of 
#'   \code{levels} or \code{cell size}. Any other data source that shall be used
#'   instead has to be included in the treemap generation and explicitly 
#'   specified here. The default value is \code{NULL}.
#' @param diameter_inner (numeric) The minimum inner diameter of the drawn map. 
#'   Defaults to 0.3,
#' @param diameter_outer (numeric) The maximum outer diameter of the drawn map. 
#'   Defaults to 0.8
#' 
#' @return A named list with treemap objects containing polygons and metadata 
#' 
#' @seealso \code{\link{drawTreemap}} for drawing the treemap.
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
#' tm <- sunburstTreemap(
#'   data = df,
#'   levels = c("A", "B", "C"),
#'   cell_size = "C",
#' )
#' 
#' # draw treemap
#' drawTreemap(tm)
#' 
#' @importFrom tidyr %>%
#' @importFrom dplyr mutate_if
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr pull
#' @importFrom scales rescale
#' 
#' @export sunburstTreemap

sunburstTreemap <- function(
  data, 
  levels, 
  fun = sum,
  sort = TRUE,
  filter = 0,
  cell_size = NULL,
  custom_color = NULL,
  diameter_inner = 0.3,
  diameter_outer = 0.8
) {
  
  # validate input data and parameters
  data <- validate_input(
    data, levels, fun,
    sort, filter, cell_size, 
    custom_color)
  
  # CORE FUNCTION (RECURSIVE)
  sunburst_core <- function(level, df, parent = c(0, 1), output = list()) {
    
    # 1. summarise current level's category
    ncells <- df[[levels[level]]] %>% table
    
    # 2. generate the weights, these are the (aggregated) scaling factors 
    # supplied by the user or simply the n members per cell
    if (is.null(cell_size)) {
      # average cell size by number of members, if no function is given
      weights <- ncells %>% cumsum %>% {./tail(., 1)}
    } else {
      # average cell size by user defined function, e.g. sum of expression values
      # the cell size is calculated as aggregated relative fraction of total
      stopifnot(is.numeric(df[[cell_size]]))
      weights <- df %>%
        dplyr::group_by(get(levels[level])) %>%
        dplyr::summarise(cumfun = fun(get(cell_size))) %>% 
        dplyr::pull(cumfun) %>% cumsum %>% {./tail(., 1)}
    }
    
    # 3. rescale the weights to lower and upper boundary of parent
    weights = scales::rescale(weights, from = c(0, 1), to = parent)
    lower_bound <- c(parent[1], weights[-length(weights)])
    upper_bound <- weights
    
    # 4. generate sector polygons and collect in list
    sectors <- lapply(1:length(ncells), function(i) {
      
      draw_sector(
        level = level,
        lower_bound = lower_bound[i],
        upper_bound = upper_bound[i],
        diameter_inner = diameter_inner,
        diameter_sector = (diameter_outer-diameter_inner)/length(levels),
        name = names(ncells)[i]
      )
      
    })
    
    revert_list <- function(x) do.call(Map, c(c, x))
    sectors <- revert_list(sectors)
    
    
    # CALL CORE FUNCTION RECURSIVELY
    if (level != length(levels)) {
      
      # iterate through all possible sub-categories,
      # these are the children of the parental polygon
      # and pass the children's polygon as new parental
      # also add current results to output list
      res <- lapply(1:length(ncells), function(i) {
        
        sunburst_core(
          level = level + 1,
          df = subset(df, get(levels[level]) %in% names(ncells)[i]),
          parent = c(lower_bound[i], upper_bound[i]),
          output = {
            output[[paste0("LEVEL", level, "_", names(ncells)[i])]] <- 
              list(
                names = sectors$names[i], 
                k = sectors$k[i],
                a = sectors$a[i],
                level = level,
                custom_color = {if (!is.null(custom_color)) color_value[[i]] else NA}
              )
            output
          }
        )
      }) %>%
        unlist(recursive = FALSE)
      return(res)
      
    } else {
      
      sectors$level = level
      sectors$custom_color = {if (!is.null(custom_color)) color_value else NA}
      output[[paste0("LEVEL", level, "_", names(ncells)[1])]] <- sectors
      return(output)
      
    }
  }
  
  # MAIN FUNCTION CALL
  # ------------------
  # iterate through all levels,
  # collect results in list, remove duplicated polygons
  # and order by hierarchical level
  tm <- sunburst_core(level = 1, df = data, parent = c(0, 1)) %>%
    .[!duplicated(.)]
  tm <- tm[names(tm) %>% order]
  cat("Treemap successfully created\n")
  
  
  # set S4 class and return result
  tm <- sunburstResult(
    cells = tm,
    data = data,
    call = list(
      levels = levels, 
      fun = fun,
      sort = sort,
      filter = filter,
      cell_size = cell_size,
      custom_color = custom_color,
      diameter_inner = diameter_inner,
      diameter_outer = diameter_outer
    )
  )

  return(tm)
  
}
