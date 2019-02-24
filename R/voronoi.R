#' voronoiTreemap
#'
#' Create nested additively weighted Voronoi treemaps. 
#'
#' This is a recursive wrapper function, making use of the original implementation 
#' of the voronoi tesselation from Paul Murrell, University of Auckland.
#' The original functions were obtained and slightly modified from
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/VoronoiTreemap/voronoiTreeMap.html}
#' This function returns a list of graphical objects instead of a plot. In order 
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
#'   cells that may not be visible. The default is to remove cells with a 
#'   relative target area below 0.0001, or 0.01%. The algorithm can fail
#'   when processing many tiny cells so it can be worthwhile to simply 
#'   rerun the function with a stricter filter.
#' @param cell.size (character) Indicates the column used to control cell size. 
#'   Can be one of \code{levels}
#' @param cell.color (character) Indicates the column used to control cell color. 
#'   Can be one of \code{levels}
#' @param color.palette (character) A character vector of colors used to fill cells. 
#' @param border.size (character) The initial line width of the highest level 
#'   cells. Is reduced each level.
#' @param border.color (numeric) Color for cell borders.
#' @param labels (character) The column name indicating the hierarchical level 
#'   used for cell labels, or NULL to omit drawing labels. The default is the
#'   lowest level.
#' @param label.col (character) Color for cell labels.
#' @param shape (character) Set the initial shape of the treemap. Currently 
#'   supported are "rectangle", "circle" or "hexagon".
#' @param maxIteration (numeric) Force algorithm to stop at this number of iterations
#'   for each parent cell. The algorithm usually converges to an acceptable 
#'   solution fairly quickly, so it seems reasonable to restrict this number
#'   in order to save computation time. However, more iterations give higher
#'   accuracy.
#' @param seed (integer) The default seed is NULL, which will lead to a new 
#'   random sampling of cell coordinates for each tesselation. If you want
#'   a reproducibloe arrangement of cells, set seed to an arbitrary number.
#' @param debug (logical) If debug is TRUE (default is FALSE), the solution 
#'   for each iteration is drawn to the viewport to allow some visual 
#'   inspection. The weights, target area, and difference are printed to the 
#'   console. Use with care, this makes treemap generation much slower!
#'   
#' 
#' @return A named list with grid graphical objects for polygons and labels. 
#' 
#' @seealso \code{\link{drawTreemap}} for drawing the treemap.
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
#'   cell.size = "C",
#'   cell.color = "A",
#'   maxIteration = 50,
#'   )
#' 
#' # plot treemap
#' drawTreemap(tm)
#' 
#' 
#' @import gpclib
#' @import tidyr
#' @importFrom grid grid.newpage
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom dplyr mutate_if
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom scales rescale
#' @importFrom sp Polygon
#' @importFrom sp spsample
#' @importFrom soiltexture TT.polygon.centroids
#' @importFrom Rcpp sourceCpp
#' 
#' @useDynLib SysbioTreemaps, .registration = TRUE
#' 
#' @export voronoiTreemap

voronoiTreemap <- function(
  data,
  levels,
  fun = sum,
  sort = TRUE,
  filter = 0,
  cell.size = NULL,
  cell.color = levels[1],
  color.palette = NULL,
  border.size = 6,
  border.color = grey(0.9),
  labels = levels[length(levels)],
  label.col = grey(0.5),
  shape = "rectangle",
  maxIteration = 100,
  seed = NULL,
  debug = FALSE) {

  # ERROR HANDLING
  #
  # check input data frame
  if (!is.data.frame(data)) {
    if (!is.matrix(data))
      stop("'data' must be a matrix or a data frame")
    else if (is.null(colnames(data)))
      stop("'data' must have column names")
    else
      data <- as.data.frame(data)
  }

  # filter out zero or negative target areas, or apply a user threshold
  # to filter target areas
  if (!is.null(cell.size)) {
    if (is.numeric(filter)) {
      
      filtered <- data[[cell.size]] %>% {. / sum(.)} > filter
      
      if (sum(!filtered) > 0) {
        data <- subset(data, filtered)
        cat(sum(!filtered), "out of", length(filtered), 
          "cells were filtered due to target area falling below treshold.\n")
      }
    }
  }
  
  # check levels/hierarchies and level options
  if (!all(levels %in% colnames(data))) {
    stop("Not all given levels are column names of 'data'")
  }
  
  # check that no level columns are factors and coerce 
  # to character if necessary
  if (lapply(df, is.factor) %>% unlist %>% any) {
    data <- data %>% dplyr::mutate_if(is.factor, as.character)
  }
  
  # check labels
  if (is.null(labels)) {
    warning("Drawing of labels disabled.", immediate. = TRUE)
  } else if (!all(labels %in% levels)) {
    warning("'labels' is not a colname of 'data'. Drawing of labels disabled.", immediate. = TRUE)
    labels = NULL
  }
  if (is.null(cell.size)) {
    warning("No experimental condition is given, cell size encoded by number of members", immediate. = TRUE)
  } else if (!(cell.size %in% colnames(data))) {
    stop("'cell.size' is not a colname of 'data'")
  }
  if (!is.null(cell.color) & !(cell.color %in% colnames(data))) {
    stop("'cell.color' is not a colname of 'data'")
  }

  if (!is.function(fun)) {
    stop("'fun' must be a function")
  }
  # sort data in case it is unsorted
  if (sort) {
    data <- data[do.call("order", data[levels]),]
  } else {
    warning("Sorting is FALSE, it is expected that the input data is sorted", immediate. = TRUE)
  }

  # in debug mode, open a viewport to draw iterations
  # of treemap generation called from allocate()
  if (debug) {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(
      width = 0.9,
      height = 0.9,
      xscale = c(0, 2000),
      yscale = c(0, 2000)
    ))
  }

  # generate a color value for each cell and add to data frame
  data$fill <- with(data, {
    
    # if input is character or similar, we try to convert to factor 
    # and then numeric; otherwise use numeric and rescale
    # to a new range between 1 and 100
    if (is.numeric(get(cell.color))) {
      get(cell.color) %>%
      scales::rescale(to = c(1, 100))
      
    } else {
      get(cell.color) %>%
      as.factor %>%
      as.numeric %>%
      scales::rescale(to = c(1, 100))
    }
  })
  
  # define color palette with 100 steps
  if (is.null(color.palette)) {
    pal <- colorspace::rainbow_hcl(100, start = 60)
  } else {
    pal <- colorRampPalette(color.palette)(100)
  }
  
  # CORE FUNCTION (RECURSIVE)
  voronoi.core <- function(level, df, parent = NULL, output = list()) {
  
    # Setting the same seed here will sample the same set of coordinates
    # when drawing similar maps, and lead to similar layout
    if (!is.null(seed)) { 
      set.seed(seed) 
    }
    
    repeat {
      
      # set counter for number of maximum tries to not get stuck
      # in repeat loop
      counter = 1
      
      # CREATE VORONOI TREEMAP OBJECT
      #
      # 1. define the boundary polygon
      # either predefined rectangular bounding box for 1st level
      if (level == 1) {
        
        if (shape == "rectangle") {
          ParentPoly <- list(
            x = c(0, 0, 2000, 2000, 0),
            y = c(0, 2000, 2000, 0, 0)
          )
        } else if (shape == "circle") {
          ParentPoly <- list(
            x = sin(seq(0, 2, 2/50)*pi) * 1000 + 1000,
            y = cos(seq(0, 2, 2/50)*pi) * 1000 + 1000
          )
        } else if (shape == "hexagon") {
          ParentPoly <- list(
            x = sin(seq(0, 2, 2/6)*pi) * 1000 + 1000,
            y = cos(seq(0, 2, 2/6)*pi) * 1000 + 1000
          )
        } else {
          stop("shape is none of 'rectangle', 'circle', or 'hexagon'.")
        }
        
        # turn boundary polygon into gpc.poly object for treemap generation
        GpcPoly <- suppressWarnings(as(ParentPoly, "gpc.poly"))

      } else {

        # or the parental polygon in case of all lower levels > 1
        stopifnot(!is.null(parent))
        GpcPoly <- parent
        ParentPoly <- parent@pts[[1]][c("x", "y")]

      }

      # 2. generate starting coordinates within the boundary polygon
      # using sp package's spsample function. The set.seed() is important
      # here because it makes the sampling reproducible
      # (same set of coordinates for same query)
      ncells <- df[[levels[level]]] %>% table
      sampledPoints <- sp::Polygon(coords = ParentPoly) %>%
        sp::spsample(n = length(ncells), type = "random") %>% { .@coords }
      
      # 3. generate the weights, these are the (aggregated) scaling factors 
      # supplied by the user or simply the n members per cell
      if (is.null(cell.size)) {
        # average cell size by number of members, if no function is given
        weights <- ncells / sum(ncells)

      } else {
        # average sector size by user defined function, e.g. sum of expression values
        # the cell size is calculated as aggregated relative fraction of total
        stopifnot(is.numeric(df[[cell.size]]))
        weights <- df %>%
          dplyr::group_by(get(levels[level])) %>%
          dplyr::summarise(fun(get(cell.size))) %>% 
          { .[[2]] / sum(.[[2]]) }
      }
      
      # 4. generate colors by aggregating color
      # scores for each cell using a palette, applies only to lowest level
      if (level == length(levels)) {
        fill.color <- df %>%
          dplyr::group_by(get(levels[level])) %>%
          dplyr::summarise(mean(fill)) %>%
          {.[[2]]} %>% round %>%
          pal[.]
      }
      
      # 5. generate additively weighted voronoi treemap object;
      # the allocate function returns a list of polygons to draw, 
      # among others.
      # if the parent has only 1 child, skip map generation
      # and make pseudo treemap object instead
      if (length(ncells) == 1) {

        treemap <- list(
          names = names(ncells),
          k = list(GpcPoly),
          s = {
            soiltexture::TT.polygon.centroids(ParentPoly[[1]], ParentPoly[[2]]) %>%
            list(x = .[[1]], y = .[[2]])
          },
          w = weights,
          a = gpclib::area.poly(GpcPoly),
          t = weights
        )
      } else {
      
        treemap <- allocate(
          names = names(ncells),
          s = list(
            x = sampledPoints[, 1],
            y = sampledPoints[, 2]),
          w = weights,
          target = weights,
          maxIteration = maxIteration,
          outer = GpcPoly,
          debug = debug
        )
        
        if (is.null(treemap) & counter <= 10) {
          counter = counter + 1
          cat("Iteration failed, randomising positions...\n")
          next
        } else if (is.null(treemap) & counter > 10) {
          stop("Iteration failed after 10 randomisation trials, try to rerun treemap with new seed")
        }

        # print summary of cell tesselation
        tessErr <- unlist(treemap$a) %>% {(. / sum(.)) - weights } %>% abs
        cat("Level", level, "tesselation: ",
          round(mean(tessErr) * 100, 2), "% mean error, ",
          round(max(tessErr) * 100, 2), "% max error, ",
          treemap$count, "iterations\n"
        )

      }

      # DRAWING FUNCTION
      # this function draws the polygons as polygonGrob objects
      # from grid package
      voronoiGrid <- drawRegions(
      treemap,
      debug = FALSE,
      label = { if (!is.null(labels)) levels[level] %in% labels else FALSE },
      label.col = label.col,
      lwd = border.size / level,
      col = border.color,
      fill = { if (level != length(levels)) NA else fill.color }
    )
    
    
      # CALL CORE FUNCTION RECURSIVELY
      if (level != length(levels)) {

        # iterate through all possible sub-categories,
        # these are the children of the parental polygon
        # and pass the children's polygon as new parental
        # also add current tesselation results to output list
        res <- lapply(1:length(ncells), function(i) {
          
          voronoi.core(
            level = level + 1,
            df = subset(df, get(levels[level]) %in% names(ncells)[i]),
            parent = treemap$k[[i]],
            output = {
              if ("labels" %in% names(voronoiGrid)) {
                addOut <- voronoiGrid[c("labels", names(voronoiGrid)[i])]
              } else {
                addOut <- voronoiGrid[i]
              }
              output[[paste0("LEVEL", level)]] <- addOut
              output
            }
          )
        }) %>%
        unlist(recursive = FALSE)
        return(res)
      
      } else {
      
        output[[paste0("LEVEL", level)]] <- voronoiGrid
        return(output)
      
      }
    }
  }

  # MAIN FUNCTION CALL
  # iterate through all levels,
  # collect results in list, remove duplicated polygons
  # and order by hierarchical level
  tm <- voronoi.core(level = 1, df = data) %>%
    .[!duplicated(.)]
  tm <- tm[names(tm) %>% order(decreasing = TRUE)]
  cat("Treemap successfully created\n")
  return(tm)

}
