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
#'   cells that would not be visible. The default is to remove cells with a 
#'   relative target area below 0.0005, or 0.05%. The algorithm can still fail
#'   so it can be worthwhile to simply rerun the function, probably applying a
#'   stricter threshold.
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
#' @param maxIteration (numeric) Force algorithm to stop at this number of iterations
#'   for each parent cell. The algorithm usually converges to an acceptable 
#'   solution fairly quickly, so it seems reasonable to restrict this number
#'   in order to save computation time. However, more iterations give higher
#'   accuracy.
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
#' 
#' @import gpclib
#' @import tidyr
#' @importFrom grid grid.newpage
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
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
  filter = 0.0005,
  cell.size = NULL, 
  cell.color = levels[1], 
  color.palette = NULL,
  border.size = 6,
  border.color = grey(0.9),
  labels = levels[length(levels)],
  label.col = grey(0.5),
  maxIteration = 50,
  debug = FALSE)
{
  
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
  
  # filter out very small target areas that are barely visible
  # and will make the treemap generation more unstable
  if (!is.null(cell.size)) {
    if (data[[cell.size]] %>% {./sum(.) < 0.0005} %>% any) {
      if (is.numeric(filter)) {
        filtered <- data[[cell.size]] %>% {./sum(.)} >= filter
        data <- subset(data, filtered)
        warning(paste(sum(!filtered), "out of", length(filtered), 
          "cells were filtered due to target area falling below treshold."))
      } else {
        warning("Some cells have very small target areas. Use 'filter' argument to remove those automatically.")
      }
    }
  }
  
  # check levels/hierarchies and level options
  if (!all(levels %in% colnames(data))) {
    stop("Not all given levels are column names of 'data'")
  }
  if (is.null(labels)) {
    warning("Drawing of labels disabled.")
  } else if (!all(labels %in% levels)) {
    warning("'labels' is not a colname of 'data'. Drawing of labels disabled.")
    labels = NULL
  }
  if (is.null(cell.size)) {
    warning("No experimental condition is given, cell size encoded by number of members")
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
    data <- data[do.call("order", data[levels]), ]
  } else {
    warning(" Sorting is FALSE, it is expected that the input data is sorted")
  }
  
  # in debug mode, open a viewport to draw iterations
  # of treemap generation called from allocate()
  if (debug) {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(
      width = 0.9,
      height = 0.9,
      xscale = c(0,1000),
      yscale = c(0,1000)
    ))
  }
  
  # generate colors for each cell and add to data frame
  data$fill.color <- with(data, {
    colvector <- get(cell.color) %>%
    as.factor %>%
    as.numeric
    if (is.null(color.palette)) {
      colorspace::rainbow_hcl(length(unique(colvector)))[colvector]
    } else {
      colorRampPalette(color.palette)(length(unique(colvector)))[colvector]
    }
  })
  
  # CORE FUNCTION (RECURSIVE)
  voronoi.core <- function(level, df, parent=NULL, output=list()) {
    
    # CREATE VORONOI TREEMAP OBJECT
    #
    # 1. define the boundary polygon
    # either predefined rectangular bounding box for 1st level
    if (level==1) {
      
      ParentPoly <- list(
        x=c(0,0,1000,1000,0), 
        y=c(0,1000,1000,0,0)
      )
      # turn boundary polygon into gpc.poly object for treemap generation
      GpcPoly <- suppressWarnings(as(ParentPoly, "gpc.poly"))
    
    } else {
      
      # or the parental polygon in case of all lower levels > 1
      stopifnot(!is.null(parent))
      GpcPoly <- parent
      ParentPoly <- parent@pts[[1]][c("x", "y")]
      
    }
  
    # 2. generate starting coordinates within the boundary polygon
    # using sp package's spsample function
    ncells <- df[[levels[level]]] %>% table
    sampledPoints <- sp::Polygon(coords=ParentPoly) %>%
      sp::spsample(n=length(ncells), type="random") %>%
      {.@coords}
    
    
    # 3. generate the weights, these are the (aggregated) scaling factors 
    # supplied by the user or simply the n members per cell
    if (is.null(cell.size)) {
      # average cell size by number of members, if no function is given
      weights <- ncells/sum(ncells)
      
    } else {
      # average sector size by user defined function, e.g. sum of expression values
      # the cell size is calculated as aggregated relative fraction of total
      stopifnot(is.numeric(df[[cell.size]]))
      weights <- df %>%
        dplyr::group_by(get(levels[level])) %>%
        dplyr::summarise(fun(get(cell.size))) %>%
        {.[[2]]/sum(.[[2]])}
    }
    
    # 4. generate additively weighted voronoi treemap object;
    # the allocate function returns a list of polygons to draw, 
    # among others.
    # if the parent has only 1 child, skip map generation
    # and make pseudo treemap object instead
    
    if (length(ncells) == 1) {
      
      treemap <- list(
        names = names(ncells), 
        k = list(GpcPoly), 
        s = {soiltexture::TT.polygon.centroids(ParentPoly[[1]], ParentPoly[[2]]) %>%
          list(x=.[[1]], y=.[[2]])},
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
      
      # print summary of cell tesselation
      tessErr <- unlist(treemap$a) %>%
        {(./sum(.))-weights} %>% abs
      cat("Level" , level, "tesselation: ",
          round(mean(tessErr)*100, 2), "% mean error, ",
          round(max(tessErr)*100, 2), "% max error, ",
          treemap$count, "iterations\n"
        )
      
    }
    
    # DRAWING FUNCTION
    # this function draws the polygons as polygonGrob objects
    # from grid package
    voronoiGrid <- drawRegions(
      treemap, 
      debug = FALSE,
      label = {if (!is.null(labels)) levels[level] %in% labels else FALSE}, 
      label.col = label.col, 
      lwd = border.size/level, 
      col = border.color,
      fill = {if (level != length(levels)) NA else unique(df$fill.color)}
    )
    
    
    # CALL CORE FUNCTION RECURSIVELY
    if (level != length(levels)) {
      
      # iterate through all possible sub-categories,
      # these are the children of the parental polygon
      # and pass the children's polygon as new parental
      # also add current tesselation results to output list
      lapply(1:length(ncells), function(i) {
        
        
        voronoi.core(
          level=level+1, 
          df=subset(df, get(levels[level]) %in% names(ncells)[i]),
          parent=treemap$k[[i]],
          output={
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
      unlist(recursive=FALSE)
      
    } else {
      
      output[[paste0("LEVEL", level)]] <- voronoiGrid
      return(output)
      
    }
  }
  
  # MAIN FUNCTION CALL
  # iterate through all levels,
  # collect results in list, remove duplicated polygons
  # and order by hierarchical level
  tm <- voronoi.core(level=1, df=data) %>%
    .[!duplicated(.)]
  tm <- tm[names(tm) %>% order(decreasing=TRUE)]
  cat("Treemap successfully created\n")
  return(tm)
  
}
