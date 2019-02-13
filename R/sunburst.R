# SUNBURST TREEMAPS OR 'SECTOR PLOTS'
# as alternative to voronoi treemaps
# 
# author: Michael Jahn
# affiliation: Scilifelab - KTH, Stockholm
# date: 2019-02-05


## MAIN FUNCTION
sunburstTreemap <- function(
  data, levels, fun = length,
  sort = "alphabetical", sort.level = 1:length(levels),
  sector.size = NA, sector.col = NA, height.inner = 0.3,
  height = (0.7 - height.inner) / length(levels),
  range = NULL, col = c("royalblue", "chartreuse3", "darkgoldenrod1", "red"),
  col.steps = 100, border = NULL, lwd = 1, labels = NA,
  label.col = grey(0.5), label.thresh = NA, main = NULL, 
  draw.legend = TRUE) 
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
  
  # check levels = hierarchies and level options
  if (!all(levels %in% colnames(data)))
    stop("Not all given levels are column names of 'data'")
  if (!labels %in% levels)
    stop("Column 'labels' is not one of the levels")
  if (is.na(sector.size))
    warning("No experimental condition is given, sector size encoded by number of members")
  if (!is.na(sector.size) & !(sector.size %in% colnames(data)))
    stop("'sector.size' is not a colname of 'data'")
  if (!is.na(sector.col) & !(sector.col %in% colnames(data)))
    stop("'sector.col' is not a colname of 'data'")
  if ((!is.numeric(range) | is.null(range)) & !is.na(sector.col)) {
    range <- c(min(data[[sector.col]], na.rm = TRUE),
               max(data[[sector.col]], na.rm = TRUE))
  }
  if (!is.function(fun))
    stop("'fun' must be a function")
  
  # sort data in case it is unsorted
  if (sort == "alphabetical")
    sort.fun <- function(df, level)
      order(df[[levels[level]]])
  else if (sort == "sector.size") {
    if (is.na(sector.size))
      stop("if ordering by sector.size, a column name must be given")
    else {
      sort.fun <- function(df, level) {
        level.order <-
          as.numeric(reorder(x = df[[levels[level]]], df[[sector.size]], fun))
        order(level.order, decreasing = TRUE)
      }
    }
  }
  else if (sort == "none")
    warning("Sorting is omitted")
  else
    stop("'sort' must be one of 'none', 'alphabetical' or 'sector.size'")
  if (!is.numeric(sort.level) |
      any(!sort.level %in% 1:length(levels)))
    stop("'sort.level' must be numeric and include at least one level")
  
  
  # CALCULATE SECTOR RANGE AND DRAW SECTOR
  draw.sector <- function(level,
    lower.bound,
    upper.bound,
    color,
    draw.label,
    sector.name) {
    
    #draw sectors
    segment <- c(lower.bound, upper.bound) * 2 * pi
    a <- height.inner + (height * (level - 1))
    z <- seq(segment[1], segment[2], length = 50)
    xx <- c(a * cos(z), rev((a + height) * cos(z)))
    yy <- c(a * sin(z), rev((a + height) * sin(z)))
    polygon(xx, yy,
      border = border,
      col = color,
      lwd = lwd)
    
    # draw labels
    if (draw.label) {
      if (a * cos(median(z)) >= 0)
        side = 1
      else
        side = -1
      sinz <- sin(median(z))
      cosz <- cos(median(z))
      #draw label arcs
      z <- z[-c(1, length(z))]
      lines(c(0.72 * cos(z[1]), 0.75 * cos(z), 0.72 * cos(tail(z, 1))),
            c(0.72 * sin(z[1]), 0.75 * sin(z), 0.72 * sin(tail(z, 1))),
            col = label.col)
      #draw label lines
      lines(
        x = c(0.75 * cosz, 0.75 * cosz + 0.25 * cosz * abs(sinz), 0.8 * side),
        y = c(0.75 * sinz, 0.75 * sinz + 0.25 * sinz * abs(sinz), 
          0.75 * sinz + 0.25 * sinz * abs(sinz)),
        col = label.col
      )
      #draw label text
      text(
        x = 0.8 * side,
        y = 0.75 * sinz + 0.25 * sinz * abs(sinz),
        labels = substring(sector.name, 1, 18),
        col = label.col,
        pos = 3 + side,
        offset = 0.2,
        cex = 0.7
      )
    }
  }
  
  # CORE FUNCTION
  sector.core <- function(level, df, lower.bound) {
    
    # sorting of data
    if (!sort == "none" & level %in% sort.level)
      df <- df[sort.fun(df, level),]
    
    # iterate through categories of one level
    for (category in unique(df[[levels[level]]])) {
      
      # SECTOR BOUNDARY AND COLOR SETTINGS
      sector.hits <- which(df[[levels[level]]] == category)
      
      if (is.na(sector.size))
        # average sector size by number of members, if no function is given
        upper.bound <- lower.bound + length(sector.hits) / nrow(data)
      else
        # average sector size by given function, e.g. sum of expression values
        upper.bound <-
          lower.bound + fun(df[sector.hits, sector.size]) / fun(data[[sector.size]])
      
      # define sector colors, default is to use a colorRamp
      if (is.na(sector.col))
        color <- colorRampPalette(col)(length(levels))[level]
      else {
        ramp <- colorRampPalette(col)(col.steps)
        expression <- fun(df[sector.hits, sector.col])
        if (expression < range[1])
          color <- ramp[1]
        if (expression > range[2])
          color <- ramp[col.steps]
        if (expression >= range[1] & expression <= range[2]) {
          color <- ramp[1 + ((expression - range[1]) / (range[2] - range[1]) * (col.steps - 1))]
        }
      }
      
      # decision about drawing labels
      if (!is.na(labels) & levels[level] == labels) {
        if (!is.na(label.thresh) & is.numeric(label.thresh))
          draw.label = fun(df[sector.hits, sector.size]) >= label.thresh
        else
          draw.label = TRUE
      }
      else
        draw.label = FALSE
      
      # CALL DRAWING FUNCTION
      draw.sector(
        level,
        lower.bound,
        upper.bound,
        color = color,
        draw.label,
        sector.name = category
      )
      
      # CALL CORE FUNCTION RECURSIVELY
      if (level != length(levels))
        sector.core(level = level + 1,
          df = df[sector.hits, ],
          lower.bound = lower.bound)
      
      # set sector 'counter' to next higher bound
      lower.bound <- upper.bound
    }
  }
  
  # FUNCTION CALL
  par(mar = c(0, 0, 0, 0))
  plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1), axes = FALSE)
  sector.core(level = 1, df = data, lower.bound = 0)
  text(0, 0, labels = main, col = grey(0.5))
  
  
  # DRAW LEGEND
  if (draw.legend) {
    xcoord <- grconvertX(0, from = "ndc", to = "user")
    ycoord <- grconvertY(1, from = "ndc", to = "user")
    if (is.na(sector.col))
      legend(
        xcoord, ycoord, legend = levels,
        pch = 15, pt.cex = 3,
        col = colorRampPalette(col)(length(levels)),
        xpd = NA, bty = "n"
      )
    else {
      legend.labels <- pretty(range, n = 5)
      col = colorRampPalette(col)(length(legend.labels))
      legend(
        xcoord, ycoord, legend = legend.labels,
        pch = 15, col = col, pt.cex = 2, cex = 0.6,
        text.col = grey(0.5), xpd = NA, bty = "n"
      )
    }
  }
}
