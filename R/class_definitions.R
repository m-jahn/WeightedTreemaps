#
# S4 class definitions for sunburst and voronoi treemaps
# ------------------------------------------------------
# 
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