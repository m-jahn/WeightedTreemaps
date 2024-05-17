## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "png/"
)

## ----install_1, eval = FALSE--------------------------------------------------
#  install.packages("WeightedTreemaps")

## ----install_2, eval = FALSE--------------------------------------------------
#  devtools::install_github("m-jahn/WeightedTreemaps")

## ----mtcars_load--------------------------------------------------------------
library(WeightedTreemaps)

# load example data
data(mtcars)
mtcars$car_name = gsub(" ", "\n", row.names(mtcars))

## ----mtcars_tm----------------------------------------------------------------
# set seed to obtain same pattern every time
tm <- voronoiTreemap(
  data = mtcars,
  levels = c("gear", "car_name"),
  cell_size = "wt",
  shape = "rounded_rect",
  seed = 123
)

## ----mtcars_tm_draw, fig.width = 5, fig.height = 5, out.width = "50%", fig.align = 'center'----
drawTreemap(tm, label_size = 2.5, label_color = "white")

## ----mtcars_draw_examples, fig.width = 9, fig.height = 9, out.width = "100%", fig.align = 'center'----
drawTreemap(tm, title = "treemap 1", label_size = 2,
  color_type = "categorical", color_level = 1,
  layout = c(2, 2), position = c(1, 1), legend = TRUE)

drawTreemap(tm, title = "treemap 2", label_size = 2,
  color_type = "categorical", color_level = 2, border_size = 3,
  add = TRUE, layout = c(2, 2), position = c(1, 2), legend = TRUE)

drawTreemap(tm, title = "treemap 3", label_size = 2,
  color_type = "both", color_level = 1,
  add = TRUE, layout = c(2, 2), position = c(2, 1), legend = TRUE)

drawTreemap(tm, title = "treemap 4", label_size = 2,
  color_type = "cell_size", color_level = 2,
  color_palette = heat.colors(10),
  border_color = grey(0.4), label_color = grey(0.4),
  add = TRUE, layout = c(2, 2), position = c(2, 2),
  title_color = "black", legend = TRUE)

## ----mtcars_positioning-------------------------------------------------------
# set seed to obtain same df every time
set.seed(123)
df <- data.frame(A = sample(10:100, 45))

tm1 <- voronoiTreemap(
  data = df, levels = "A",
  cell_size = "A",
  shape = "rounded_rect",
  positioning = "random"
)

tm2 <- voronoiTreemap(
  data = df, levels = "A",
  cell_size = "A",
  shape = "rounded_rect",
  positioning = "regular"
)

tm3 <- voronoiTreemap(
  data = df, levels = "A",
  cell_size = "A",
  shape = "rounded_rect",
  positioning = "clustered"
)

## ----mtcars_positioning_draw, fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center'----
drawTreemap(tm1, title = "positioning = 'random'", border_size = 3,
  layout = c(1,3), position = c(1, 1))

drawTreemap(tm2, title = "positioning = 'regular'", border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 2))

drawTreemap(tm3, title = "positioning = 'clustered'", border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 3))

## ----custom_shapes------------------------------------------------------------
# different initial shapes, the more squared the better
house_coords <- list(
  x = c(0, 10, 10, 5, 0),
  y = c(0, 0, 10,15,10))

rect_coords <- list(
  x = c(0, 10, 10, 0),
  y = c(0, 0, 3, 3))

oct_coord <- list(
  x = sin(seq(0, 2, 2/8)*pi) * 1000 + 1000,
  y = cos(seq(0, 2, 2/8)*pi) * 1000 + 1000
)

## ----custom_shapes_tm---------------------------------------------------------
tm1 <- voronoiTreemap(data = df, levels = "A",
  shape = house_coords)

tm2 <- voronoiTreemap(data = df, levels = "A",
  shape = rect_coords)

tm3 <- voronoiTreemap(data = df, levels = "A",
  shape = oct_coord)

## ----custom_shapes_draw, fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center'----
drawTreemap(tm1, layout = c(1,3), position = c(1, 1))
drawTreemap(tm2, add = TRUE, layout = c(1,3), position = c(1, 2))
drawTreemap(tm3, add = TRUE, layout = c(1,3), position = c(1, 3))

## ----sunburst-----------------------------------------------------------------
# generate data frame
set.seed(123)
df <- data.frame(
  A = rep(c("a", "b", "c"), each = 15),
  B = sample(letters[4:12], 45, replace = TRUE)
)

head(df)

## ----sunburst_tm--------------------------------------------------------------
# by default cell (sector) size is encoded by number of members per group
tm <- sunburstTreemap(
  data = df,
  levels = c("A", "B")
)

## ----sunburst_draw, fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center'----
# draw treemap with default options
drawTreemap(tm,
  title = "A sunburst treemap",
  legend = TRUE,
  border_size = 2,
  label_color = grey(0.6),
  layout = c(1, 3),
  position = c(1, 1)
)

# use custom color palette
drawTreemap(tm,
  title = "Use custom palette",
  legend = TRUE,
  color_palette = rep(c("#81E06E", "#E68CFF", "#76BBF7"), c(3, 4, 5)),
  border_size = 2,
  label_level = 2,
  label_size = 0.7,
  label_color = grey(0.5),
  layout = c(1, 3),
  position = c(1, 2),
  add = TRUE
)

# color cells (sectors) based on cell size
drawTreemap(tm,
  title = "Coloring encoded by cell size",
  color_type = "cell_size",
  legend = TRUE,
  color_palette = rev(heat.colors(10)),
  border_size = 3,
  border_color = grey(0.3),
  label_level = 1,
  label_size = 2,
  label_color = grey(0.3),
  layout = c(1, 3),
  position = c(1, 3),
  add = TRUE
)

