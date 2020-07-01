## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "png/"
)

## ---- results = 'hide', message = FALSE, echo = FALSE, warning = FALSE--------
library(SysbioTreemaps)

# load example data
data(mtcars)
mtcars$car_name = gsub(" ", "\n", row.names(mtcars))

# generate treemap; set seed to obtain same pattern every time
tm <- voronoiTreemap(
  data = mtcars,
  levels = c("gear", "car_name"),
  cell_size = "wt",
  shape = "rounded_rect",
  seed = 123
)

## ---- fig.height = 5, fig.width = 5, out.width = "50%", fig.align = 'center', echo = FALSE----
# draw treemap
drawTreemap(tm, label_size = 2.5, label_color = "white", title = "An example")

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("m-jahn/SysbioTreemaps")

## ---- eval = FALSE------------------------------------------------------------
#  library(SysbioTreemaps)
#  
#  # load example data
#  data(mtcars)
#  mtcars$car_name = gsub(" ", "\n", row.names(mtcars))

## ---- eval = FALSE------------------------------------------------------------
#  # generate treemap; set seed to obtain same pattern every time
#  tm <- voronoiTreemap(
#    data = mtcars,
#    levels = c("gear", "car_name"),
#    cell_size = "wt",
#    shape = "rounded_rect",
#    seed = 123
#  )

## ---- fig.width = 5, fig.height = 5, out.width = "50%", fig.align = 'center'----
drawTreemap(tm, label_size = 2.5, label_color = "white")

## ---- fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center', warning = FALSE----
drawTreemap(tm, title = "treemap 1", label_size = 2,
  color_type = "categorical", color_level = 1,
  layout = c(1,3), position = c(1, 1))

drawTreemap(tm, title = "treemap 2", label_size = 2,
  color_type = "categorical", color_level = 2, border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 2))

drawTreemap(tm, title = "treemap 3", label_size = 2,
  color_type = "cell_size", color_level = 2,
  color_palette = heat.colors(10),
  border_color = grey(0.4), label_color = grey(0.4),
  add = TRUE, layout = c(1,3), position = c(1, 3),
  title_color = "black")

## ---- message = FALSE, error = FALSE, results = 'hide'------------------------
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

## ---- fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center', warning = FALSE----
drawTreemap(tm1, title = "positioning = 'random'", border_size = 3,
  layout = c(1,3), position = c(1, 1))

drawTreemap(tm2, title = "positioning = 'regular'", border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 2))

drawTreemap(tm3, title = "positioning = 'clustered'", border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 3))

## -----------------------------------------------------------------------------
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

## ---- message = FALSE, error = FALSE, results = 'hide'------------------------
tm1 <- voronoiTreemap(data = df, levels = "A",
  shape = house_coords)

tm2 <- voronoiTreemap(data = df, levels = "A",
  shape = rect_coords)

tm3 <- voronoiTreemap(data = df, levels = "A",
  shape = oct_coord)

## ---- fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center', warning = FALSE----
drawTreemap(tm1, layout = c(1,3), position = c(1, 1))
drawTreemap(tm2, add = TRUE, layout = c(1,3), position = c(1, 2))
drawTreemap(tm3, add = TRUE, layout = c(1,3), position = c(1, 3))

## ---- message = FALSE, error = FALSE, results = 'hide'------------------------
# additional libraries for data filtering and colors
library(dplyr)
library(colorspace)

# pick the top most abundant proteins
df <- Jahn_CellReports_2018 %>%
  filter(condition == "CO2-0-15") %>%
  arrange(desc(mean_mass_fraction_norm)) %>%
  slice(1:1000)

## ---- eval = FALSE------------------------------------------------------------
#  tm <- voronoiTreemap(
#    data = df,
#    levels = c("Process.abbr", "protein"),
#    cell_size = "mean_mass_fraction_norm",
#    shape = "rectangle",
#    error_tol = 0.005,
#    maxIteration = 200,
#    positioning = "clustered_by_area",
#    seed = 1
#  )

## ---- message = FALSE, error = FALSE, results = 'hide'------------------------
# outcomment to run interactive wizard:
#hclwizard()

custom_pal_1 <- sequential_hcl(
  n = 20,
  h = c(-46, 78),
  c = c(61, 78, 54),
  l = c(60, 91),
  power = c(0.8, 1),
  rev = TRUE
)

custom_pal_2 <- diverging_hcl(
  n = 7, 
  h = c(340, 128), 
  c = c(60, 80), 
  l = c(75, 97), 
  power = c(0.8, 1.5),
  rev = TRUE
)

## ---- eval = FALSE------------------------------------------------------------
#  drawTreemap(
#    tm,
#    color_palette = custom_pal_1,
#    color_type = "cell_size",
#    color_level = 2,
#    label_level = c(1,2),
#    label_size = 2,
#    label_color = grey(0.5),
#    border_color = grey(0.65),
#    layout = c(1, 2),
#    position = c(1, 1)
#  )
#  
#  drawTreemap(
#    tm,
#    color_palette = custom_pal_2,
#    color_type = "cell_size",
#    color_level = 2,
#    label_level = c(1,2),
#    label_size = 2,
#    label_color = grey(0.5),
#    border_color = grey(0.9),
#    layout = c(1, 2),
#    position = c(1, 2),
#    add = TRUE
#  )

## ---- fig.width = 10, fig.height = 5, out.width = "100%", fig.align = 'center', echo = FALSE----
# we load precomputed result to save time
knitr::include_graphics("png/unnamed-chunk-14-1.png")

## -----------------------------------------------------------------------------
library(parallel)

df <- Jahn_CellReports_2018 %>%
  group_by(condition) %>%
  arrange(desc(mean_mass_fraction_norm)) %>%
  slice(1:200)

## ---- eval = FALSE------------------------------------------------------------
#  tm <- mclapply(
#    unique(df$condition),
#    mc.cores = 10,
#    mc.set.seed = FALSE,
#    FUN = function(cond) {
#  
#      voronoiTreemap(
#        data = filter(df, condition == cond),
#        levels = c("Process.abbr", "protein"),
#        cell_size = "mean_mass_fraction_norm",
#        custom_color = "mean_mass_fraction_norm",
#        shape = "rounded_rect",
#        positioning = c("regular", "clustered_by_area"),
#        maxIteration = 50,
#        error_tol = 0.01
#      )
#    }
#  )

## ---- eval = FALSE------------------------------------------------------------
#  lapply(1:10, function(i) {
#  
#    drawTreemap(
#      tm[[i]],
#      color_type = "custom_color",
#      color_level = 2,
#      color_palette = custom_pal_2,
#      custom_range = c(0, 0.05),
#      border_size = 6,
#      border_color = grey(0.9),
#      label_level = c(1,2),
#      label_size = 1.5,
#      label_color = grey(0.4),
#      legend = TRUE,
#      title = unique(df$condition)[i],
#      title_size = 1.5,
#      title_color = grey(0.4),
#      layout = c(2, 5),
#      position = c(
#        ifelse(i <= 5, 1, 2),
#        ifelse(i <= 5, i, i-5)),
#      add = ifelse(i == 1, FALSE, TRUE)
#    )
#  
#  }) %>% invisible

## ---- fig.width = 20, fig.height = 8, out.width = "100%", fig.align = 'center', echo = FALSE----
# we load precomputed result to save time
knitr::include_graphics("png/unnamed-chunk-17-1.png")

## -----------------------------------------------------------------------------
# generate data frame
set.seed(123)
df <- data.frame(
  A = rep(c("a", "b", "c"), each = 15),
  B = sample(letters[4:12], 45, replace = TRUE)
)

head(df)

## ---- results = 'hide'--------------------------------------------------------
# by default cell (sector) size is encoded by number of members per group
tm <- sunburstTreemap(
  data = df,
  levels = c("A", "B")
)

## ---- fig.width = 12, fig.height = 4, out.width = "100%", fig.align = 'center', warning = FALSE----
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

