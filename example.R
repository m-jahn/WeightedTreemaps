library(SysbioTreemaps)

# SIMPLE EXAMPLE
# -------------------------------------------
# generate data frame
df <- data.frame(
  A = rep(c("a", "b", "c"), each=15),
  B = sample(letters[4:12], 45, replace = TRUE),
  C = sample(10:100, 45)
)

# generate treemap
tm <- voronoiTreemap(
  data = df,
  levels = c("A", "B", "C"),
  cell_size = "C",
  shape = "rounded_rect"
)

# draw treemap
drawTreemap(tm)

# draw different variants of the same treemap on one page using
# the 'layout' and 'position' arguments (indicating rows and columns)
drawTreemap(tm, 
  cell_color = 1, title = "treemap 1", 
  layout = c(1,3), position = c(1, 1))

drawTreemap(tm, title = "treemap 2", 
  cell_color = 2, border_size = 6, 
  add = TRUE, layout = c(1,3), position = c(1, 2))

drawTreemap(tm, title = "treemap 3",
  cell_color = 3, border_color = grey(0.4), 
  label_color = grey(0.4),
  color_palette = heat.colors(10),
  add = TRUE, layout = c(1,3), position = c(1, 3))


# ADVANCED EXAMPLE
# -------------------------------------------
# read test data set from Jahn et al., Cell Reports, 2018
library(dplyr)
library(colorspace)

df <- Jahn_CellReports_2018 %>%
  filter(condition == "CO2-0-15") %>%
  filter(mean_mass_fraction_norm > 0)


# Generate treemap using some more of the function's parameters.
# We can increase maxIterations and decrease error tolerance which will lead to lower
# errors. We can set a seed to obtain a similar arrangment of cells for similar maps, 
# otherwise it will be random.
tm <- voronoiTreemap(
  data = df,
  levels = c("Process.abbr", "Pathway.abbr", "protein"),
  cell_size = "mean_mass_fraction_norm",
  shape = "rectangle",
  error_tol = 0.001,
  maxIteration = 200,
  seed = 13
)


# generate a custom color palette using colorspace
hclwizard()
custom.pal <- sequential_hcl(n = 40, 
  h = c(-100, 100), 
  c = c(60, NA, 66), 
  l = c(42, 100), 
  power = c(2.65, 0.7), 
  rev = TRUE)


# draw treemap
drawTreemap(
  tm, 
  color_palette = custom.pal,
  cell_color = "cell_size",
  labels = c(2,3),
  label_color = grey(0.5),
  border_color = grey(0.7)
)

