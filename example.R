library(SysbioTreemaps)

# SIMPLE EXAMPLE
# -------------------------------------------
# generate data frame
df <- data.frame(
  A = rep(c("a", "b", "c"), each=15),
  B = sample(letters[4:13], 45, replace=TRUE),
  C = sample(1:100, 45)
)

# generate treemap
tm <- voronoiTreemap(
  data = df,
  levels = c("A", "B", "C"),
  cell.size = "C",
  cell.color = "B",
  labels="C",
  shape = "rounded_rect", 
  maxIteration = 20,
  seed = 123
)

# draw treemap
drawTreemap(tm)


# ADVANCED EXAMPLE
# -------------------------------------------
# read test data set from Jahn et al., Cell Reports, 2018
df <- Jahn_CellReports_2018 %>%
  subset(condition=="CO2-0-15")

# generate a custom color palette using colorspace
library(colorspace); hclwizard()
custom.pal <- sequential_hcl(n = 40, 
  h = c(-100, 100), 
  c = c(60, NA, 66), 
  l = c(42, 100), 
  power = c(2.65, 0.7), 
  rev = TRUE)

# Generate treemap using some more of the function's parameters.
# For example, we can supply more than one level for drawing labels,
# change label colors, encode cell size and cell color by
# a numeric variable, use a custom color palette, 
# and increase maxIterations which will lead to lower
# errors in some cases. Set a seed if you want the same arrangement 
# of cells every time, otherwise it will be random.
tm <- voronoiTreemap(
  data = df,
  levels = c("Process.abbr", "Pathway.abbr", "protein"),
  labels = c("Process.abbr", "protein"),
  label.col = grey(0.7, 0.4),
  border.color = grey(0.7),
  cell.size = "mean_mass_fraction_norm",
  cell.color = "mean_mass_fraction_norm",
  maxIteration = 200,
  color.palette = custom.pal
)

# draw treemap
drawTreemap(tm)

