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
  shape = "rounded_rect",
  maxIteration = 100,
  debug = FALSE
)

# draw treemap
drawTreemap(tm, title = "A treemap", legend = TRUE)

# draw different variants of the same treemap on one page using
# the 'layout' and 'position' arguments (indicating rows and columns)
drawTreemap(tm, title = "treemap 1", legend = TRUE,
  color_type = "categorical", color_level = 1, 
  layout = c(1,3), position = c(1, 1))

drawTreemap(tm, title = "treemap 2", legend = TRUE,
  color_type = "categorical", color_level = 2, border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 2))

drawTreemap(tm, title = "treemap 3", legend = TRUE,
  color_type = "cell_size", color_level = 3,
  color_palette = heat.colors(10),
  border_color = grey(0.4), label_color = grey(0.4),
  add = TRUE, layout = c(1,3), position = c(1, 3),
  title_color = "black")


# It is also possible to control the initial placement of cells
# by using the "positioning" argument. Compare the following examples
tm1 <- voronoiTreemap(
  data = df, levels = "C",
  cell_size = "C",
  shape = "rounded_rect",
  positioning = "random"
)

tm2 <- voronoiTreemap(
  data = df, levels = "C",
  cell_size = "C",
  shape = "rounded_rect",
  positioning = "regular"
)

tm3 <- voronoiTreemap(
  data = df, levels = "C",
  cell_size = "C",
  shape = "rounded_rect",
  positioning = "clustered"
)

# draw treemaps
drawTreemap(tm1, title = "positioning = 'random'", border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 1))
drawTreemap(tm2, title = "positioning = 'regular'", border_size = 3,
  add = TRUE, layout = c(1,3), position = c(1, 2))
drawTreemap(tm3, title = "positioning = 'clustered'", border_size = 3,
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
  levels = c("Process.abbr", "protein"),
  cell_size = "mean_mass_fraction_norm",
  shape = "rectangle",
  error_tol = 0.01,
  maxIteration = 100,
  positioning = "clustered_by_area"
)

# save and load very large treemaps to avoid re-computation
#save(tm, file = "tm.Rdata")
#load("tm.Rdata")


# We can generate custom color palettes using the colorspace wizard.
# Just browse to the R tab and copy the code to your script.
hclwizard()

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


# Draw the treemap using some custom graphical parameters.
svg("vignettes/tm_heatcol.svg", 10, 10)
drawTreemap(
  tm, 
  color_palette = custom_pal_2,
  color_type = "cell_size",
  color_level = 2,
  label_level = c(1,2),
  label_size = 4,
  label_color = grey(0.5),
  border_color = grey(0.9)
)
dev.off()


# GENERATE TREEMAPS WITH PARALLEL COMPUTING
# -------------------------------------------
# Read test data set with proteomics data from 10 different conditions
library(parallel)

df <- Jahn_CellReports_2018 %>%
  filter(mean_mass_fraction_norm > 0)


# Generate 10 treemaps using the parallel version of lapply, and the 'condition'
# annotation to subset the data frame. Note that you can adjust the 'mc.cores'
# parameter to the number of CPUs available on your computer.

tm <- mclapply(
  unique(df$condition), 
  mc.cores = 10, 
  mc.set.seed = FALSE,
  FUN = function(cond) {
    
    voronoiTreemap(
      data = filter(df, condition == cond),
      levels = c("Process.abbr", "protein"), 
      cell_size = "mean_mass_fraction_norm",
      custom_color = "mean_mass_fraction_norm",
      shape = "rounded_rect",
      positioning = "clustered_by_area",
      maxIteration = 200,
      error_tol = 0.0025
    )
  }
)

# Draw all 10 treemaps on one canvas using layout and position arguments.
png("10_treemaps.png", width = 3000, height = 1200)
lapply(1:10, function(i) {
  
  drawTreemap(
    tm[[i]],
    color_type = "custom_color",
    color_level = 2,
    color_palette = custom_pal2,
    custom_range = c(0, 0.05),
    border_size = 6,
    border_color = grey(0.9),
    label_level = c(1,2),
    label_size = 2,
    label_color = grey(0.4),
    legend = TRUE,
    title = unique(df$condition)[i],
    title_size = 1.5,
    title_color = grey(0.4),
    layout = c(2, 5),
    position = c(
      ifelse(i <= 5, 1, 2),
      ifelse(i <= 5, i, i-5)),
    add = TRUE
  )
  
}) %>% invisible
dev.off()

