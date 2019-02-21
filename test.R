library(SysbioTreemaps)
setwd("/home/michael/Documents/SciLifeLab/Resources/R_projects/SysbioTreemaps")
# 
# 
# # TODO include check to coerce factors to strings or not allow factors at all
# 
# df <- data.frame(stringsAsFactors = FALSE,
#   A=rep(c("a", "b", "c"), each=15),
#   B=sample(letters[4:13], 45, replace=TRUE),
#   C=sample(1:100, 45)
# )
# 
# 
# tm <- voronoiTreemap(
#   data = df,
#   levels = c("A", "B", "C"),
#   cell.size = "C",
#   labels="C",
#   maxIteration = 100,
#   debug = FALSE
# )
# drawTreemap(tm)
# 
# 
# read test data set
df <- read.csv("data/Jahn_et_al_CellReports_2018.csv", stringsAsFactors = FALSE) %>%
  subset(condition=="CO2-0-15")


tm <- voronoiTreemap(
  data = df,
  levels = c("Process.abbr", "protein"),
  labels = "protein",
  cell.size = "mean_mass_fraction_norm",
  maxIteration = 100,
  debug = FALSE
)
# drawTreemap(tm)
# 
# 
# library(SysbioTreemaps)
# library(dplyr)
# data("starwars")
# df <- filter(starwars, !(is.na(homeworld) | is.na(gender)))
# 
# 
# tm <- voronoiTreemap(
#   data = df,
#   levels = c("gender","homeworld", "name"),
#   maxIteration = 100,
#   debug = FALSE
# )
# drawTreemap(tm)
