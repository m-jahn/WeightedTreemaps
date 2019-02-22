library(SysbioTreemaps)

df <- data.frame(#stringsAsFactors = FALSE,
  A=rep(c("a", "b", "c"), each=15),
  B=sample(letters[4:13], 45, replace=TRUE),
  C=sample(1:100, 45)
)
#apply(df, 2, as.character)

tm <- voronoiTreemap(
  data = df,
  levels = c("A", "B", "C"),
  cell.size = "C",
  label.col = grey(1, alpha = 0.5),
  labels=c("A", "C"),
  maxIteration = 5
)
drawTreemap(tm)


# read test data set
df <- read.csv("data/Jahn_et_al_CellReports_2018.csv", stringsAsFactors = FALSE) %>%
  subset(condition=="CO2-0-15")


tm <- voronoiTreemap(
  data = df,
  levels = c("Process.abbr", "Pathway.abbr", "protein"),
  labels = c("Process.abbr", "protein"),
  cell.size = "mean_mass_fraction_norm",
  maxIteration = 50,
  seed = 123
)
drawTreemap(tm)
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
