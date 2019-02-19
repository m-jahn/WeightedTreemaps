library(SysbioTreemaps)


# TODO include check to coerce factors to strings or not allow factors at all

data <- data.frame(stringsAsFactors = FALSE,
  A=rep(c("a", "b", "c"), each=15),
  B=sample(letters[4:13], 45, replace=TRUE),
  C=sample(1:100, 45)
)

tm <- voronoiTreemap(
  data = data,
  levels = c("A", "B", "C"),
  cell.size = "C",
  cell.color = "A",
  maxIteration = 50,
  debug = FALSE,
  sort=TRUE
)
drawTreemap(tm)


# read test data set
data <- read.csv("data/Jahn_et_al_CellReports_2018.csv", stringsAsFactors = FALSE) %>%
  subset(condition=="CO2-0-15")


tm <- voronoiTreemap(
  data = data, 
  levels = c("Process.abbr", "Pathway.abbr", "protein"), 
  labels = c("Process.abbr", "protein"),
  cell.size = "mean_mass_fraction_norm",
  maxIteration = 50, 
  debug = FALSE
)
drawTreemap(tm)



data("starwars")
sw <- filter(starwars, !(is.na(homeworld) | is.na(gender)))


tm <- voronoiTreemap(
  data = sw,
  levels = "gender",
  maxIteration = 10,
  debug = TRUE
)
drawTreemap(tm, main="star wars characters treemap")
