library(dplyr)
library(SysbioTreemaps)


data <- tibble(
  A=rep(c("a", "b", "c"), each=15),
  B=sample(letters[4:13], 45, replace=TRUE),
  C=sample(20:100, 45)
) %>% arrange(A, B, C)
 

tm <- voronoiTreemap(
  data,
  levels = c("A", "B", "C"),
  cell.size = "C",
  maxIteration = 50,
  debug = FALSE
)
drawTreemap(tm)


# read test data set
data <- read.csv("data/Jahn_et_al_CellReports_2018.csv", stringsAsFactors = FALSE) %>%
  subset(condition=="CO2-0-15") %>%
  arrange(Process.abbr, Pathway.abbr, protein)

tm <- voronoiTreemap(
  filter(data, mean_mass_fraction_norm > 0.0003), 
  levels = c("Process.abbr", "protein"), #"Pathway.abbr"
  labels = c("Process.abbr", "protein"),
  cell.size = "mean_mass_fraction_norm",
  maxIteration = 50, 
  debug = FALSE
)
drawTreemap(tm)



# data("starwars")
# sw <- filter(starwars, !(is.na(homeworld) | is.na(gender)))
# 
# 
# tm <- voronoiTreemap(
#   as.data.frame(sw), 
#   levels = c("gender", "homeworld", "name"),
#   maxIteration = 20,
#   labels = c("name"),
#   debug = FALSE
# )
# 
# drawTreemap(tm, main="star wars characters treemap")