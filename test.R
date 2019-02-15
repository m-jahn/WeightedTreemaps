# read test data set
# data <- read.csv("data/Jahn_et_al_CellReports_2018.csv", stringsAsFactors = FALSE) %>%
#   subset(condition=="CO2-0-15") %>%
#   arrange(Process.abbr, Pathway.abbr, protein)
# 
devtools::load_all()

library(grid)
library(dplyr)
library(tidyr)
library(gpclib)
library(soiltexture)
library(sp)
# 
source("R/allocate.R")
source("R/draw.R")
source("R/drawUtils.R")
source("R/tesselation.R")
source("R/voronoi.R")

library(dplyr)
data <- tibble(
  A=rep(c("a", "b", "c"), each=15),
  B=sample(letters[4:13], 45, replace=TRUE),
  C=sample(20:100, 45)
) %>% arrange(A, B, C)
 

tm <- voronoiTreemap(
  data,
  levels = c("A", "B"),
  maxIteration = 3, debug = TRUE
)

drawTreemap(tm)

data("starwars")

sw <- filter(starwars, !(is.na(homeworld) | is.na(gender))) %>%
  arrange(gender, homeworld, name)


tm <- voronoiTreemap(
  sw,
  levels = c("gender", "homeworld"),
  maxIteration = 5,
  labels = c("gender", "homeworld"),
  cell.size = "height",
  debug = TRUE
)

drawTreemap(tm, main="star wars characters treemap")
