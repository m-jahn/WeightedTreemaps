library(WeightedTreemaps)

df <- data.frame(
  words = rep(c("Weighted", "Treemaps"), each = 8),
  letters = strsplit("WeightedTreemaps", split = "")[[1]],
  weights = rnorm(16, 3, 0.5)
)

tm <- voronoiTreemap(
  df, levels = c("words", "letters"),
  cell_size = "weights",
  shape = "hexagon",
  positioning = "clustered_by_area"
)

svg(filename = "images/svg/logo.svg", width = 5, height = 4.7)
drawTreemap(tm, color_type = "both", border_size = 10)
dev.off()