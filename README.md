---
title: "Create Voronoi and Sunburst Treemaps from Hierarchical Data"
author: "Michael Jahn"
date: "2019-02-24"
output: github_document
#output: rmarkdown::html_vignette
#vignette: >
#  %\VignetteIndexEntry{}
#  %\VignetteEngine{knitr::rmarkdown}
#  %\VignetteEncoding{UTF-8}
#
# create README.md from R markdown source:
# knitr::knit("vignettes/sysbiotreemaps.Rmd", "README.md")
---




## Description

This package can be used to generate and plot **Voronoi treemaps** or
**Sunburst treemaps** from hierarchical data. 

<img src="vignettes/tm.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="700px" style="display: block; margin: auto;" />

There are different implementations for
Voronoi tesselations in R, the simplest being the deldir() function (from package deldir). However, deldir and other do not handle nested Voronoi tesselations, nor do they perform additively weighted Voronoi tesselation. This is an important demand for systems biology and other applications where one likes to scale the cell size (or area) to a set of predefined weights. The functions provided in this package allow both the additively weighted Voronoi tesselation, and the nesting of different hierarchical levels in one plot. The underlying functions for the tesselation
were developed and published by Paul Murrell, University of Auckland, and serve as the basis for this package. They are called by a recursive wrapper function, voronoiTreemap(), which subdivides the plot area in polygonal cells according to the highest hierarchical level.  It then continues with the tesselation on the next lower level uisng the child cell of the previous level as the new parental plotting cell, and so on. 

The sunburst treemap is a computationally less demanding treemap that does not require iterative refinement, but simply generates circle sectors that are sized according to predefined weights. It is also a arecursive algorithm and can be used to draw sectors of different hierarchical level with increasing granularity.

## Requirements

The C++ code requires the [CGAL](https://www.cgal.org/download.html) library.
For installation in (Ubuntu-like) Linux systems, type in the shell:

```
sudo apt install libcgal-dev
```

## Installation

To install the package directly from github, use this function from devtools package in your R session:

```
require(devtools)
devtools::install_github("https://github.com/m-jahn/SysbioTreemaps")
```
The package is not available on CRAN yet. 

## Usage

Create a simple example data frame

```
library(SysbioTreemaps)

df <- data.frame(
  A=rep(c("a", "b", "c"), each=15),
  B=sample(letters[4:13], 45, replace=TRUE),
  C=sample(1:100, 45)
)
```

Compute the treemap. It will return a list of grid graphics objects.

```
tm <- voronoiTreemap(
  data = data,
  levels = c("A", "B", "C"),
  cell.size = "C",
  cell.color = "A",
  maxIteration = 50,
)
```
Draw the treempap.

```
drawTreemap(tm)
```

<img src="vignettes/tm_small.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="300px" style="display: block; margin: auto;" />

## Adcanced example

Read test data set from Jahn et al., Cell Reports, 2018.
(https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31485-2)

```
df <- Jahn_CellReports_2018 %>%
  subset(condition=="CO2-0-15")
```

Generate a custom color palette using colorspace.

```
library(colorspace); hclwizard()
custom.pal <- sequential_hcl(n = 40, 
  h = c(-100, 100), 
  c = c(60, NA, 66), 
  l = c(42, 100), 
  power = c(2.65, 0.7), 
  rev = TRUE)
```

Generate treemap using some more of the function's parameters. For example, we can supply more than one level for drawing labels, change label colors, encode cell size *and* cell color by a numeric variable, use a custom color palette, and increase maxIterations which will lead to lower errors in some cases. Set a seed if you want the same arrangement of cells every time, otherwise it will be random.


```
tm <- voronoiTreemap(
  data = df,
  levels = c("Process.abbr", "Pathway.abbr", "protein"),
  labels = c("Process.abbr", "protein"),
  label.col = grey(0.7, 0.4),
  cell.size = "mean_mass_fraction_norm",
  cell.color = "mean_mass_fraction_norm",
  maxIteration = 200,
  color.palette = custom.pal, 
  seed = 1
)
```

Draw the treemap.

```
drawTreemap(tm)
```

<img src="vignettes/tm_heatcol.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="700px" style="display: block; margin: auto;" />

## References and other treemap packages


The Voronoi tesselation is based on functions from Paul Murrell, https://www.stat.auckland.ac.nz/~paul/Reports/VoronoiTreemap/voronoiTreeMap.html.
We created a recursive wrapper around the main tesselation function and
improved the stability regarding generation of larger treemaps.

For a similar but JAVA based implementation of Voronoi treemaps wrapped in R, see
David Leslie's scripts at https://github.com/dlesl/voronoi_treemap_rJava.

A Javascript based R package lets you draw simple treemaps in your browser, however,
this is not suitable for treemaps with many (as, hundreds of) cells. The package is
available from CRAN or github, https://github.com/uRosConf/voronoiTreemap.

Another popular resource is the web-based treemap generation from University of
Greifswald at https://bionic-vis.biologie.uni-greifswald.de/.
