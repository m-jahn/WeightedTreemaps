SysbioTreemaps
================
Michael Jahn, David Leslie,
2019-06-22

------------------------------------------------------------------------

This package can be used to generate and plot **Voronoi treemaps** or **Sunburst treemaps** from hierarchical data.

<img src="vignettes/tm.png" width="700px" style="display: block; margin: auto;" />

Description
-----------

A Voronoi treemap is a visually appealing graphical representation of numerical data using a space-filling approach. The cells in the map are scaled according to an underlying metric which allows to grasp the hierarchical organization and relative importance of many objects at once. There are different implementations available for Voronoi tesselations in R, the simplest being the `deldir()` function (from package `deldir`). However, `deldir` and others do not handle nested Voronoi tesselations, nor do they perform additively weighted Voronoi tesselation. This is an important demand for systems biology and other applications where it is useful to scale the cell size (or area) to a set of predefined weights. The functions provided in this package allow both the additively weighted Voronoi tesselation, and the nesting of different hierarchical levels in one plot.

The underlying functions for the tesselation were developed and published by Paul Murrell, University of Auckland, and serve as the basis for this package. They are called by a recursive wrapper function, `voronoiTreemap()`, which subdivides the plot area in polygonal cells according to the highest hierarchical level. It then continues with the tesselation on the next lower level uisng the child cell of the previous level as the new parental cell, and so on.

The sunburst treemap is a computationally less demanding treemap that does not require iterative refinement, but simply generates circle sectors that are sized according to predefined weights. It is also a recursive algorithm and can be used to draw sectors of different hierarchical levels with increasing granularity. The documentation for his function is still missing and will be added soon.

Requirements
------------

The C++ code computing the actual Voronoi tesselation requires the [CGAL](https://www.cgal.org/download.html) library. For installation in (Ubuntu-like) Linux systems, open a terminal and execute:

    sudo apt install libcgal-dev

Installation
------------

To install the package directly from github, use this function from the `devtools` package in your R session:

    require(devtools)
    devtools::install_github("https://github.com/m-jahn/SysbioTreemaps")

The package is not available on CRAN yet but it is planned to be deposited soon.

Usage
-----

### Voronoi treemaps

The functions to create Voronoi or Sunburst treemaps take a `data.frame` as main input. The `data.frame` should contain column(s) with numerical or categorical data (i.e. a character vector). Let's Create a simple example data frame (all following examples can also be found in `R/example.R`).

    library(SysbioTreemaps)

    df <- data.frame(
      A = rep(c("a", "b", "c"), each = 15),
      B = sample(letters[4:12], 45, replace = TRUE),
      C = sample(10:100, 45)
    )

Generate the treemap. It will return a list of polygons and metadata. The columns of the data frame that are used as levels of the treemap need to be specified. Different parameters like the initial shape, or the maximum number of iterations are optional.

    tm <- voronoiTreemap(
      data = df,
      levels = c("A", "B", "C"),
      cell_size = "C",
      shape = "rounded_rect"
    )

Draw the treempap.

    drawTreemap(tm)

<img src="vignettes/tm_small.png" width="300px" style="display: block; margin: auto;" />

The `voronoiTreemap()` and `drawTreemap()` functions are separated in order to allow drawing of the same treemap object in different ways. Computation of treemaps with thousands of cells can be very time and resource consuming (around 5-10 minutes for a 2000-cell treemap on a regular desktop computer). With the `drawTreemap()` function, we can not only plot the same treemap in different ways but also combine several treemaps on one page using the `layout` and `position` arguments.

    drawTreemap(tm, title = "treemap 1", 
      color_type = "categorical", color_level = 1, 
      layout = c(1,3), position = c(1, 1))

    drawTreemap(tm, title = "treemap 2",
      color_type = "categorical", color_level = 2, border_size = 3,
      add = TRUE, layout = c(1,3), position = c(1, 2))

    drawTreemap(tm, title = "treemap 3",
      color_type = "cell_size", color_level = 3,
      color_palette = heat.colors(10),
      border_color = grey(0.4), label_color = grey(0.4),
      add = TRUE, layout = c(1,3), position = c(1, 3),
      title_color = "black")

<img src="vignettes/tm_multiple.png" width="800px" style="display: block; margin: auto;" />

### Positioning of cells

Generating a Voronoi treemap is an iterative and somewhat random process. Since the cells 'move' during the iteration process, it can be difficult to control the exact final position of cells. However, there are two ways to influence cell positioning. The first is to use different algorithms for sampling initial coordinates for each cell. The second is simply setting a seed, which will sample the same set of starting coordinates for the same input data. Regarding the `positioning` argument, compare the following three examples where initial positions are 1) random, 2) ordered from top to bottom, or 3) ordered from center to edges.

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

    drawTreemap(tm1, title = "positioning = 'random'", border_size = 3,
      add = TRUE, layout = c(1,3), position = c(1, 1))
    drawTreemap(tm2, title = "positioning = 'regular'", border_size = 3,
      add = TRUE, layout = c(1,3), position = c(1, 2))
    drawTreemap(tm3, title = "positioning = 'clustered'", border_size = 3,
      add = TRUE, layout = c(1,3), position = c(1, 3))

<img src="vignettes/tm_position.png" width="800px" style="display: block; margin: auto;" />

### Adcanced example for Voronoi treemaps

This example will cover the generation of a somewhat larger treemap, as it is often useful to visualize e.g. many genes or proteins at once in molecular biology studies. However, treemaps can be used for any type of data visualizations. First we read a proteomics test data set from Jahn et al., Cell Reports, 2018. (<https://www.cell.com/cell-reports/fulltext/S2211-1247(18)31485-2>) This dataset contains thousands of protein measurements of the cyanobacterium *Synechocystis*.

    library(dplyr)
    library(colorspace)

    df <- Jahn_CellReports_2018 %>%
      filter(condition == "CO2-0-15") %>%
      filter(mean_mass_fraction_norm > 0)

Generate treemap using some more of the function's parameters. We can increase maxIterations and decrease error tolerance which will lead to lower errors. We can set a seed to obtain a similar arrangment of cells for similar maps, otherwise it will be random.

    tm <- voronoiTreemap(
      data = df,
      levels = c("Process.abbr", "Pathway.abbr", "protein"),
      cell_size = "mean_mass_fraction_norm",
      shape = "rectangle",
      error_tol = 0.001,
      maxIteration = 200,
      seed = 13
    )

We can generate custom color palettes using the colorspace wizard. Just browse to the R tab and copy the code to your script.

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

Draw the treemap using some custom graphical parameters.

    drawTreemap(
      tm, 
      color_palette = custom_pal_2,
      color_type = "cell_size",
      color_level = 3,
      label_level = c(1,3),
      label_size = 4,
      label_color = grey(0.5),
      border_color = grey(0.65)
    )

<img src="vignettes/tm_heatcol.png" width="700px" style="display: block; margin: auto;" />

Generate treemaps with parallel computing
-----------------------------------------

Read test data set with proteomics data from 10 different conditions

    library(parallel)

    df <- Jahn_CellReports_2018 %>%
      filter(mean_mass_fraction_norm > 0)

Generate 10 treemaps using the parallel version of lapply, and the 'condition' annotation to subset the data frame. Note that you can adjust the 'mc.cores' parameter to the number of CPUs available on your computer.

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

Draw all 10 treemaps on one canvas using layout and position arguments.

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

<img src="vignettes/tm_parallel.png" width="800px" style="display: block; margin: auto;" />

References and other treemap packages
-------------------------------------

The Voronoi tesselation is based on functions from Paul Murrell, <https://www.stat.auckland.ac.nz/~paul/Reports/VoronoiTreemap/voronoiTreeMap.html>. We created a recursive wrapper around the main tesselation function and improved the stability regarding generation of larger treemaps.

For a similar but JAVA based implementation of Voronoi treemaps wrapped in R, see David Leslie's scripts at <https://github.com/dlesl/voronoi_treemap_rJava>.

A Javascript based R package lets you draw simple treemaps in your browser, however, this is not suitable for treemaps with many (as, hundreds of) cells. The package is available from CRAN or github, <https://github.com/uRosConf/voronoiTreemap>.

Another popular resource is the web-based treemap generation from University of Greifswald at <https://bionic-vis.biologie.uni-greifswald.de/>.
