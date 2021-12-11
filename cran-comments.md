## Resubmission

This package was submitted previously, but returned with the following comments:

1. `Package has a FOSS license but eventually depends on the following package which restricts use: gpclib`. Pls explain how it works.

2. Please omit `+ file LICENSE` and the file itself which is part of R anyway. It is only used to specify additional restrictions to the GPL such as attribution requirements.

Actions taken:

1. The `gpclib` dependency was removed. The respective functions from `gpclib` were replaced with similar functions from the newer `sf` package

2. The license file and link in DESCRIPTION was removed.


## Test environments (via Github Actions)

- windows-latest (release)
- macOS-latest (release)
- ubuntu-20.04 (release)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 3 NOTEs:

1. Note:

```
File ‘WeightedTreemaps/libs/WeightedTreemaps.so’:
  Found ‘__ZNSt3__14cerrE’, possibly from ‘std::cerr’ (C++)
    Object: ‘voronoiDiagram.o’. Compiled code should not call entry 
    points which might terminate R nor write to stdout/stderr instead
    of to the console, nor use Fortran I/O nor system RNGs.
```

The C++ function `voronoiDiagram.cpp` does not contain any such entry points. This Note is caused by the upstream dependency CGAL 4 headers (R package `cgal4h`). This note appeas only when checking on Mac OS.

2. Note:

```
Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
  drawTreemap    5.244  0.184   5.434
  voronoiTreemap 5.093  0.140   5.238
```

Examples take just above 5 seconds on one of the test environments (ubuntu linux), triggering this note.

3. Note:

```
checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    doc    1.1Mb
    libs   7.7Mb
```

Installed package size exceeding 5 MB is mainly caused by the compiled function `voronoiDiagram.o`. The size of this function can not be easily reduced.

## Downstream dependencies

- There are currently no downstream dependencies for this package
