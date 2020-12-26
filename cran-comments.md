## Test environments (via Github Actions)

- windows-latest (release)
- macOS-latest (release)
- ubuntu-20.04 (release)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 3 NOTEs:

1. The C++ function `voronoiDiagram.cpp` does not contain any such entry points (as far as I can see). The Note is probably caused by the upstream dependency CGAL 4 headers (R package `cgal4h`). This note appeas only when checking on Mac OS.

```
File ‘SysbioTreemaps/libs/SysbioTreemaps.so’:
  Found ‘__ZNSt3__14cerrE’, possibly from ‘std::cerr’ (C++)
    Object: ‘voronoiDiagram.o’. Compiled code should not call entry 
    points which might terminate R nor write to stdout/stderr instead
    of to the console, nor use Fortran I/O nor system RNGs.
```

2. Examples take just above 5 seconds on one of the test environments (ubuntu linux), triggering this note.

```
Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
  drawTreemap    5.244  0.184   5.434
  voronoiTreemap 5.093  0.140   5.238
```

3. Installed package size exceeding 5 MB is mainly caused by the compiled function `voronoiDiagram.o`. I am not aware of a method to significantly reduce this size.

```
checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    doc    1.1Mb
    libs   7.7Mb
```

## Downstream dependencies

- There are currently no downstream dependencies for this package