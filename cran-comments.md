## Resubmission

This package was submitted previously, but returned with the following comments:

1. "If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file ..."

Action: One reference was added to the DESCRIPTION.

2. "Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) ... Missing Rd-tags: poly_sortpoints.Rd: \value"

Action: The documentation in \value fields was extended for several functions.
The function with missing \value was not required and therefore removed.

3. "You write information messages to the console that cannot be easily
suppressed. It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning() ..."

Action: All cat() and print() statements were replaced by message(). Messages are
now predominantly printed when the user specifies verbose = TRUE.

## Test environments (via Github Actions)

- windows-latest (release)
- macOS-latest (release)
- ubuntu-20.04 (release)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

1. Note:

```
File ‘WeightedTreemaps/libs/WeightedTreemaps.so’:
  Found ‘__ZNSt3__14cerrE’, possibly from ‘std::cerr’ (C++)
    Object: ‘voronoiDiagram.o’. Compiled code should not call entry 
    points which might terminate R nor write to stdout/stderr instead
    of to the console, nor use Fortran I/O nor system RNGs.
```

The C++ function `voronoiDiagram.cpp` does not contain any such entry points. This Note is caused by the upstream dependency CGAL 4 headers (R package `cgal4h`). This note appears only when checking on Mac OS.

2. Note:

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
