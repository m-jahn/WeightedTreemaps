## Resubmission

This package was submitted previously, but returned with the following comments:

1. "If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file ..."

-> one reference was added to the DESCRIPTION.

2. "Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) ... Missing Rd-tags: poly_sortpoints.Rd: \value"

-> the documentation in \value fields was extended for several functions.
The function with a missing \value was removed.

3. "You write information messages to the console that cannot be easily
suppressed. It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning() ..."

-> all cat() and print() statements were replaced by message(). Messages are
now only printed when the user specifies verbose = TRUE.

## Test environments

### with Github Actions

- windows-latest (release)
- macOS-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (release)
- ubuntu-latest (oldrel-1)


### with `rhub::check_for_cran()`

- Fedora Linux, R-devel, clang, gfortran
- Debian Linux, R-release, GCC
- Windows Server 2022, R-devel, 64 bit
- Ubuntu Linux 20.04.1 LTS, R-release, GCC

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

The C++ function `voronoiDiagram.cpp` does not contain any such entry points. This Note occasionally turns up on Mac OS tests, probably caused by the upstream dependency CGAL headers (R package `RcppCGAL`) or boost headers (`BH`).

2. Note:

```
checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    doc    1.1Mb
    libs   7.7Mb
```

Installed package size exceeding 5 MB is caused by the compiled function `voronoiDiagram.o`. The size of this file can not be reduced.

## Downstream dependencies

- There are currently no downstream dependencies for this package
