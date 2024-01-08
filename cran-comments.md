## Update

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
- Debian Linux, R-devel, GCC ASAN/UBSAN

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

```
checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    doc    1.1Mb
    libs   7.7Mb
```

Installed package size exceeding 5 MB is caused by the compiled function `voronoiDiagram.o`. The size of this file can not be reduced.

There were "additional issues" brought up with from ASAN/UBSAN sanitizer checks.
These were fixed in the current version.

## Downstream dependencies

- There are currently no downstream dependencies for this package
