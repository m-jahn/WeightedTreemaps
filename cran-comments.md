## Update

## Test environments

### with Github Actions

- windows-latest (release)
- macOS-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (release)
- ubuntu-latest (oldrel-1)

### with `rhub::rhub_check()`

- MacOS arm64 latest, R-* (any version)
- Windows Server 2022, R-devel, 64 bit
- Ubuntu 22.04.5 LTS, R-devel, GCC 14
- Ubuntu 22.04.5 LTS, R-devel, Clang ASAN/UBSAN

## R CMD check results

There were no ERRORs or WARNINGs.

The Clang ASAN test throws 1 error, "AddressSanitizer: alloc-dealloc-mismatch (operator new vs free)", related to the upstream dependency libgeos. This is most likely a false-positive, see https://github.com/r-hub/rhub/issues/598.

## Downstream dependencies

- There are currently no downstream dependencies for this package
