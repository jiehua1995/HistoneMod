# Check and optionally install HistoneMod dependencies

Validates that the packages declared in `DESCRIPTION` are installed and
available to the current R session. If requested, missing packages are
installed with `pak`, which can manage both CRAN and Bioconductor
packages.

## Usage

``` r
depends_check(install_missing = FALSE, attach = FALSE)
```

## Arguments

- install_missing:

  Logical; if `TRUE`, install any missing packages using
  [`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html).

- attach:

  Logical; if `TRUE`, attach the required packages with
  [`library()`](https://rdrr.io/r/base/library.html). The default is
  `FALSE` because HistoneMod imports the required namespaces itself.

## Value

Invisibly returns `TRUE` when all required packages are available after
any optional installation step. Throws an error if packages are still
missing.
