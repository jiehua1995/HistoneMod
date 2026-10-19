# Launch the HistoneMod Shiny application

Convenience wrapper around
[`histonemod_app()`](https://jiehua1995.github.io/HistoneMod/reference/histonemod_app.md)
and [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) for
standard end-user usage.

## Usage

``` r
runHistoneMod(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly returns the value from
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
