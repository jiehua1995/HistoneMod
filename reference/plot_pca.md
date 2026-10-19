# Plot PCA scores

Builds a PCA score plot from a processed HistoneMod percentage table.

## Usage

``` r
plot_pca(data_merge, show_ellipse = TRUE, color_palette = "viridis")
```

## Arguments

- data_merge:

  A processed data frame containing at least `Replicate.Name`,
  `Peptide.Note`, and `Percentage`.

- show_ellipse:

  Logical; if `TRUE`, draws 95 percent confidence ellipses for groups
  with enough samples.

- color_palette:

  Character palette name. Supported values are `"viridis"`, `"magma"`,
  `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, `"mako"`, and
  `"turbo"`.

## Value

A `ggplot` object.
