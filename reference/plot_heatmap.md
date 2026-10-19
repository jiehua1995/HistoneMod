# Plot a heatmap

Converts a processed HistoneMod percentage table into a
peptide-by-sample matrix and renders a heatmap with `pheatmap`.

## Usage

``` r
plot_heatmap(
  data_merge,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color_palette = "viridis",
  fontsize = 12,
  fontsize_row = NULL,
  fontsize_col = NULL
)
```

## Arguments

- data_merge:

  A processed data frame containing at least `Replicate.Name`,
  `Peptide.Note`, and `Percentage`.

- cluster_rows:

  Logical; whether peptides should be clustered.

- cluster_cols:

  Logical; whether samples should be clustered.

- color_palette:

  Character palette name. Supported values are `"viridis"`, `"magma"`,
  `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, `"mako"`, and
  `"turbo"`.

- fontsize:

  Base font size passed to `pheatmap`.

- fontsize_row:

  Optional row label font size.

- fontsize_col:

  Optional column label font size.

## Value

A `pheatmap` result object.
