# Plot a single-peptide barplot

Summarises peptide percentages by group and renders a single barplot
with optional significance testing.

## Usage

``` r
plot_barplot_single(
  data_merge,
  protein_name,
  peptide_name,
  add_signif = TRUE,
  color_palette = "viridis",
  y_limits = NULL,
  font_scale = 1,
  title_text = NULL
)
```

## Arguments

- data_merge:

  A processed data frame returned by
  [`percentage_calculation()`](https://jiehua1995.github.io/HistoneMod/reference/percentage_calculation.md).

- protein_name:

  Optional protein identifier used to subset the data.

- peptide_name:

  Peptide identifier to plot.

- add_signif:

  Logical; whether to add significance labels.

- color_palette:

  Character palette name. Supported values are `"viridis"`, `"magma"`,
  `"plasma"`, `"inferno"`, `"cividis"`, `"rocket"`, `"mako"`, and
  `"turbo"`.

- y_limits:

  Optional numeric vector of length 2 for the y-axis range.

- font_scale:

  Numeric multiplier used to scale plot text.

- title_text:

  Optional custom plot title.

## Value

A `ggplot` object.
