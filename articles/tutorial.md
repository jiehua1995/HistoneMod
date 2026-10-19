# Using HistoneMod

## Overview

`HistoneMod` is primarily intended for interactive use through its
bundled local Shiny application. A typical workflow is:

1.  install the package;
2.  launch the application from R;
3.  upload an `MS1` table and a `Sample` table;
4.  review PCA, heatmap, and barplot outputs;
5.  export figures, tables, or the quick PDF report.

## Installation

Install the package from GitHub with dependencies enabled so required
packages are installed automatically:

``` r

install.packages("remotes")
remotes::install_github("jiehua1995/HistoneMod", dependencies = TRUE)
```

## Launch the application

The recommended entry point for end users is:

``` r

HistoneMod::runHistoneMod()
```

After the application opens, you can either upload your own CSV files or
use the sidebar demo actions:

- `Load Demo Data` loads the packaged example dataset directly into the
  app.
- `Copy Demo Files` copies the packaged CSV files into a local folder
  that you choose.

If you need the Shiny application object itself, you can also use:

``` r

app <- HistoneMod::histonemod_app()
shiny::runApp(app)
```

## Input tables

`HistoneMod` expects two CSV files:

- `MS1` table: should include columns such as `Protein.Name`,
  `Peptide.Note`, `Replicate.Name`, `Total.Area.MS1`, and
  `Isotope.Label.Type`.
- `Sample` table: should include columns such as `Replicate.Name`,
  `Group`, and `Replicate.No`.

Use the help buttons inside the application for concrete format
examples.

## Bundled demo dataset

HistoneMod includes a small packaged demo dataset for testing and
training. These files are derived from the structure of real histone
modification tables, but the sample identifiers were anonymized, numeric
values were randomized, and the content was simplified to retain only
four representative modification states.

You can locate the packaged demo files with:

``` r

system.file("extdata", "MS1_demo.csv", package = "HistoneMod")
system.file("extdata", "samples_demo.csv", package = "HistoneMod")
```

## Typical analysis flow

After launching the application:

1.  upload the `MS1` and `Sample` CSV files in the sidebar, or click
    `Load Demo Data`;
2.  select peptides and samples of interest;
3.  choose whether unmodified peptides should be excluded;
4.  inspect the PCA, heatmap, and barplot tabs;
5.  export plots, the wide-format table, or the quick PDF report.

## Exported analysis helpers

Most users only need the Shiny interface. For scripted workflows,
`HistoneMod` also exports a small set of analysis helpers:

- [`percentage_calculation()`](https://jiehua1995.github.io/HistoneMod/reference/percentage_calculation.md)
- [`plot_pca()`](https://jiehua1995.github.io/HistoneMod/reference/plot_pca.md)
- [`plot_heatmap()`](https://jiehua1995.github.io/HistoneMod/reference/plot_heatmap.md)
- [`plot_barplot_single()`](https://jiehua1995.github.io/HistoneMod/reference/plot_barplot_single.md)

## Next steps

For routine use, start with
[`HistoneMod::runHistoneMod()`](https://jiehua1995.github.io/HistoneMod/reference/runHistoneMod.md).
If you later need to integrate HistoneMod into a larger R analysis
pipeline, the exported calculation and plotting helpers provide a
lightweight programmatic interface.
