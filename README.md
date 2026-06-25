# HistoneMod

## ⭐ If you find this repository useful for your research, please consider giving it a star!

[![Version](https://img.shields.io/badge/version-0.6.2-blue.svg)](CHANGELOG.md)
[![R](https://img.shields.io/badge/R-%3E%3D%204.1-brightgreen.svg)](https://www.r-project.org/)
![Shiny](https://img.shields.io/badge/framework-Shiny-1f77b4)
![GitHub last commit](https://img.shields.io/github/last-commit/jiehua1995/HistoneMod)
![GitHub repo size](https://img.shields.io/github/repo-size/jiehua1995/HistoneMod)
<a href="https://doi.org/10.5281/zenodo.20831990"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.20831990.svg" alt="DOI"></a>


`HistoneMod` is an R package for interactive analysis of histone post-translational modifications (PTMs) from Skyline-like peptide-level CSV exports. It provides a local Shiny application for end users together with a small programmatic API for preprocessing and visualization in scripted workflows.

Starting with version `0.6.2`, HistoneMod is distributed as an installable R package rather than as an online hosted Shiny service. The recommended workflow is therefore to install the package once and run the application locally from R.

## Features

- Validates `MS1` and `Sample` input tables before analysis.
- Computes relative peptide abundances across replicates and groups.
- Provides interactive PCA, heatmap, and barplot views in a single Shiny workflow.
- Supports plot export, wide-format table export, and quick PDF reporting.
- Exposes reusable preprocessing and plotting helpers for scripted analyses.

## Installation

Install HistoneMod from GitHub with dependencies enabled:

```r
install.packages("remotes")
remotes::install_github("jiehua1995/HistoneMod", dependencies = TRUE)
```

HistoneMod declares its required runtime dependencies in `DESCRIPTION`, so they are installed automatically as part of package installation.

If you already cloned the repository locally, you can also install from the package directory:

```r
remotes::install_local("HistoneMod", dependencies = TRUE)
```

## Launch the application

For normal use, start the local Shiny application with:

```r
HistoneMod::runHistoneMod()
```

After the app opens, you can either upload your own `MS1` and `Sample` CSV
files or use the built-in demo actions in the sidebar:

- `Load Demo Data` loads the packaged demo dataset directly into the app.
- `Copy Demo Files` copies the packaged demo CSV files into a folder that you
  choose on your local machine.

If you want the Shiny application object itself:

```r
app <- HistoneMod::histonemod_app()
shiny::runApp(app)
```

## Input data

The application expects two comma-separated CSV files. Example layouts are shown below.

### MS1 file example

| Protein Name | Peptide Note | Replicate Name | Total Area MS1 | Isotope Label Type |
| --- | --- | --- | ---: | --- |
| H3_3-8 | H3_3_8_K4_un | SampleA_rep1 | 4320000000 | light |
| H3_3-8 | H3_3_8_K4_me3 | SampleA_rep1 | 1680000000 | light |

### Sample file example

| Replicate Name | Group | Replicate No |
| --- | --- | ---: |
| SampleA_rep1 | SampleA | 1 |
| SampleA_rep2 | SampleA | 2 |

Because HistoneMod uses `read.csv()`, column names with spaces, such as `Protein Name`, are automatically converted by R to dotted names such as `Protein.Name` after import. Skyline exports with space-separated headers are therefore accepted. CSV files produced under different operating systems, language settings, or software versions may vary slightly, but standard comma-separated files are supported as long as the required columns are present.

## Bundled demo data

HistoneMod ships with a small packaged demo dataset for local exploration and
training. The bundled demo data are derived from the structure of real histone
modification result tables, but sample identifiers were anonymized, numeric
values were randomized, and the content was simplified to retain only four
representative histone modification states.

You can access the packaged demo files from R with:

```r
system.file("extdata", "MS1_demo.csv", package = "HistoneMod")
system.file("extdata", "samples_demo.csv", package = "HistoneMod")
```

## Package tutorial

Open the bundled vignette with:

```r
vignette("tutorial", package = "HistoneMod")
```

## Programmatic API

Most users only need the Shiny interface. For scripted analyses, the following functions are exported:

- `depends_check()`
- `histonemod_app()`
- `runHistoneMod()`
- `percentage_calculation()`
- `plot_pca()`
- `plot_heatmap()`
- `plot_barplot_single()`

## Dependency management

`depends_check()` can optionally install missing packages with `pak`, which is useful when you prefer a single dependency manager for packages from both CRAN and Bioconductor:

```r
HistoneMod::depends_check(install_missing = TRUE)
```

## Project information

- **Maintainer**: Jie Hua
- **Contributors**: Marco Borso, Beyza Bozdag
- **Supervisor**: Axel Imhof
- **Research group**: [Imhof Laboratory](https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/index.html)
- **Institution**: Ludwig Maximilian University of Munich

## Support

Please use the issue tracker for bug reports, feature requests, and installation questions:

- [GitHub Issues](https://github.com/jiehua1995/HistoneMod/issues)

## Citation

Jie Hua et al. (2026). jiehua1995/HistoneMod: v0.6.2. Zenodo. https://doi.org/10.5281/zenodo.20831990

[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fjiehua1995%2FHistoneMod.svg?type=shield&issueType=security)](https://app.fossa.com/projects/git%2Bgithub.com%2Fjiehua1995%2FHistoneMod?ref=badge_shield&issueType=security)
