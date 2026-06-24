# HistoneMod: Histone Post-translational Modifications Quantification Tool

## If you find this repository useful for your research, please consider giving it a star

![Version](https://img.shields.io/badge/version-0.5.1-blue.svg)
[![R](https://img.shields.io/badge/R-%3E%3D%204.1-brightgreen.svg)](https://www.r-project.org/)
![Shiny](https://img.shields.io/badge/framework-Shiny-1f77b4)
![GitHub contributors](https://img.shields.io/github/contributors/jiehua1995/HistoneMod)
![GitHub last commit](https://img.shields.io/github/last-commit/jiehua1995/HistoneMod)
![GitHub repo size](https://img.shields.io/github/repo-size/jiehua1995/HistoneMod)

![Bioinformatics](https://img.shields.io/badge/field-Bioinformatics-green)
![Proteomics](https://img.shields.io/badge/data-Proteomics-orange)
![Histone PTM](https://img.shields.io/badge/analysis-Histone%20PTM-red)
![Maintained](https://img.shields.io/badge/status-actively%20maintained-brightgreen)

`HistoneMod` is an installable R package that provides a Shiny application for **quantitative analysis of histone post-translational modifications (PTMs)**. It is designed for processing, filtering, visualizing, and exporting peptide-level data exported from Skyline.

## Key Features

### Data Upload and Validation
- Import peptide-level tables directly from Skyline in CSV format
- Validate MS1 and sample annotation files before analysis

### Advanced Filtering
- Select peptide modifications of interest
- Filter samples by experimental group or replicate
- Exclude or include unmodified peptides

### Rich Visualizations
- PCA plot for dimensionality reduction and sample clustering
- Heatmap for peptide abundance patterns
- Barplots for individual peptide-level comparisons

### Data Export
- Download filtered datasets in wide-format CSV
- Export plots in multiple formats
- Generate a quick PDF report

### Modern UI
- Clean Shiny interface for interactive exploration

## Installation

`HistoneMod` is not on CRAN yet. Please install it directly from GitHub with `remotes`.

```r
install.packages("remotes")
remotes::install_github("jiehua1995/HistoneMod", subdir = "HistoneMod", dependencies = TRUE)
```

If you already cloned this repository locally, you can also install from the package directory:

```r
remotes::install_local("HistoneMod", dependencies = TRUE)
```

## Launch

After installation, start the app with:

```r
HistoneMod::runHistoneMod()
```

You can also build the application object directly:

```r
app <- HistoneMod::histonemod_app()
shiny::runApp(app)
```

## Input Files

The application expects two CSV files:

1. `MS1 file`
   Contains peptide-level quantitative data with required columns such as:
   `Protein.Name`, `Peptide.Note`, `Replicate.Name`, `Total.Area.MS1`, and `Isotope.Label.Type`.

2. `Sample file`
   Contains sample annotation data with required columns such as:
   `Replicate.Name`, `Group`, and `Replicate.No`.

Use the in-app help buttons for concrete file format examples.

## Authors and Contributors

- **Jie Hua** - Package developer and maintainer
- **Dr. Marco Borso** - Contributor
- **Beyza Bozdag** - Contributor
- **Prof. Dr. Axel Imhof** - Supervisor

**Group**: [Imhof Laboratory](https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/index.html)  
**Institution**: Ludwig Maximilian University of Munich

## Bug Reports and Feature Requests

If you encounter any issues or have suggestions for improvements, please open an issue:

- [GitHub Issues](https://github.com/jiehua1995/HistoneMod/issues)
