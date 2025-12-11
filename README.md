# Shiny App for Histone Post-translational Modifications Data Visualization

This Shiny application provides an interactive platform for **loading, filtering, visualizing, and downloading data**.  
It is designed to help researchers process peptide-level tables exported from skyline and generate common exploratory plots such as PCA, heatmaps, and barplots.

---

## ✨ Features

- **Upload Data**
  - Import table  from Skyline
- **Data Filtering**
  - Select or exclude unmodified peptides.
  - Choose samples or peptides of interest interactively.
- **Visualization**
  - **PCA plot** for dimensionality reduction and sample clustering.
  - **Heatmap** for peptide intensity patterns.
  - **Barplots** for selected peptides.
- **Download**
  - Export filtered tables in wide format.

## 🚀 Quick Start

1. **Open the `app.R` in RStudio.**  

   Make sure `app.R` and `functions.R`, `ui.R`, and `server.R` are in the same directory.

2. **Load required packages.**  

   In the R console, run:

   ```r
   source("functions.R")
   depends_check()
   ```

3. **Run the app**

   Click the **Run App** button in the top-right corner of Rstudio.

## 👥 Authors

- Jie Hua
- Dr. Marco Borso
- Imhof Group