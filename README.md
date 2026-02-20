# HistoneMod: Histone Post-translational Modifications Quantification Tool

[![Version](https://img.shields.io/badge/version-0.4.0-blue.svg)](CHANGELOG.md)
[![R](https://img.shields.io/badge/R-%3E%3D%204.0-brightgreen.svg)](https://www.r-project.org/)

A Shiny application for **quantitative analysis of histone post-translational modifications (PTMs)**. This tool provides researchers with a tool for processing, filtering, visualizing, and analyzing peptide-level data exported from Skyline.

## 🌐 Live Demo

Access the application online via shinyapps.io:

| Version | Features | Link |
|---------|----------|------|
| **Full** | Complete functionality with advanced filtering, customizable visualizations, high-resolution plot exports | [🚀 Launch Full Version](https://jiehua.shinyapps.io/HistoneMod/) |
| **Tiny** | Lightweight version with core features for quick analysis | [⚡ Launch Tiny Version](https://jiehua.shinyapps.io/HistomeModShiny/) |

> **Note**: The full version offers more customization options and features but requires additional computational resources and memory. Due to limit of free shinyapps.io accounts, the app may take some time (more than 3 min) to start if it has been idle or it will be temporarily unavailable during peak times. 
> For best performance, consider running the application locally. If you have to use online version to quickly check the data, please use the tiny version.

## ✨ Key Features

### 📤 Data Upload & Validation
- Import peptide-level tables directly from Skyline (CSV format)

### 🔍 Advanced Filtering
- **Peptide Selection**: Choose specific modifications or peptides of interest
- **Sample Selection**: Filter by experimental groups or individual replicates
- **Unmodified Peptides**: Option to exclude or include unmodified peptides

### 📊 Rich Visualizations
- **PCA Plot**: Dimensionality reduction and sample clustering analysis
- **Heatmap**: Hierarchical clustering of peptide intensity patterns
- **Barplots**: Detailed visualization of individual peptides

### 💾 Data Export
- Download filtered datasets in wide format (CSV)
- Export high-resolution plots (PNG, JPEG, PDF)
- Customizable figure dimensions and resolution

### 🎨 Modern UI/UX
- Clean, responsive design with Tailwind CSS

## 🚀 Quick Start

### Online Usage
Simply visit the [live demo links](#-live-demo) above - no installation required!

### Local Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/jiehua1995/HistoneMod.git
   cd HistoneMod
   ```

2. **Install dependencies**
   
   Choose one of the following methods:
   
   **Option A: Using renv (Recommended)**
   ```r
   # Restore the exact package versions from renv.lock
   renv::restore()
   ```
   
   **Option B: Using the dependency checker**
   ```r
   # Automatically install all required packages
   source("functions.R")
   depends_check()
   ```

3. **Run the application**
   
   Open `app.R` in RStudio and click the **Run App** button, or run:
   ```r
   shiny::runApp()
   ```

**Tip**: Use the built-in help buttons (?) in the application for detailed format examples.

## 👥 Authors & Contributors

- **Jie Hua** – Package developer and maintainer  
- **Dr. Marco Borso** – Contributor (code review / testing)  
- **Beyza Bozdağ** – Contributor (code review / testing)  
- **Prof. Dr. Axel Imhof** – Supervisor

**Group**: [Imhof Laboratory](https://www.molekularbiologie.abi.med.uni-muenchen.de/personen/imhof_group/index.html)  
**Institution**: Ludwig Maximilian University of Munich

## 🐛 Bug Reports & Feature Requests

If you encounter any issues or have suggestions for improvements, please:
1. Check existing [issues](https://github.com/yourusername/HistoneMod/issues)
2. Create a new issue with detailed description
3. Or contact the development team directly

<div align="center">
  
**[⬆ Back to Top](#histonemod-histone-post-translational-modifications-quantification-tool)**

Made with ❤️ by the Imhof Group

</div>