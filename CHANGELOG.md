# Changelog

All notable changes to the HistoneMod Shiny application will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [0.3.0] - 2025-12-15

### ✨ Added
- **File Validation System**: Implemented robust file validation for MS1 and Sample CSV files
  - Real-time validation feedback with visual status indicators
  - Detailed error messages for missing columns or invalid data
  - Preview functionality for uploaded files (first 100 rows for MS1, full Sample file)
- **Modern UI Enhancements**: 
  - Integrated Tailwind CSS for responsive, modern design
  - Added confetti celebration effects for successful operations
  - Improved visual hierarchy with card-based layouts
  - Enhanced color schemes and typography
- **Interactive Help System**: Added contextual help buttons (question mark icons) with detailed tooltips for:
  - MS1 file format requirements
  - Sample file structure
  - Peptide selection options
  - Sample filtering options
  - Exclude unmodified peptides feature
- **Color Palette Options**: Added customizable color palettes for all visualizations:
  - PCA plots: viridis, plasma, magma, inferno, turbo
  - Heatmaps: viridis, RdYlBu, RdBu, viridis, magma, inferno
  - Barplots: viridis, plasma, cividis, mako, rocket
- **Enhanced Plot Export**: Improved plot download functionality with high-resolution outputs
  - Floating download buttons on each plot panel
  - Multiple format support with optimal quality settings

### 🔧 Changed
- **Code Organization**: Restructured application architecture for better maintainability
  - Moved dependency checking to `ui.R` for proper initialization
  - Removed redundant `depends_check()` calls from `server.R`
  - Streamlined `app.R` with clearer setup instructions
- **Package Management**: Updated and optimized dependency list
  - Removed unnecessary packages (car, cowplot)
  - Added `shinyWidgets` for enhanced UI components
  - Updated `renv.lock` with latest package versions
- **File Upload Workflow**: Improved data upload process
  - Better error handling and user feedback
  - Clearer instructions for required file formats
  - Sample data examples embedded in help dialogs

### 🐛 Fixed
- Fixed namespace issues with `shinyWidgets::pickerInput()` calls
- Improved error handling for edge cases in data processing
- Enhanced stability of reactive data flow in server logic

### 📚 Documentation
- Updated README.md with:
  - Clear distinction between Tiny and Full versions
  - Improved feature descriptions
  - Added Beyza Bozdağ to authors list
  - Refined Quick Start instructions
- Added comprehensive inline code comments
- Improved UI labels and helper text throughout the application

### 🎨 UI/UX Improvements
- Redesigned sidebar with collapsible sections
- Enhanced validation status display with color-coded badges
- Improved plot containers with consistent styling
- Added footer with team links and copyright information
- Better responsive layout for different screen sizes

---

## [0.2.0] - 2024

### Added
- Initial public release with core functionality
- PCA visualization
- Heatmap generation
- Barplot for peptide analysis
- Data filtering and export capabilities
- Deployment to shinyapps.io

---

## [0.1.0] - 2024

### Added
- Initial development version
- Basic Shiny app structure
- Core data processing functions
- Skyline data import functionality
