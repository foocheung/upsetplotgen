# upsetplotgen




https://github.com/user-attachments/assets/d3faa0fa-e98c-483e-b6da-35542a39b0dd



<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `upsetplotgen` package provides a Shiny application for generating UpSet plots from CSV data with comprehensive intersection analysis and data export capabilities.

## Installation

You can install the development version of upsetplotgen from GitHub:

```r
# install.packages("devtools")
devtools::install_github("yourusername/upsetplotgen")
```

## Usage

To run the Shiny application:

```r
library(upsetplotgen)
run_app()
```

## Features

- **Interactive UpSet Plot Generation**: Create publication-ready UpSet plots from your CSV data
- **Data Upload**: Support for CSV file uploads with customizable headers
- **Column Selection**: Interactive selection of identifier and grouping columns
- **Intersection Analysis**: Comprehensive analysis of set intersections with detailed statistics
- **Data Export**: Download intersection sets as CSV files and binary matrices
- **Customizable Plots**: Adjust plot titles, colors, and styling options
- **Data Preview**: Built-in data preview and validation tools
- **Binary Matrix Generation**: Create and export binary matrices for further analysis

## Data Format

The application expects CSV data with at least two columns:
- **Identifier Column**: Unique identifiers for your data points (e.g., gene names, sample IDs)
- **Grouping Column**: Categories or datasets that define the sets for intersection analysis

Example data structure:
```csv
junction_aa,Dataset
ITEM001,Dataset_A
ITEM002,Dataset_A
ITEM001,Dataset_B
ITEM003,Dataset_C
```

## Workflow

1. **Upload Data**: Upload your CSV file using the file input
2. **Configure Columns**: Load and select the appropriate identifier and grouping columns
3. **Customize Settings**: Set plot title, color scheme, and processing options
4. **Generate Plot**: Create the UpSet plot with intersection analysis
5. **Explore Results**: View the plot, intersection statistics, and data previews
6. **Export Data**: Download intersection sets and binary matrices for further analysis

## Output Files

The application generates several output files:

- **Individual Set Files**: CSV files for each individual set (`set_[SetName].csv`)
- **Intersection Files**: CSV files for each intersection combination (`intersection_[Number]_[SetNames].csv`)
- **Binary Matrix**: Complete binary matrix showing set membership (`binary_matrix.csv`)
- **ZIP Archive**: Compressed file containing all generated files

## Package Structure

This package is built using the [Golem](https://thinkr-open.github.io/golem/) framework for production-grade Shiny applications. The main components include:

- `run_app()`: Main function to launch the application
- `make_upset_plot_from_data()`: Core function for generating UpSet plots
- `create_zip_file()`: Utility function for creating downloadable archives

## Dependencies

The package requires the following R packages:
- `shiny`: Web application framework
- `ComplexHeatmap`: For generating UpSet plots
- `dplyr` & `tidyr`: Data manipulation
- `DT`: Interactive data tables
- `readr`: Data import
- `zip`: File compression
- `golem`: Application framework

## Development

To set up the development environment:

```r
# Clone the repository
# Navigate to the package directory

# Install dependencies
devtools::install_deps()

# Load the package for development
devtools::load_all()

# Run the app in development mode
run_app()
```
