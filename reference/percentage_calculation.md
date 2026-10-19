# Calculate relative peptide percentages

Merges MS1 and sample tables, keeps light peptides, computes
within-protein relative abundances, and optionally filters unmodified
peptides, peptide selections, and sample selections.

## Usage

``` r
percentage_calculation(
  ms1,
  sample,
  exclude_un = TRUE,
  selected_peptides = NULL,
  selected_samples = NULL,
  max_percentage = 100
)
```

## Arguments

- ms1:

  A data frame containing peptide-level MS1 measurements.

- sample:

  A data frame containing sample metadata.

- exclude_un:

  Logical; if `TRUE`, exclude peptide names ending in `"un"` or
  `"unun"`.

- selected_peptides:

  Optional character vector of peptide names to keep.

- selected_samples:

  Optional character vector of sample names to keep.

- max_percentage:

  Numeric upper bound used to filter implausible percentage values.

## Value

A processed data frame with merged metadata and a `Percentage` column.
