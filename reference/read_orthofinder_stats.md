# Read and parse Orthofinder summary statistics

Read and parse Orthofinder summary statistics

## Usage

``` r
read_orthofinder_stats(stats_dir = NULL)
```

## Arguments

- stats_dir:

  Path to directory containing Orthofinder's comparative genomics
  statistics. In your Orthofinder results directory, this directory is
  named **Comparative_Genomics_Statistics**.

## Value

A list of data frames with the following elements:

1.  **stats** A data frame of summary stats per species with the
    following variables:

    - Species:

      Factor of species names.

    - N_genes:

      Numeric of number of genes.

    - N_genes_in_OGs:

      Numeric of number of genes in orthogroups.

    - Perc_genes_in_OGs:

      Numeric of percentage of genes in orthogroups.

    - N_ssOGs:

      Numeric of number of species-specific orthogroups.

    - N_genes_in_ssOGs:

      Numeric of number of genes in species-specific orthogroups.

    - Perc_genes_in_ssOGs:

      Numeric of percentage of genes in species-specific orthogroups.

    - Dups:

      Integer with number of duplications per species.

2.  **duplications** A 2-column data frame with node IDs in the first
    column and number of gene duplications (50% support) in the second
    column.

## Examples

``` r
stats_dir <- system.file("extdata", package = "cogeqc")
ortho_stats <- read_orthofinder_stats(stats_dir)
```
