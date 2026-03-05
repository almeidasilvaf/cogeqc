# Plot percentage of genes in orthogroups for each species

Plot percentage of genes in orthogroups for each species

## Usage

``` r
plot_genes_in_ogs(stats_list = NULL)
```

## Arguments

- stats_list:

  A list of data frames with Orthofinder summary stats as returned by
  the function `read_orthofinder_stats`.

## Value

A ggplot object with a barplot of percentages of genes in orthogroups
for each species.

## Examples

``` r
dir <- system.file("extdata", package = "cogeqc")
stats_list <- read_orthofinder_stats(dir)
plot_genes_in_ogs(stats_list)
```
