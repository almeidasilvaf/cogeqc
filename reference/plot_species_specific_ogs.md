# Plot number of species-specific orthogroups

Plot number of species-specific orthogroups

## Usage

``` r
plot_species_specific_ogs(stats_list = NULL)
```

## Arguments

- stats_list:

  A list of data frames with Orthofinder summary stats as returned by
  the function `read_orthofinder_stats`.

## Value

A ggplot object with a barplot of number of species-specific orthogroups
for each species.

## Examples

``` r
dir <- system.file("extdata", package = "cogeqc")
stats_list <- read_orthofinder_stats(dir)
plot_species_specific_ogs(stats_list)
```
