# Plot species-specific duplications

Plot species-specific duplications

## Usage

``` r
plot_duplications(stats_list = NULL)
```

## Arguments

- stats_list:

  A list of data frames with Orthofinder summary stats as returned by
  the function `read_orthofinder_stats`.

## Value

A ggplot object with a barplot of number of species-specific
duplications.

## Examples

``` r
dir <- system.file("extdata", package = "cogeqc")
stats_list <- read_orthofinder_stats(dir)
plot_duplications(stats_list)
```
