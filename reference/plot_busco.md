# Plot BUSCO summary output

Plot BUSCO summary output

## Usage

``` r
plot_busco(summary_df = NULL)
```

## Arguments

- summary_df:

  Data frame with BUSCO summary output as returned by
  [`read_busco()`](read_busco.md).

## Value

A ggplot object with a barplot of BUSCOs in each class.

## Examples

``` r
# Single file
result_dir <- system.file("extdata", package = "cogeqc")
summary_df <- read_busco(result_dir)
# Batch mode
data(batch_summary)
plot_busco(summary_df)

plot_busco(batch_summary)
```
