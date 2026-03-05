# Goodness of fit test for the scale-free topology model

Goodness of fit test for the scale-free topology model

## Usage

``` r
fit_sft(edges)
```

## Arguments

- edges:

  A 2-column data frame with network edges represented in each. Columns
  1 and 2 represent nodes 1 and 2 of each edge.

## Value

A numeric scalar with the R squared for the scale-free topology fit.

## Examples

``` r
data(synnet)
edges <- synnet
fit_sft(edges)
#> [1] 0.6806854
```
