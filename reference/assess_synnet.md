# Assess synteny network based on graph properties

Assess synteny network based on graph properties

## Usage

``` r
assess_synnet(synnet = NULL, cc_type = "average")
```

## Arguments

- synnet:

  Edge list for the synteny network in a 2-column data frame, with
  columns 1 and 2 representing names of loci in anchor 1 and anchor 2,
  respectively.

- cc_type:

  Type of clustering coefficient to be calculated. One of 'global' or
  'average'. Default: 'average'.

## Value

A data frame with the following variables:

- CC:

  Numeric representing clustering coefficient.

- Node_count:

  Numeric representing number of nodes in the network.

- Rsquared:

  Numeric indicating the coefficient of determination for the scale-free
  topology fit.

- Score:

  Numeric representing network score, which is the product of 'CC' and
  'Node_number'.

## Details

Network score is the product of the network's clustering coefficient,
node count, and R squared for the scale-free topology fit.

## Examples

``` r
data(synnet)
assess_synnet(synnet)
#>         CC Node_count  Rsquared    Score
#> 1 0.877912     149144 0.6806854 89125.76
```
