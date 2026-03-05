# Assess list of synteny networks as in `assess_synnet`

Assess list of synteny networks as in `assess_synnet`

## Usage

``` r
assess_synnet_list(synnet_list = NULL, cc_type = "average")
```

## Arguments

- synnet_list:

  A list of networks, each network being an edge list as a 2-column data
  frame, with columns 1 and 2 representing names of loci in anchor 1 and
  anchor 2, respectively.

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

- Network:

  Character of network name.

## Examples

``` r
set.seed(123)
data(synnet)
net1 <- synnet
net2 <- synnet[-sample(1:10000, 500), ]
net3 <- synnet[-sample(1:10000, 1000), ]
synnet_list <- list(net1 = net1, net2 = net2, net3 = net3)
assess_synnet_list(synnet_list)
#>          CC Node_count  Rsquared    Score Network
#> 1 0.8779120     149144 0.6806854 89125.76    net1
#> 2 0.8769428     149133 0.6813367 89105.97    net2
#> 3 0.8758974     149114 0.6810978 88957.20    net3
```
