# Assessing synteny identification

## Introduction

Synteny analysis allows the identification of conserved gene content and
gene order (collinearity) in a genomic segment, and it is often used to
study how genomic rearrangements have shaped genomes during the course
of evolution. However, accurate detection of syntenic blocks is highly
dependent on parameters such as minimum number of anchors, and maximum
number of upstream and downstream genes to search for syntenic blocks.
Zhao and Schranz (2019) proposed a network-based synteny analysis
(algorithm now implemented in the Bioconductor package
*[syntenet](https://bioconductor.org/packages/3.23/syntenet)*) that
allows the identification of optimal parameters using the network’s
**average clustering coefficient** and **number of nodes**. Here, we
slightly modified the approach to also take into account **how well the
network’s degree distribution fits a scale-free topology**, which is a
typical property of biological networks. This method allows users to
identify the best combination of parameters for synteny detection and
synteny network inference.

## Installation

To install the package from Bioconductor, use the following code:

``` r

if(!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')
BiocManager::install("cogeqc")
```

Loading the package after installtion:

``` r

# Load package after installation
library(cogeqc)
set.seed(123) # for reproducibility
```

## Data description

Here, we will use a subset of the synteny network inferred in Zhao and
Schranz (2019) that contains the synteny network for *Brassica
oleraceae*, *B. napus*, and *B. rapa*.

``` r

# Load synteny network for 
data(synnet)

head(synnet)
#>             anchor1        anchor2
#> 1 bnp_BnaA01g05780D bol_Bo1g011310
#> 2 bnp_BnaA01g05800D bol_Bo1g011320
#> 3 bnp_BnaA01g05810D bol_Bo1g011330
#> 4 bnp_BnaA01g05820D bol_Bo1g011340
#> 5 bnp_BnaA01g05830D bol_Bo1g011350
#> 6 bnp_BnaA01g05840D bol_Bo1g011360
```

## Network-based assessment of synteny identification

To assess synteny detection, we calculate a synteny network score as
follows:

``` math
\begin{aligned}
Score &= C N R^2_{SFT}
\end{aligned}
```

where $`C`$ is the network’s clustering coefficient, $`N`$ is the number
of nodes, and $`R^2_{SFT}`$ is the coefficient of determination for the
scale-free topology fit.

The network with the highest score is considered the most accurate. To
score a network, you will use the function
[`assess_synnet()`](../reference/assess_synnet.md).

``` r

assess_synnet(synnet)
#>         CC Node_count  Rsquared    Score
#> 1 0.877912     149144 0.6806854 89125.76
```

Ideally, you should infer synteny networks using
*[syntenet](https://bioconductor.org/packages/3.23/syntenet)* with
multiple combinations of parameters and assess each network to pick the
best. To demonstrate it, let’s simulate different networks through
resampling and calculate scores for each of them with the wrapper
function [`assess_synnet_list()`](../reference/assess_synnet_list.md).

``` r

# Simulate networks
net1 <- synnet
net2 <- synnet[-sample(1:10000, 500), ]
net3 <- synnet[-sample(1:10000, 1000), ]
synnet_list <- list(
  net1 = net1, 
  net2 = net2, 
  net3 = net3
)

# Assess original network + 2 simulations
synnet_assesment <- assess_synnet_list(synnet_list)
synnet_assesment
#>          CC Node_count  Rsquared    Score Network
#> 1 0.8779120     149144 0.6806854 89125.76    net1
#> 2 0.8769428     149133 0.6813367 89105.97    net2
#> 3 0.8758974     149114 0.6810978 88957.20    net3

# Determine the best network
synnet_assesment$Network[which.max(synnet_assesment$Score)]
#> [1] "net1"
```

As you can see, the first (original) network is the best, as it has the
highest score.

## Session information

This document was created under the following conditions:

``` r

sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value
#>  version  R Under development (unstable) (2026-03-01 r89508)
#>  os       Ubuntu 24.04.4 LTS
#>  system   x86_64, linux-gnu
#>  ui       X11
#>  language en
#>  collate  en_US.UTF-8
#>  ctype    en_US.UTF-8
#>  tz       UTC
#>  date     2026-03-05
#>  pandoc   3.9 @ /usr/bin/ (via rmarkdown)
#>  quarto   1.8.27 @ /usr/local/bin/quarto
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package           * version date (UTC) lib source
#>  ape                 5.8-1   2024-12-16 [1] CRAN (R 4.6.0)
#>  aplot               0.2.9   2025-09-12 [1] CRAN (R 4.6.0)
#>  beeswarm            0.4.0   2021-06-01 [1] CRAN (R 4.6.0)
#>  BiocGenerics        0.57.0  2025-10-30 [1] Bioconductor 3.23 (R 4.6.0)
#>  BiocManager         1.30.27 2025-11-14 [1] CRAN (R 4.6.0)
#>  BiocStyle         * 2.39.0  2025-10-30 [1] Bioconductor 3.23 (R 4.6.0)
#>  Biostrings          2.79.4  2026-01-07 [1] Bioconductor 3.23 (R 4.6.0)
#>  bookdown            0.46    2025-12-05 [1] CRAN (R 4.6.0)
#>  bslib               0.10.0  2026-01-26 [2] CRAN (R 4.6.0)
#>  cachem              1.1.0   2024-05-16 [2] CRAN (R 4.6.0)
#>  cli                 3.6.5   2025-04-23 [2] CRAN (R 4.6.0)
#>  cogeqc            * 1.15.1  2026-03-05 [1] Bioconductor
#>  crayon              1.5.3   2024-06-20 [2] CRAN (R 4.6.0)
#>  desc                1.4.3   2023-12-10 [2] CRAN (R 4.6.0)
#>  digest              0.6.39  2025-11-19 [2] CRAN (R 4.6.0)
#>  dplyr               1.2.0   2026-02-03 [1] CRAN (R 4.6.0)
#>  evaluate            1.0.5   2025-08-27 [2] CRAN (R 4.6.0)
#>  farver              2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
#>  fastmap             1.2.0   2024-05-15 [2] CRAN (R 4.6.0)
#>  fontBitstreamVera   0.1.1   2017-02-01 [1] CRAN (R 4.6.0)
#>  fontLiberation      0.1.0   2016-10-15 [1] CRAN (R 4.6.0)
#>  fontquiver          0.2.1   2017-02-01 [1] CRAN (R 4.6.0)
#>  fs                  1.6.6   2025-04-12 [2] CRAN (R 4.6.0)
#>  gdtools             0.5.0   2026-02-09 [1] CRAN (R 4.6.0)
#>  generics            0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
#>  ggbeeswarm          0.7.3   2025-11-29 [1] CRAN (R 4.6.0)
#>  ggfun               0.2.0   2025-07-15 [1] CRAN (R 4.6.0)
#>  ggiraph             0.9.6   2026-02-21 [1] CRAN (R 4.6.0)
#>  ggplot2             4.0.2   2026-02-03 [1] CRAN (R 4.6.0)
#>  ggplotify           0.1.3   2025-09-20 [1] CRAN (R 4.6.0)
#>  ggtree              4.1.1   2025-10-30 [1] Bioconductor 3.23 (R 4.6.0)
#>  glue                1.8.0   2024-09-30 [2] CRAN (R 4.6.0)
#>  gridGraphics        0.5-1   2020-12-13 [1] CRAN (R 4.6.0)
#>  gtable              0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
#>  htmltools           0.5.9   2025-12-04 [2] CRAN (R 4.6.0)
#>  htmlwidgets         1.6.4   2023-12-06 [2] CRAN (R 4.6.0)
#>  igraph              2.2.2   2026-02-12 [1] CRAN (R 4.6.0)
#>  IRanges             2.45.0  2025-10-31 [1] Bioconductor 3.23 (R 4.6.0)
#>  jquerylib           0.1.4   2021-04-26 [2] CRAN (R 4.6.0)
#>  jsonlite            2.0.0   2025-03-27 [2] CRAN (R 4.6.0)
#>  knitr               1.51    2025-12-20 [2] CRAN (R 4.6.0)
#>  lattice             0.22-9  2026-02-09 [3] CRAN (R 4.6.0)
#>  lazyeval            0.2.2   2019-03-15 [1] CRAN (R 4.6.0)
#>  lifecycle           1.0.5   2026-01-08 [2] CRAN (R 4.6.0)
#>  magrittr            2.0.4   2025-09-12 [2] CRAN (R 4.6.0)
#>  MASS                7.3-65  2025-02-28 [3] CRAN (R 4.6.0)
#>  nlme                3.1-168 2025-03-31 [3] CRAN (R 4.6.0)
#>  otel                0.2.0   2025-08-29 [2] CRAN (R 4.6.0)
#>  patchwork           1.3.2   2025-08-25 [1] CRAN (R 4.6.0)
#>  pillar              1.11.1  2025-09-17 [2] CRAN (R 4.6.0)
#>  pkgconfig           2.0.3   2019-09-22 [2] CRAN (R 4.6.0)
#>  pkgdown             2.2.0   2025-11-06 [1] CRAN (R 4.6.0)
#>  plyr                1.8.9   2023-10-02 [1] CRAN (R 4.6.0)
#>  purrr               1.2.1   2026-01-09 [2] CRAN (R 4.6.0)
#>  R6                  2.6.1   2025-02-15 [2] CRAN (R 4.6.0)
#>  ragg                1.5.0   2025-09-02 [2] CRAN (R 4.6.0)
#>  rappdirs            0.3.4   2026-01-17 [2] CRAN (R 4.6.0)
#>  RColorBrewer        1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
#>  Rcpp                1.1.1   2026-01-10 [2] CRAN (R 4.6.0)
#>  reshape2            1.4.5   2025-11-12 [1] CRAN (R 4.6.0)
#>  rlang               1.1.7   2026-01-09 [2] CRAN (R 4.6.0)
#>  rmarkdown           2.30    2025-09-28 [1] CRAN (R 4.6.0)
#>  S4Vectors           0.49.0  2025-10-30 [1] Bioconductor 3.23 (R 4.6.0)
#>  S7                  0.2.1   2025-11-14 [1] CRAN (R 4.6.0)
#>  sass                0.4.10  2025-04-11 [2] CRAN (R 4.6.0)
#>  scales              1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
#>  Seqinfo             1.1.0   2025-10-31 [1] Bioconductor 3.23 (R 4.6.0)
#>  sessioninfo         1.2.3   2025-02-05 [2] CRAN (R 4.6.0)
#>  stringi             1.8.7   2025-03-27 [2] CRAN (R 4.6.0)
#>  stringr             1.6.0   2025-11-04 [2] CRAN (R 4.6.0)
#>  systemfonts         1.3.1   2025-10-01 [2] CRAN (R 4.6.0)
#>  textshaping         1.0.4   2025-10-10 [2] CRAN (R 4.6.0)
#>  tibble              3.3.1   2026-01-11 [2] CRAN (R 4.6.0)
#>  tidyr               1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
#>  tidyselect          1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
#>  tidytree            0.4.7   2026-01-08 [1] CRAN (R 4.6.0)
#>  treeio              1.35.0  2025-10-30 [1] Bioconductor 3.23 (R 4.6.0)
#>  vctrs               0.7.1   2026-01-23 [2] CRAN (R 4.6.0)
#>  vipor               0.4.7   2023-12-18 [1] CRAN (R 4.6.0)
#>  xfun                0.56    2026-01-18 [2] CRAN (R 4.6.0)
#>  XVector             0.51.0  2025-10-31 [1] Bioconductor 3.23 (R 4.6.0)
#>  yaml                2.3.12  2025-12-10 [2] CRAN (R 4.6.0)
#>  yulab.utils         0.2.4   2026-02-02 [1] CRAN (R 4.6.0)
#> 
#>  [1] /__w/_temp/Library
#>  [2] /usr/local/lib/R/site-library
#>  [3] /usr/local/lib/R/library
#>  * ── Packages attached to the search path.
#> 
#> ──────────────────────────────────────────────────────────────────────────────
```

## References

Zhao, Tao, and M Eric Schranz. 2019. “Network-Based Microsynteny
Analysis Identifies Major Differences and Genomic Outliers in Mammalian
and Angiosperm Genomes.” *Proceedings of the National Academy of
Sciences* 116 (6): 2165–74.
