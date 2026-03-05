# cogeqc

The goal of `cogeqc` is to facilitate systematic quality checks on
standard comparative genomics analyses to help researchers detect issues
and select the most suitable parameters for each data set. Currently,
cogeqc can be used to assess:

1.  **Genome assembly and annotation quality:** using two approaches:

    - *Statistics in a context:* users can extract summary assembly and
      annotation statistics for genomes on NCBI (via the [NCBI Datasets
      API](https://www.ncbi.nlm.nih.gov/datasets/)) and compare their
      observed values (e.g., genome size, number of genes, contiguity
      measures) with previously reported values on NCBI genomes.

    - *Gene space completeness with BUSCOs:* users can assess gene space
      completeness using Best Universal Single-Copy Orthologs (BUSCOs)
      through wrapper functions that run
      [BUSCO](https://doi.org/10.1093/bioinformatics/btv351) from the
      comfort of an R session and create publication-ready plots with
      summary statistics.

2.  **Orthogroup inference:** orthogroups are assessed based on the
    percentage of shared protein domains in all ortogroups. The
    rationale for this approach is that genes in the same orthogroup
    evolved from a common ancestor, so the percentage of conserved
    protein domains in an orthogroup should be as high as possible.

3.  **Synteny detection:** synteny detection is assessed using
    network-based approaches, namely the clustering coefficient and
    degree of a synteny network.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `cogeqc` using from
[Bioconductor](http://bioconductor.org/) the following code:

``` r

if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("cogeqc")
```

And the development version from
[GitHub](https://github.com/almeidasilvaf/cogeqc) with:

``` r

BiocManager::install("almeidasilvaf/cogeqc")
```

## Citation

Below is the citation output from using `citation('cogeqc')` in R.
Please run this yourself to check for any updates on how to cite
**cogeqc**.

``` r

print(citation('cogeqc'), bibtex = TRUE)
#> To cite cogeqc in publications, use:
#> 
#>   Almeida-Silva, F., & Van de Peer, Y. (2023). Assessing the quality of
#>   comparative genomics data and results with the cogeqc R/Bioconductor
#>   package. Methods in Ecology and Evolution, 14, 2942–2952.
#>   https://doi.org/10.1111/2041-210X.14243
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Assessing the quality of comparative genomics data and results with the cogeqc R/Bioconductor package},
#>     author = {Fabricio Almeida-Silva and Yves {Van de Peer}},
#>     journal = {Methods in Ecology and Evolution},
#>     year = {2023},
#>     volume = {14},
#>     number = {12},
#>     pages = {2942-2952},
#>     url = {https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.14243},
#>     doi = {10.1111/2041-210X.14243},
#>   }
```

Please note that the `cogeqc` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `cogeqc` project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*, and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.23/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The [documentation website](http://almeidasilvaf.github.io/cogeqc) is
  automatically updated thanks to
  *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.23/biocthis)*.
