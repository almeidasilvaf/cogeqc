# Compare user-defined assembly statistics with statistics of NCBI genomes

This function helps users analyze their genome assembly stats in a
context by comparing metrics obtained by users with "reference" metrics
in closely-related organisms.

## Usage

``` r
compare_genome_stats(ncbi_stats = NULL, user_stats = NULL)
```

## Arguments

- ncbi_stats:

  A data frame of summary statistics for a particular taxon obtained
  from the NCBI, as obtained with the function `get_genome_stats`.

- user_stats:

  A data frame with assembly statistics obtained by the user. A column
  named **accession** is mandatory, and it must contain unique
  identifiers for the genome(s) analyzed by the user. Dummy variables
  can be used as identifiers (e.g., "my_genome_001"), as long as they
  are unique. All other column containing assembly stats must have the
  same names as their corresponding columns in the data frame specified
  in **ncbi_stats**. For instance, stats on total number of genes and
  sequence length must be in columns named "gene_count_total" and
  "sequence_length", as in the **ncbi_stats** data frame.

## Value

A data frame with the following variables:

- accession:

  character, unique identifier as in user_stats\$accession.

- variable:

  character, name of the genome assembly metric (e.g., "CC_ratio").

- percentile:

  numeric, percentile in the distribution.

- rank:

  numeric, rank in the distribution (highest to lowest). For the
  variable "CC_ratio", ranks go from lowest to highest.

## Details

For each genome assembly statistic (e.g., "gene_count_total"), values in
**user_stats** are compared to a distribution of values from
**ncbi_stats**, and their percentile and rank in the distributions are
reported.

## Examples

``` r
# Use case: user assembled a maize (Zea mays) genome

## Obtain stats for maize genomes on the NCBI
ncbi_stats <- get_genome_stats(taxon = "Zea mays")

## Create a data frame of stats for fictional maize genome
user_stats <- data.frame(
    accession = "my_lovely_maize",
    sequence_length = 2.4 * 1e9,
    gene_count_total = 50000,
    CC_ratio = 1
)

# Compare stats
compare_genome_stats(ncbi_stats, user_stats)
#>         accession         variable percentile rank
#> 1 my_lovely_maize  sequence_length 0.96875000  7.0
#> 2 my_lovely_maize gene_count_total 1.00000000  1.0
#> 3 my_lovely_maize         CC_ratio 0.01680672  1.5
```
