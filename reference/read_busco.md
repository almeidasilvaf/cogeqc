# Read and parse BUSCO's summary report

Read and parse BUSCO's summary report

## Usage

``` r
read_busco(result_dir = NULL)
```

## Arguments

- result_dir:

  Path to the directory where BUSCO results are stored. This function
  will look for the short_summary\* file (single run) or short_summary\*
  file (batch mode).

## Value

A data frame with the following variables:

- Class:

  BUSCO class. One of **Complete_SC**, **Complete_duplicate**,
  **Fragmented**, or **Missing**

- Frequency:

  Frequency of BUSCOs in each class. If BUSCO was run in batch mode,
  this variable will contain relative frequencies. If BUSCO was run for
  a single file, it will contain absolute frequencies.

- Lineage:

  Name of the lineage dataset used.

- File (batch mode only):

  Name of the input FASTA file.

## Examples

``` r
result_dir <- system.file("extdata", package = "cogeqc")
df <- read_busco(result_dir)
```
