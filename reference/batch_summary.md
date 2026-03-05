# BUSCO summary output for batch mode

This object was created with the function
[`read_busco()`](read_busco.md) using a batch run of BUSCO on the
genomes of Herbaspirillum seropedicae SmR1 and Herbaspirillum
rubrisubalbicans M1.

## Usage

``` r
data(batch_summary)
```

## Format

A 2-column data frame with the following variables:

- Class:

  Factor of BUSCO classes

- Frequency:

  Numeric with the percentage of BUSCOs in each class.

- Lineage:

  Character with the lineage dataset used.

- File:

  Character with the name of the FASTA file used.

## Examples

``` r
data(batch_summary)
```
