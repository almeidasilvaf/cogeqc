# Run BUSCO assessment of assembly and annotation quality

Run BUSCO assessment of assembly and annotation quality

## Usage

``` r
run_busco(
  sequence = NULL,
  outlabel = NULL,
  mode = c("genome", "transcriptome", "proteins"),
  lineage = NULL,
  auto_lineage = NULL,
  force = FALSE,
  threads = 1,
  outpath = NULL,
  download_path = tempdir()
)
```

## Arguments

- sequence:

  An object of class DNAStringSet/AAStringSet/RNAStringSet or path to
  FASTA file with the genome, transcriptome, or protein sequences to be
  analyzed. If there are many FASTA files in a directory, you can input
  the path to this directory, so BUSCO will be run in all FASTA files
  inside it.

- outlabel:

  Character with a recognizable short label for analysis directory and
  files.

- mode:

  Character with BUSCO mode. One of 'genome', 'transcriptome', or
  'proteins'.

- lineage:

  Character with name of lineage to be used.

- auto_lineage:

  Character indicating whether BUSCO should determine optimum lineage
  path automatically. One of 'euk', 'prok', 'all', or NULL. If 'euk', it
  will determine optimum lineage path on eukaryote tree. If 'prok', it
  will determine optimum lineage path on non-eukaryote trees. If 'all',
  it will determine optimum lineage path for all trees. If NULL, it will
  not automatically determine lineage, and *lineage* must be manually
  specified. Default: NULL.

- force:

  Logical indicating whether existing runs with the same file names
  should be overwritten. Default: FALSE.

- threads:

  Numeric with the number of threads/cores to use. Default: 1.

- outpath:

  Path to results directory. If NULL, results will be stored in the
  current working directory. Default: NULL.

- download_path:

  Path to directory where BUSCO datasets will be stored after
  downloading. Default: tempdir().

## Value

A character vector with the names of subdirectories and files in the
results directory.

## Examples

``` r
# \donttest{
sequence <- system.file("extdata", "Hse_subset.fa", package = "cogeqc")
download_path <- paste0(tempdir(), "/datasets")
if(busco_is_installed()) {
    run_busco(sequence, outlabel = "Hse", mode = "genome",
              lineage = "burkholderiales_odb10",
              outpath = tempdir(), download_path = download_path)
}
# }
```
