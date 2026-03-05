# Get summary statistics for genomes on NCBI using the NCBI Datasets API

Get summary statistics for genomes on NCBI using the NCBI Datasets API

## Usage

``` r
get_genome_stats(taxon = NULL, filters = NULL)
```

## Arguments

- taxon:

  Taxon for which summary statistics will be retrieved, either as a
  character scalar (e.g., "brassicaceae") or as a numeric scalar
  representing NCBI Taxonomy ID (e.g., 3700).

- filters:

  (optional) A list of filters to use when querying the API in the form
  of key-value pairs, with keys in list names and values in list
  elements (e.g., `list(filters.reference_only = "true")`, see examples
  for details).

## Value

A data frame with the following variables:

- accession:

  character, accession number.

- source:

  character, data source.

- species_taxid:

  numeric, NCBI Taxonomy ID.

- species_name:

  character, species' scientific name.

- species_common_name:

  character, species' common name.

- species_ecotype:

  character, species' ecotype.

- species_strain:

  character, species' strain.

- species_isolate:

  character, species' isolate.

- species_cultivar:

  character, species' cultivar.

- assembly_level:

  factor, assembly level ("Complete", "Chromosome", "Scaffold", or
  "Contig").

- assembly_status:

  character, assembly status.

- assembly_name:

  character, assembly name.

- assembly_type:

  character, assembly type.

- submission_date:

  character, submission date (YYYY-MM-DD).

- submitter:

  character, submitter name.

- sequencing_technology:

  character, sequencing technology.

- atypical:

  logical, indicator of wheter the genome is atypical.

- refseq_category:

  character, RefSeq category.

- chromosome_count:

  numeric, number of chromosomes.

- sequence_length:

  numeric, total sequence length.

- ungapped_length:

  numeric, ungapped sequence length.

- contig_count:

  numeric, number of contigs.

- contig_N50:

  numeric, contig N50.

- contig_L50:

  numeric, contig L50.

- scaffold_N50:

  numeric, contig N50.

- scaffold_L50:

  numeric, contig L50.

- GC_percent:

  numeric, GC percentage (0-100).

- annotation_provider:

  character, name of annotation provider.

- annotation_release_date:

  character, annotation release date (YYYY-MM-DD).

- gene_count_total:

  numeric, total number of genes.

- gene_count_coding:

  numeric, number of protein-coding genes.

- gene_count_noncoding:

  numeric, number of non-coding genes.

- gene_count_pseudogene:

  numeric, number of pseudogenes.

- gene_count_other:

  numeric, number of other genes.

- CC_ratio:

  numeric, ratio of the number of contigs to the number of chromosomes.

## Details

Possible filters for the **filters** parameter can be accessed at
https://www.ncbi.nlm.nih.gov/datasets/docs/v2/reference-docs/rest-api/#get-/genome/taxon/-taxons-/dataset_report.

## Examples

``` r
# Example 1: Search for A. thaliana genomes by tax ID
ex1 <- get_genome_stats(taxon = 3702)

# Example 2: Search for A. thaliana genomes by name
ex2 <- get_genome_stats(taxon = "Arabidopsis thaliana")

# Example 3: Search for chromosome-level Brassicaeae genomes
ex3 <- get_genome_stats(
    taxon = "brassicaceae",
    filters = list(filters.assembly_level = "chromosome")
)
```
