# Number of shared orthogroups between model organisms

Orthogroups from a set of model organisms obtained from OrthoFinder2'
example output, which was then passed to
[`get_og_overlap()`](get_og_overlap.md) to get shared orthogroups.

## Usage

``` r
data(og_overlap_model)
```

## Format

A 3-column data frame with the following variables:

- sp1:

  Character, name of species 1.

- sp2:

  Character, name of species 2.

- og_overlap:

  Numeric, number of shared orthogroups.

## References

Emms, D. M., & Kelly, S. (2019). OrthoFinder: phylogenetic orthology
inference for comparative genomics. Genome biology, 20(1), 1-14.

## Examples

``` r
data(og_overlap_model)
```
