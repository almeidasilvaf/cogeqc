# cogeqc 0.99.0

NEW FEATURES

* Added a `NEWS.md` file to track changes to the package.

# cogeqc 1.1.3

NEW FEATURES

* Added a correction for overclustering in `calculate_H()` that penalizes
protein domains in multiple orthogroups.
* Updated vignette to provide a detailed explanation of 
how homogeneity scores are calculated.

# cogeqc 1.1.4

NEW FEATURES

* Added option to scale scores by the maximum value

# cogeqc 1.1.7

BUG FIXES

* Variable **Duplications_50** of the `duplications` data frame was not
matching variable **Dups** of the `stats` data frame in the output of
`read_orthofinder_stats()`

NEW FEATURES

* Replaced dispersal formula with a more meaningful and interpretable one.
* Added a *max_size* param to `plot_og_sizes()` to ignore OGs larger
than a specific size.

# cogeqc 1.1.8

CHANGES

* Synteny assessment formula now also considers scale-free topology fit.

BUG FIXES

* Reference-based orthogroup inference does not require the exact same set
of species anymore.

# cogeqc 1.3.1

CHANGES

* Added functions to explore assembly and annotation statistics in a context:
assembly and annotation stats for NCBI genomes can be extracted through the
Datasets API and compared with user-defined values. New functions:
`get_genome_stats()`, `compare_genome_stats()`, and `plot_genome_stats()`.


