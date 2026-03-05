# Plot species tree

Plot species tree

## Usage

``` r
plot_species_tree(tree = NULL, xlim = c(0, 1), stats_list = NULL)
```

## Arguments

- tree:

  Tree object as returned by `treeio::read.*`, a family of functions in
  the **treeio** package to import tree files in multiple formats, such
  as Newick, Phylip, NEXUS, and others. If your species tree was
  inferred with Orthofinder (using STAG), the tree file is located in
  *Species_Tree/SpeciesTree_rooted_node_labels.txt*. Then, it can be
  imported with `treeio::read_tree(path_to_file)`.

- xlim:

  Numeric vector of x-axis limits. This is useful if your node tip
  labels are not visible due to margin issues. Default: c(0, 1).

- stats_list:

  (optional) A list of data frames with Orthofinder summary stats as
  returned by the function `read_orthofinder_stats`. If this list is
  given as input, nodes will be labeled with the number of duplications.

## Value

A ggtree/ggplot object with the species tree.

## Examples

``` r
data(tree)
plot_species_tree(tree)
```
