
#----Load data----
data(tree)
file <- system.file("extdata", "Statistics_PerSpecies.tsv", package = "cogeqc")
stats_table <- read_orthofinder_stats(file)


#----Start tests----
test_that("plot_species_tree() returns a ggplot object with a species tree", {
    p <- plot_species_tree(tree)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_genes_in_ogs() returns a ggplot object", {
    p <- plot_genes_in_ogs(stats_table)
    expect_true("ggplot" %in% class(p))
})
