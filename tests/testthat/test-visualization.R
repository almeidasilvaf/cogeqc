
#----Load data----
data(tree)
data(og)
dir <- system.file("extdata", package = "cogeqc")
stats_list <- read_orthofinder_stats(dir)


#----Start tests----
test_that("plot_species_tree() returns a ggplot object with a species tree", {
    p <- plot_species_tree(tree)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_genes_in_ogs() returns a ggplot object", {
    p <- plot_genes_in_ogs(stats_list)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_species_specific_ogs() returns a ggplot object", {
    p <- plot_species_specific_ogs(stats_list)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_duplications() returns a ggplot object", {
    p <- plot_duplications(stats_list)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_orthofinder_stats() returns a ggplot object", {
    p <- plot_orthofinder_stats(tree, stats_list)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_og_overlap() returns a ggplot object", {
    p <- plot_og_overlap(stats_list)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_og_sizes() returns a ggplot object", {
    p <- plot_og_sizes(og, log = TRUE)
    expect_true("ggplot" %in% class(p))
})

