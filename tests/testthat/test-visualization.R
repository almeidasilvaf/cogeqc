
#----Load data------------------------------------------------------------------
data(tree)
data(og)
dir <- system.file("extdata", package = "cogeqc")
stats_list <- read_orthofinder_stats(dir)


#----Start tests----------------------------------------------------------------
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
    expect_error(plot_orthofinder_stats())
})

test_that("plot_og_overlap() returns a ggplot object", {
    p <- plot_og_overlap(stats_list)
    expect_true("ggplot" %in% class(p))
})

test_that("plot_og_sizes() returns a ggplot object", {
    p <- plot_og_sizes(og, log = TRUE)
    p2 <- plot_og_sizes(og, log = FALSE)

    # Create fake orthogroup data frame with >20 species for testing
    og_sim <- data.frame(
        Orthogroup = paste0("OG", 0001:0005),
        Species = rep(paste0("sp", 1:25), 5),
        Gene = c(
            rep("gene1", 50), rep("gene2", 50),
            rep("gene3", 25)
        )
    )
    p3 <- plot_og_sizes(og_sim)

    expect_true("ggplot" %in% class(p))
    expect_true("ggplot" %in% class(p2))
    expect_true("ggplot" %in% class(p3))

})

