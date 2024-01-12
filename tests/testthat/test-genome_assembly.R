
#----Load data------------------------------------------------------------------
data(batch_summary)

#----Start tests----------------------------------------------------------------
test_that("plot_busco() returns a ggplot object", {

    result_dir <- system.file("extdata", package = "cogeqc")
    summary_df <- read_busco(result_dir)

    p <- plot_busco(batch_summary)
    p2 <- plot_busco(summary_df)

    expect_true("ggplot" %in% class(p))
    expect_true("ggplot" %in% class(p2))
})

test_that("list_busco_datasets() works", {
    if(!busco_is_installed()) {
        expect_error(list_busco_datasets())
    } else {
        expect_true(!is.null(list_busco_datasets()))
    }
})

test_that("handle_busco_input() handles input file to run_busco()", {
    seq_file <- system.file("extdata", "Hse_subset.fa", package = "cogeqc")
    seq_dna <- Biostrings::readDNAStringSet(seq_file)

    h <- handle_busco_input(seq_file)
    h2 <- handle_busco_input(seq_dna)

    expect_equal(class(h), "character")
    expect_equal(class(h2), "character")
})


test_that("get_genome_stats() returns a data frame of NCBI genome stats", {

    t1 <- get_genome_stats(
        taxon = "spermatophyta",
        filters = list(filters.assembly_level = "chromosome")
    )

    expect_equal(ncol(t1), 36)

    expect_error(get_genome_stats())
})


test_that("compare_genome_stats() compares genome assembly stats", {

    ncbi_stats <- get_genome_stats(taxon = "Zea mays")

    ## Create a data frame of stats for fictional maize genome
    user_stats <- data.frame(
        accession = "my_lovely_maize",
        sequence_length = 2.4 * 1e9,
        gene_count_total = 50000,
        CC_ratio = 1
    )
    user_stats_error <- user_stats
    user_stats_error$random_column <- "nothing"

    # Compare stats
    t1 <- compare_genome_stats(ncbi_stats, user_stats)

    expect_equal(names(t1), c("accession", "variable", "percentile", "rank"))

    expect_error(compare_genome_stats())
    expect_error(compare_genome_stats(ncbi_stats, user_stats[, -1]))
    expect_error(compare_genome_stats(ncbi_stats, user_stats_error))
})

