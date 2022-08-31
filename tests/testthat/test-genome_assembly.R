
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
