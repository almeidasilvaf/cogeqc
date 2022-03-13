
#----Load data------------------------------------------------------------------
data(batch_summary)

#----Start tests----------------------------------------------------------------
test_that("plot_busco() returns a ggplot object", {
    p <- plot_busco(batch_summary)
    expect_true("ggplot" %in% class(p))
})
