
data(synnet)

test_that("assess_synnet() works", {
    x <- assess_synnet(synnet)
    expect_equal(ncol(x), 3)
    expect_equal(names(x), c("CC", "Node_number", "Score"))
})

test_that("assess_synnet_list() returns a data frame", {
    net1 <- synnet
    net2 <- synnet[-sample(1:10000, 500), ]
    net3 <- synnet[-sample(1:10000, 1000), ]
    synnet_list <- list(net1 = net1, net2 = net2, net3 = net3)
    x <- assess_synnet_list(synnet_list)
    expect_equal(class(x), "data.frame")
    expect_equal(ncol(x), 4)
    expect_true("Network" %in% names(x))
})
