data("og")
data("interpro_ath")
data("interpro_bol")

test_that("calculate_H() calculates orthogroup homogeneity", {
    orthogroup_df <- merge(og[og$Species == "Ath", ], interpro_ath)
    H <- calculate_H(orthogroup_df)
    expect_equal(class(H), "data.frame")
    expect_equal(ncol(H), 2)
})

test_that("assess_orthogroups() reports homogeneity scores by species", {
    annotation <- list(Ath = interpro_ath[1:1000,], Bol = interpro_bol[1:1000,])
    assess <- assess_orthogroups(og, annotation)
    expect_equal(class(assess), "data.frame")
    expect_true("Mean_H" %in% names(assess))
})

test_that("compare_orthogroups() returns a df of preservation status", {
    og <- og[1:5000, ]
    ref <- og
    test <- og
    comparison <- compare_orthogroups(ref, test)
    perc <- sum(comparison$Preserved) / length(comparison$Preserved)
    expect_equal(class(comparison), "data.frame")
    expect_true(perc == 1)
})
