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
