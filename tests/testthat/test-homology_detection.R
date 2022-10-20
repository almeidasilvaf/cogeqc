#----Load data------------------------------------------------------------------
data("og")
data("interpro_ath")
data("interpro_bol")

## Simulate orthogroups to test if percentages are correclty calculated
fake_og_zero <- data.frame(
    Orthogroup = "OG0001",
    Species = "X",
    Gene = paste0("Gene", 1:8),
    Annotation = paste0("Domain", 1:8)
)
fake_og_hundred <- data.frame(
    Orthogroup = "OG0002",
    Species = "X",
    Gene = paste0("Gene", 1:8),
    Annotation = "Domain2"
)
fake_og <- rbind(fake_og_hundred, fake_og_zero)

#----Start tests----------------------------------------------------------------
test_that("calculate_H() calculates orthogroup homogeneity", {
    H <- calculate_H(fake_og, correct_overclustering = TRUE)
    H2 <- calculate_H(fake_og, correct_overclustering = FALSE)
    expect_equal(class(H), "data.frame")
    expect_equal(ncol(H), 2)
    expect_equal(ncol(H2), 2)
})

test_that("assess_orthogroups() reports homogeneity scores by species", {
    annotation <- list(Ath = interpro_ath[1:1000,], Bol = interpro_bol[1:1000,])
    assess <- assess_orthogroups(og, annotation)
    expect_equal(class(assess), "data.frame")
    expect_true("Mean_score" %in% names(assess))
})

test_that("compare_orthogroups() returns a df of preservation status", {

    og <- og[1:5000, ]
    ref <- og
    test <- og
    comparison <- compare_orthogroups(ref, test)
    perc <- sum(comparison$Preserved) / length(comparison$Preserved)
    expect_equal(class(comparison), "data.frame")
    expect_true(perc == 1)

    # Simulate fake test OG test with no species in common
    og_nospecies <- test
    og_nospecies$Species <- paste0(og_nospecies$Species, "_fake")

    expect_error(
        compare_orthogroups(ref, og_nospecies)
    )
})
