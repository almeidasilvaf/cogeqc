data("og")
data("interpro_ath")
data("interpro_bol")


test_that("read_orthogroups() correctly reads and parses Orthogroups.tsv", {
    file <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
    og <- read_orthogroups(file)
    expect_equal(class(og), "data.frame")
    expect_equal(names(og), c("Orthogroup", "Species", "Gene"))
})
