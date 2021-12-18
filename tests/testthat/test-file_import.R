data("og")
data("interpro_ath")
data("interpro_bol")


test_that("read_orthogroups() correctly reads and parses Orthogroups.tsv", {
    file <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
    og <- read_orthogroups(file)
    expect_equal(class(og), "data.frame")
    expect_equal(names(og), c("Orthogroup", "Species", "Gene"))
})

test_that("read_busco() properly reads BUSCO summary output", {
    result_dir <- system.file("extdata", package = "cogeqc")
    df <- read_busco(result_dir)
    expect_equal(class(df), "data.frame")
    expect_equal(ncol(df), 3)
    expect_true(identical(names(df), c("Class", "Frequency", "Lineage")))
})
