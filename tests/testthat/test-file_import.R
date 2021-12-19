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

test_that("read_orthofinder_stats() reads Orthofinder summary stats", {
    stats_path <- system.file("extdata", "Statistics_PerSpecies.tsv",
                              package = "cogeqc")
    ortho_stats <- read_orthofinder_stats(file)
    expect_equal(class(ortho_stats), "data.frame")
    expect_equal(ncol(ortho_stats), 9)
    expect_equal(sum(sapply(ortho_stats, class) == "numeric"), 8)
    expect_equal(class(ortho_stats$Species), "factor")
})
