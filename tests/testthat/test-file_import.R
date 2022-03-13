data("og")
data("interpro_ath")
data("interpro_bol")


test_that("read_orthogroups() correctly reads and parses Orthogroups.tsv", {
    file <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
    og <- read_orthogroups(file)
    expect_equal(class(og), "data.frame")
    expect_equal(names(og), c("Orthogroup", "Species", "Gene"))
    expect_equal(ncol(og), 3)
})

test_that("read_busco() properly reads BUSCO summary output", {
    result_dir <- system.file("extdata", package = "cogeqc")
    df <- read_busco(result_dir)
    expect_equal(class(df), "data.frame")
    expect_equal(ncol(df), 3)
    expect_true(identical(names(df), c("Class", "Frequency", "Lineage")))
})

test_that("read_orthofinder_stats() reads Orthofinder summary stats", {
    stats_path <- system.file("extdata", package = "cogeqc")
    ortho_stats <- read_orthofinder_stats(stats_path)$stats
    ortho_dups <- read_orthofinder_stats(stats_path)$duplications
    ortho_overlap <- read_orthofinder_stats(stats_path)$og_overlap

    expect_equal(class(ortho_stats), "data.frame")
    expect_equal(ncol(ortho_stats), 8)
    expect_equal(class(ortho_stats$Species), "factor")

    expect_equal(class(ortho_dups), "data.frame")
    expect_equal(class(ortho_overlap), "data.frame")
    expect_true(identical(rownames(ortho_overlap), colnames(ortho_overlap)))
})
