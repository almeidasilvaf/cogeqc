
#----Load data------------------------------------------------------------------
data("og")
data("interpro_ath")
data("interpro_bol")
data("batch_summary")

#----Start tests----------------------------------------------------------------
test_that("read_orthogroups() correctly reads and parses Orthogroups.tsv", {
    file <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
    og <- read_orthogroups(file)
    expect_equal(class(og), "data.frame")
    expect_equal(names(og), c("Orthogroup", "Species", "Gene"))
    expect_equal(ncol(og), 3)
})

test_that("read_busco() properly reads BUSCO summary output", {
    result_dir <- system.file("extdata", package = "cogeqc")

    # Copy BUSCO summary twice to a directory to test if it can handle 2+ files
    resdir2 <- tempdir()
    busco_out <- system.file("extdata", "short_summary.txt", package = "cogeqc")
    cp <- file.copy(
        from = busco_out,
        to = c(file.path(resdir2, "short_summary_01.txt"),
               file.path(resdir2, "short_summary_02.txt"))
    )

    # Test 1: one file
    df <- read_busco(result_dir)

    # Test 2: 2 files in the same directory
    df2 <- read_busco(resdir2)

    # Test 3: batch summary
    resdir3 <- file.path(tempdir(), "test3")
    if(!dir.exists(resdir3)) { dir.create(resdir3, recursive = TRUE) }
    lines <- c(
        "##",
        "File\tLineage\tC3\tComp_SC\tComp_dup\tFrag\tMis\tC8"
    )
    w <- writeLines(lines, con = file.path(resdir3, "batch_summary.txt"))
    df3 <- read_busco(resdir3)

    # Test 4: wrong directory (no BUSCO output)
    random_dir <- file.path(tempdir(), "random_dir")
    if(!dir.exists(random_dir)) { dir.create(random_dir, recursive = TRUE) }
    w <- writeLines("test", con = file.path(random_dir, "summary_test.txt"))
    expect_error(read_busco(random_dir))

    expect_equal(class(df), "data.frame")
    expect_equal(ncol(df), 3)
    expect_true(identical(names(df), c("Class", "Frequency", "Lineage")))

    expect_equal(class(df2), "data.frame")

    expect_equal(class(df3), "data.frame")
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
