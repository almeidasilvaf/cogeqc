

#' Read and parse orthogroups file created by OrthoFinder
#'
#' This function converts the orthogroups file named \strong{Orthogroups.tsv} to
#' a parsed data frame.
#'
#' @param orthogroups_path Path to Orthogroups/Orthogroups.tsv file generated
#' by OrthoFinder.
#'
#' @author Fabricio Almeida-Silva
#' @return A 3-column data frame with orthogroups, species IDs and
#' gene IDs, respectively.
#' @importFrom reshape2 melt
#' @importFrom utils read.csv
#' @export
#' @rdname read_orthogroups
#' @examples
#' path <- system.file("extdata", "Orthogroups.tsv.gz", package = "cogeqc")
#' og <- read_orthogroups(path)
read_orthogroups <- function(orthogroups_path = NULL) {
    of <- utils::read.csv(orthogroups_path, sep = "\t")
    melt_of <- reshape2::melt(of, id.vars="Orthogroup")
    s <- strsplit(melt_of$value, split = ", ")
    final_of <- data.frame(
        Orthogroup = rep(melt_of$Orthogroup, vapply(s, length, numeric(1))),
        Species = rep(melt_of$variable, vapply(s, length, numeric(1))),
        Gene = unlist(s)
    )
    return(final_of)
}


#' Read and parse BUSCO's summary report
#'
#' @param result_dir Path to the directory where BUSCO results are stored.
#' This function will look for the short_summary* file (single run) or
#' short_summary* file (batch mode).
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{Class}{BUSCO class. One of \strong{Complete_SC},
#'                \strong{Complete_duplicate}, \strong{Fragmented}, or
#'                \strong{Missing}}
#'   \item{Frequency}{Frequency of BUSCOs in each class. If BUSCO was run
#'                    in batch mode, this variable will contain relative
#'                    frequencies. If BUSCO was run for a single file,
#'                    it will contain absolute frequencies.}
#'   \item{Lineage}{Name of the lineage dataset used.}
#'   \item{File (batch mode only)}{Name of the input FASTA file.}
#' }
#' @export
#' @rdname read_busco
#' @importFrom reshape2 melt
#' @examples
#' result_dir <- system.file("extdata", package = "cogeqc")
#' df <- read_busco(result_dir)
read_busco <- function(result_dir = NULL) {

    file <- dir(result_dir)
    file <- file[grepl("summary", file)]
    full_path <- paste0(result_dir, "/", file)
    # Single run or batch mode?
    if(startsWith(file, "short")) {
        lines <- readLines(paste0(result_dir, "/", file))
        lineage <- lines[grepl("dataset", lines)]
        lineage <- gsub(".*dataset is: | \\(Creation.*", "", lineage)
        complete_sc <- lines[grepl("single-copy BUSCOs", lines)]
        complete_sc <- gsub("\\tComplete.*|\\t", "", complete_sc)

        complete_dup <- lines[grepl("duplicated BUSCOs", lines)]
        complete_dup <- gsub("\\tComplete.*|\\t", "", complete_dup)

        fragmented <- lines[grepl("Fragmented BUSCOs", lines)]
        fragmented <- gsub("\\tFragmented.*|\\t", "", fragmented)

        missing <- lines[grepl("Missing BUSCOs", lines)]
        missing <- gsub("\\tMissing.*|\\t", "", missing)
        final_df <- data.frame(
            Class = c("Complete_SC", "Complete_duplicate",
                      "Fragmented", "Missing"),
            Frequency = as.numeric(c(complete_sc, complete_dup,
                                     fragmented, missing)),
            Lineage = lineage
        )
    } else if(startsWith(file, "batch")) {
        df <- read.csv(full_path, sep = "\t", header = FALSE, skip = 1)
        df <- df[, c(1, 2, 4:7)]
        colnames(df) <- c("File", "Lineage", "Complete_SC",
                          "Complete_duplicate", "Fragmented", "Missing")
        final_df <- reshape2::melt(df, id = c("File", "Lineage"))
        colnames(final_df) <- c("File", "Lineage", "Class", "Frequency")
        final_df <- final_df[, c("Class", "Frequency", "Lineage", "File")]
    }
    return(final_df)

}
