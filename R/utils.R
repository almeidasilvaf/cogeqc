
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
#' path <- system.file("extdata", "Orthogroups.tsv", package = "cogeqc")
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
