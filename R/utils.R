

#' Wrapper to check if command is found in PATH
#'
#' @param cmd Command to test.
#' @param args Arguments for command.
#'
#' @author Fabricio Almeida-Silva
#' @return Logical indicating whether the command is in PATH or not.
#' @noRd
is_valid <- function(cmd = NULL, args = NULL) {
    found <- tryCatch(
        system2(cmd, args = args, stdout = FALSE, stderr = FALSE),
        error = function(e) return(FALSE),
        warning = function(w) return(FALSE)
    )
    if(!isFALSE(found)) {
        found <- TRUE
    }
    return(found)
}


#' Check if BUSCO is installed
#'
#' @return Logical indicating whether BUSCO is installed or not.
#' @export
#' @rdname busco_is_installed
#' @examples
#' busco_is_installed()
busco_is_installed <- function() {
    valid <- is_valid(cmd = "busco", args = "-h")
    return(valid)
}


#' Add label to plot results of BUSCO batch mode
#'
#' @param summary_df Data frame with BUSCO summary output as returned
#' by \code{read_busco()}.
#'
#' @return The same input data frame, but with an additional column
#' named \strong{Label} with labels for plotting.
#' @noRd
add_label_busco <- function(summary_df = NULL) {
    slist <- split(summary_df, summary_df$File)
    labels <- lapply(slist, function(x) {
        sc <- x[x$Class == "Complete_SC", "Frequency"]
        dup <- x[x$Class == "Complete_duplicate", "Frequency"]
        comp <- sc + dup
        frag <- x[x$Class == "Fragmented", "Frequency"]
        mis <- x[x$Class == "Missing", "Frequency"]
        label1 <- paste0("C: ", comp, "% [S: ", sc, "%, D: ", dup, "%]")
        label2 <- paste0("F: ", frag, "%")
        label3 <- paste0("M: ", mis, "%")
        label <- paste(label1, label2, label3, sep = ", ")
        return(label)
    })
    label_df <- data.frame(File = names(slist), Label = unlist(labels))
    final_df <- merge(summary_df, label_df, by = "File")
    return(final_df)
}


#' Convert species names to abbreviated names
#'
#' This function converts full species names to abbreviated names according
#' to the standard nomenclature rules. For instance, "Drosophila_melanogaster"
#' is converted to "Dme"
#'
#' @param snames Character vector of species names to be converted.
#'
#' @return A character vector of converted names.
#' @noRd
abbreviate_names <- function(snames = NULL) {

    genus <- substr(snames, start = 1, stop = 1)
    specific <- substr(gsub(".*_", "", snames), start = 1, stop = 2)

    new_names <- paste0(genus, specific)
    return(new_names)
}


