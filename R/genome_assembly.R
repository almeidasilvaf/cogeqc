

#' List BUSCO data sets
#'
#'
#' @return A hierarchically organized list of available data sets as returned
#' by \code{busco --list-datasets}.
#' @rdname list_busco_datasets
#' @export
#' @examples
#' if(busco_is_installed()) {
#'     list_busco_datasets()
#' }
list_busco_datasets <- function() {
    if(!busco_is_installed()) { stop("Unable to find BUSCO in PATH.") }

    out <- system2("busco", args = "--list-datasets")
    return(out)
}


#' A wrapper to handle input file to \code{run_busco()}
#'
#' @param sequence An object of class DNAStringSet/AAStringSet/RNAStringSet or
#' path to FASTA file with the genome, transcriptome, or
#' protein sequences to be analyzed. If there are many FASTA files in a
#' directory, you can input the path to this directory, so BUSCO will be run
#' in all FASTA files inside it.
#'
#' @return Path to FASTA file with the input sequence.
#' @noRd
#' @importFrom Biostrings writeXStringSet
handle_busco_input <- function(sequence = NULL) {
    outfile <- sequence
    ss <- class(sequence) %in% c("DNAStringSet", "RNAStringSet", "AAStringSet")

    if(ss) {
        outfile <- tempfile(pattern = "busco", fileext = ".fa")
        Biostrings::writeXStringSet(sequence, filepath = outfile)
    }
    return(outfile)
}


#' Run BUSCO assessment of assembly and annotation quality
#'
#' @param sequence An object of class DNAStringSet/AAStringSet/RNAStringSet or
#' path to FASTA file with the genome, transcriptome, or
#' protein sequences to be analyzed. If there are many FASTA files in a
#' directory, you can input the path to this directory, so BUSCO will be run
#' in all FASTA files inside it.
#' @param outlabel Character with a recognizable short label for
#' analysis directory and files.
#' @param mode Character with BUSCO mode. One of 'genome', 'transcriptome',
#' or 'proteins'.
#' @param lineage Character with name of lineage to be used.
#' @param auto_lineage Character indicating whether BUSCO should determine
#' optimum lineage path automatically. One of 'euk', 'prok', 'all', or NULL.
#' If 'euk', it will determine optimum lineage path on eukaryote tree.
#' If 'prok', it will determine optimum lineage path on non-eukaryote trees.
#' If 'all', it will determine optimum lineage path for all trees.
#' If NULL, it will not automatically determine lineage, and \emph{lineage}
#' must be manually specified. Default: NULL.
#' @param force Logical indicating whether existing runs with the same
#' file names should be overwritten. Default: FALSE.
#' @param threads Numeric with the number of threads/cores to use. Default: 1.
#' @param outpath Path to results directory. If NULL, results will be stored
#' in the current working directory. Default: NULL.
#' @param download_path Path to directory where BUSCO datasets will be stored
#' after downloading. Default: tempdir().
#'
#' @return A character vector with the names of subdirectories and files
#' in the results directory.
#' @export
#' @rdname run_busco
#' @examples
#' \donttest{
#' sequence <- system.file("extdata", "Hse_subset.fa", package = "cogeqc")
#' download_path <- paste0(tempdir(), "/datasets")
#' if(busco_is_installed()) {
#'     run_busco(sequence, outlabel = "Hse", mode = "genome",
#'               lineage = "burkholderiales_odb10",
#'               outpath = tempdir(), download_path = download_path)
#' }
#' }
run_busco <- function(sequence = NULL, outlabel = NULL,
                      mode = c("genome", "transcriptome", "proteins"),
                      lineage = NULL, auto_lineage = NULL, force = FALSE,
                      threads = 1, outpath = NULL, download_path = tempdir()) {

    if(!busco_is_installed()) { stop("Unable to find BUSCO in PATH.") }
    # Handle input type
    sequence <- handle_busco_input(sequence)

    # Handle outlabel
    if(is.null(outlabel)) {
        outlabel <- paste0("run_", paste0(sample(letters, 3), collapse = ""))
    }
    args <- c("-i", sequence, "-o", outlabel, "-m", mode,
              "-c", threads, "--download_path", download_path)

    # Should lineage be automatically detected?
    if(!is.null(auto_lineage)) {
        if(auto_lineage == "euk") {
            args <- c(args, "--auto-lineage-euk")
        } else if(auto_lineage == "prok") {
            args <- c(args, "--auto-lineage-prok")
        } else {
            args <- c(args, "--auto-lineage")
        }
    } else if(is.null(auto_lineage) & !is.null(lineage)) {
        args <- c(args, "-l", lineage)
    } else {
        stop("Choose a lineage or let BUSCO automatically determine it.")
    }

    # Output directory
    if(is.null(outpath)) { outpath <- getwd() }
    args <- c(args, "--out_path", outpath)

    # Should it be forced?
    if(force) { args <- c(args, "-f") }

    system2("busco", args = args)
    out_dir <- dir(paste0(outpath, "/", outlabel))
    return(out_dir)
}


#' Plot BUSCO summary output
#'
#' @param summary_df Data frame with BUSCO summary output as returned
#' by \code{read_busco()}.
#'
#' @return A ggplot object with a barplot of BUSCOs in each class.
#' @export
#' @rdname plot_busco
#' @importFrom ggplot2 ggplot aes_ geom_col ylim xlim theme_bw labs
#' scale_fill_manual geom_text
#' @examples
#' # Single file
#' result_dir <- system.file("extdata", package = "cogeqc")
#' summary_df <- read_busco(result_dir)
#' # Batch mode
#' data(batch_summary)
#' plot_busco(summary_df)
#' plot_busco(batch_summary)
plot_busco <- function(summary_df = NULL) {

    lineage <- unique(summary_df$Lineage)
    if("File" %in% names(summary_df)) {
        summary_df <- add_label_busco(summary_df)
        p <- ggplot2::ggplot(summary_df) +
            ggplot2::geom_col(ggplot2::aes_(x = ~Frequency, y = ~File,
                                            fill = ~Class),
                              color = "black") +
            ggplot2::xlim(c(0, 100)) +
            ggplot2::labs(
                title = "Percentage of BUSCOs for each class across files",
                subtitle = paste0("Lineage dataset: ", lineage),
                y = "Sequence file", x = "% BUSCOs"
            ) +
            ggplot2::geom_text(ggplot2::aes_(label = ~Label, x = 50,
                                             y = ~File), color = "grey90")

    } else {
        total <- sum(summary_df$Frequency)
        summary_df$Perc <- round((summary_df$Frequency / total) * 100, 2)
        p <- ggplot2::ggplot(summary_df) +
            ggplot2::geom_col(ggplot2::aes_(x = ~Class, y = ~Perc,
                                            fill = ~Class),
                              color = "black", show.legend = FALSE) +
            ggplot2::ylim(c(0, 105)) +
            ggplot2::labs(title = "Percentage of BUSCOs for each class",
                          subtitle = paste0("Lineage dataset: ", lineage,
                                            ", N = ", total),
                          x = "", y = "% BUSCOs") +
            ggplot2::geom_text(ggplot2::aes_(label = ~Perc, y = ~Perc,
                                             x = ~Class), vjust = -0.5)

    }
    p <- p +
        ggplot2::scale_fill_manual(labels = c(
            "Complete & SC", "Complete & duplicate", "Fragmented", "Missing"
        ),
        values = c(
            "#32709a", "#59AAE1", "darkgoldenrod2", "#db5850"
        )) +
        ggplot2::theme_bw()
    return(p)
}



