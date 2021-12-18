

#' List BUSCO data sets
#'
#' @param envname Name of the Conda environment with external dependencies
#' to be included in the temporary R environment.
#' @param miniconda_path Path to miniconda. Only valid if envname is specified.
#'
#' @return A hierarchically organized list of available data sets as returned
#' by \code{busco --list-datasets}.
#' @rdname list_busco_datasets
#' @export
#' @examples
#' if(busco_is_installed()) {
#'     list_busco_datasets()
#' }
list_busco_datasets <- function(envname = NULL, miniconda_path = NULL) {
    if(load_env(envname, miniconda_path)) {
        Herper::local_CondaEnv(envname, pathToMiniConda = miniconda_path)
    }
    if(!busco_is_installed()) { stop("Unable to find BUSCO in PATH.") }

    out <- system2("busco", args = "--list-datasets")
    return(out)
}

#' Run BUSCO assessment of assembly and annotation quality
#'
#' @param sequence Path to FASTA file with the genome, transcriptome, or
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
#' @param envname Name of the Conda environment with external dependencies
#' to be included in the temporary R environment.
#' @param miniconda_path Path to miniconda. Only valid if envname is specified.
#'
#' @return A character vector with the names of subdirectories and files
#' in the results directory.
#' @export
#' @rdname run_busco
#' @importFrom Herper local_CondaEnv
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
                      threads = 1, outpath = NULL, download_path = tempdir(),
                      envname = NULL, miniconda_path = NULL) {
    if(load_env(envname, miniconda_path)) {
        Herper::local_CondaEnv(envname, pathToMiniConda = miniconda_path)
    }
    if(!busco_is_installed()) { stop("Unable to find BUSCO in PATH.") }
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
    return(outdir)
}






