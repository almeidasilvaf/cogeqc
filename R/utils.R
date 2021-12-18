

#' Check prerequisites to load conda environment temporarily with Herper
#'
#' @param envname Name of the Conda environment with external dependencies
#' to be included in the temporary R environment.
#' @param miniconda_path Path to miniconda. Only valid if envname is specified.
#'
#' @author Fabricio Almeida-Silva
#' @return Logical indicating whether conda environment should be loaded on not.
#' @noRd
load_env <- function(envname = NULL, miniconda_path = NULL) {
    if(!is.null(envname) & !is.null(miniconda_path)) {
        load <- TRUE
    } else if(!is.null(envname) & is.null(miniconda_path)) {
        stop("To load a conda environment, both `envname` and `miniconda_path`
             must be defined.")
    } else if(is.null(envname) & !is.null(miniconda_path)) {
        stop("To load a conda environment, both `envname` and `miniconda_path`
             must be defined.")
    } else {
        load <- FALSE
    }
    return(load)
}


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
