
#' Plot species tree
#'
#' @param tree Tree object as returned by \code{treeio::read.*},
#' a family of functions in the \strong{treeio} package to import tree files
#' in multiple formats, such as Newick, Phylip, NEXUS, and others.
#' If your species tree was inferred with Orthofinder (using STAG), the tree
#' file is located in \emph{Species_Tree/SpeciesTree_rooted_node_labels.txt}.
#' Then, it can be imported with \code{treeio::read_tree(path_to_file)}.
#' @param xlim Numeric vector of x-axis limits. This is useful if your
#' node tip labels are not visible due to margin issues. Default: c(0, 1).
#'
#' @return A ggtree/ggplot object with the species tree.
#' @export
#' @rdname plot_species_tree
#' @importFrom ggtree ggtree geom_tiplab xlim
#' @examples
#' data(tree)
#' plot_species_tree(tree)
plot_species_tree <- function(tree = NULL, xlim = c(0, 1)) {

    p <- ggtree::ggtree(tree) +
        ggtree::geom_tiplab() +
        ggtree::xlim(xlim)
    return(p)
}

#' Plot percentage of genes in orthogroups for each species
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#'
#' @return A ggplot object with a barplot of percentages of genes in
#' orthogroups for each species.
#' @importFrom ggplot2 ggplot aes_ geom_col theme_bw labs xlim geom_text
#' @export
#' @rdname plot_genes_in_ogs
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_genes_in_ogs(stats_list)
plot_genes_in_ogs <- function(stats_list = NULL) {

    stats_table <- stats_list$stats
    p <- ggplot2::ggplot(stats_table) +
        ggplot2::geom_col(ggplot2::aes_(x = ~Perc_genes_in_OGs, y = ~Species),
                          fill = "#3B4992FF", color = "black") +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "(%)", y = "",
                      title = "Genes in orthogroups") +
        ggplot2::xlim(0, 105) +
        ggplot2::geom_text(ggplot2::aes_(x = ~Perc_genes_in_OGs, y = ~Species,
                                         label = ~N_genes_in_OGs), hjust = -0.1)
    return(p)
}


#' Plot number of species-specific orthogroups
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#'
#' @return A ggplot object with a barplot of number of species-specific
#' orthogroups for each species.
#' @importFrom ggplot2 ggplot aes_ geom_col theme_bw labs
#' @export
#' @rdname plot_species_specific_ogs
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_species_specific_ogs(stats_list)
plot_species_specific_ogs <- function(stats_list = NULL) {

    stats_table <- stats_list$stats
    p <- ggplot2::ggplot(stats_table) +
        ggplot2::geom_col(ggplot2::aes_(x = ~N_ssOGs, y = ~Species),
                          fill = "#3B4992FF", color = "black") +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Absolute frequency (#)", y = "",
                      title = "Species-specific orthogroups")
    return(p)
}


