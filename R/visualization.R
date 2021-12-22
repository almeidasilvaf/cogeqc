
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
#' @param stats_list (optional) A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}. If this list
#' is given as input, nodes will be labeled with the number of duplications.
#'
#' @return A ggtree/ggplot object with the species tree.
#' @export
#' @rdname plot_species_tree
#' @importFrom ggtree ggtree geom_tiplab xlim
#' @importFrom ggplot2 aes_ labs
#' @examples
#' data(tree)
#' plot_species_tree(tree)
plot_species_tree <- function(tree = NULL, xlim = c(0, 1),
                              stats_list = NULL) {

    p <- ggtree::ggtree(tree) +
        ggtree::geom_tiplab() +
        ggtree::xlim(xlim)

    if(!is.null(stats_list)) {
        dups <- stats_list$duplications
        dups <- dups[dups$Node %in% tree$node.label, ]
        names(dups) <- c("label", "dups")
        p$data <- merge(p$data, dups, all.x = TRUE)

        p <- p +
            ggtree::geom_text2(ggplot2::aes_(subset = ~!(isTip), label = ~dups),
                               hjust = 1.3, vjust = -0.5) +
            ggplot2::labs(
                title = "Duplications per node"
            )
    }
    return(p)
}

#' Plot percentage of genes in orthogroups for each species
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#'
#' @return A ggplot object with a barplot of percentages of genes in
#' orthogroups for each species.
#' @importFrom ggplot2 ggplot aes_ geom_col theme_bw labs geom_text
#' scale_x_continuous
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
        ggplot2::scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                                    limits = c(0, 115)) +
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
                          fill = "#BB0021FF", color = "black") +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Absolute frequency (#)", y = "",
                      title = "Species-specific orthogroups")
    return(p)
}


#' Plot species-specific duplications
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#'
#' @return A ggplot object with a barplot of number of species-specific
#' duplications.
#' @importFrom ggplot2 ggplot aes_ geom_col theme_bw labs
#' @export
#' @rdname plot_duplications
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_duplications(stats_list)
plot_duplications <- function(stats_list = NULL) {

    stats_table <- stats_list$stats
    p <- ggplot2::ggplot(stats_table) +
        ggplot2::geom_col(ggplot2::aes_(x = ~Dups, y = ~Species),
                          fill = "grey40", color = "black") +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Absolute frequency (#)", y = "",
                      title = "Species-specific duplications")
    return(p)
}


#' Plot a panel with a summary of Orthofinder stats
#'
#' This function is a wrapper for \code{plot_species_tree},
#' \code{plot_duplications}, \code{plot_genes_in_ogs},
#' \code{plot_species_specific_ogs}.
#'
#' @param tree Tree object as returned by \code{treeio::read.*},
#' a family of functions in the \strong{treeio} package to import tree files
#' in multiple formats, such as Newick, Phylip, NEXUS, and others.
#' If your species tree was inferred with Orthofinder (using STAG), the tree
#' file is located in \emph{Species_Tree/SpeciesTree_rooted_node_labels.txt}.
#' Then, it can be imported with \code{treeio::read_tree(path_to_file)}.
#' @param stats_list (optional) A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}. If this list
#' is given as input, nodes will be labeled with the number of duplications.
#' @param xlim Numeric vector of x-axis limits. This is useful if your
#' node tip labels are not visible due to margin issues. Default: c(0, 1).
#'
#'
#' @return A panel of ggplot objects.
#' @export
#' @rdname plot_orthofinder_stats
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 theme element_blank
#' @importFrom ggtree get_taxa_name
#' @examples
#' data(tree)
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_orthofinder_stats(tree, xlim = c(0, 1.5), stats_list = stats_list)
plot_orthofinder_stats <- function(tree = NULL, stats_list = NULL,
                                   xlim = c(0,1)) {

    if(is.null(tree) | is.null(stats_list)) {
        stop("Both 'tree' and 'stats_list' must be given as input.")
    }

    remove_species_label <- function() {
        return(ggplot2::theme(axis.text.y = ggplot2::element_blank()))
    }

    p1 <- plot_species_tree(tree, xlim, stats_list) + remove_species_label()

    # Reorder factor levels according to the tree
    fct_order <- rev(ggtree::get_taxa_name(p1))
    stats_list$stats$Species <- factor(stats_list$stats$Species,
                                       levels = fct_order)

    p2 <- plot_duplications(stats_list) + remove_species_label()
    p3 <- plot_genes_in_ogs(stats_list) + remove_species_label()
    p4 <- plot_species_specific_ogs(stats_list) + remove_species_label()

    final_figure <- patchwork::wrap_plots(p1, p2, p3, p4, nrow = 1)
    return(final_figure)
}


#' Plot pairwise orthogroup overlap between species
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#' @param clust Logical indicating whether to clust data based on overlap.
#' Default: TRUE
#' @return A ggplot object with a heatmap.
#' @export
#' @rdname plot_og_overlap
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes_ geom_tile theme_minimal geom_text labs
#' scale_fill_gradient coord_fixed
#' @importFrom stats hclust as.dist quantile
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_og_overlap(stats_list)
plot_og_overlap <- function(stats_list = NULL, clust = TRUE) {

    overlap <- as.matrix(stats_list$og_overlap)
    rownames(overlap) <- abbreviate_names(rownames(overlap))
    colnames(overlap) <- abbreviate_names(colnames(overlap))

    # Cluster to reorder
    if(clust) {
        hc <- stats::hclust(stats::as.dist(overlap))
        overlap <- overlap[hc$order, hc$order]
    }

    # Remove diagonals and lower triangle
    diag(overlap) <- NA
    overlap[lower.tri(overlap)] <- NA
    ovm <- reshape2::melt(overlap, na.rm = TRUE)
    names(ovm) <- c("Species1", "Species2", "N")

    q75 <- stats::quantile(ovm$N)[4]
    ovm$high <- ifelse(ovm$N >= q75, 'yes', 'no')
    p <- ggplot2::ggplot(ovm, ggplot2::aes_(x = ~Species1, y = ~Species2,
                                            fill = ~N)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "#E5F5E0", high = "#00441B",
                                     name = "Overlap size") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Orthogroup overlap",
            x = "",
            y = ""
        ) +
        ggplot2::geom_text(ggplot2::aes_(x = ~Species1, y = ~Species2,
                                         label = ~N, color = ~high),
                           size = 4) +
        ggplot2::scale_color_manual(
            values = c('no' = 'grey20', 'yes' = "grey90"), guide = "none",
        )
    return(p)
}


