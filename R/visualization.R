
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
#' @param stats_list (optional) A list of data frames with Orthofinder summary
#' stats as returned by the function \code{read_orthofinder_stats}. If this list
#' is given as input, nodes will be labeled with the number of duplications.
#'
#' @return A ggtree/ggplot object with the species tree.
#' @export
#' @rdname plot_species_tree
#' @importFrom ggtree ggtree geom_tiplab xlim
#' @importFrom ggplot2 aes labs
#' @examples
#' data(tree)
#' plot_species_tree(tree)
plot_species_tree <- function(tree = NULL, xlim = c(0, 1),
                              stats_list = NULL) {

    p <- ggtree(tree) +
        geom_tiplab() +
        xlim(xlim)

    if(!is.null(stats_list)) {
        dups <- stats_list$duplications
        dups <- dups[dups$Node %in% tree$node.label, ]
        names(dups) <- c("label", "dups")
        p$data <- merge(p$data, dups, all.x = TRUE)

        p <- p +
            ggtree::geom_text2(
                aes(label = .data$dups),
                hjust = 1.3, vjust = -0.5
            ) +
            labs(title = "Duplications per node")
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
#' @importFrom ggplot2 ggplot aes geom_col theme_bw labs geom_text
#' scale_x_continuous
#' @export
#' @rdname plot_genes_in_ogs
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_genes_in_ogs(stats_list)
plot_genes_in_ogs <- function(stats_list = NULL) {

    stats_table <- stats_list$stats
    p <- ggplot(stats_table) +
        geom_col(
            aes(x = .data$Perc_genes_in_OGs, y = .data$Species),
            fill = "#3B4992FF", color = "black"
        ) +
        theme_bw() +
        labs(x = "(%)", y = "", title = "Genes in orthogroups") +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 115)) +
        geom_text(
            aes(
                x = .data$Perc_genes_in_OGs, y = .data$Species,
                label = .data$N_genes_in_OGs
            ), hjust = -0.1
        )

    return(p)
}


#' Plot number of species-specific orthogroups
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#'
#' @return A ggplot object with a barplot of number of species-specific
#' orthogroups for each species.
#' @importFrom ggplot2 ggplot aes geom_col theme_bw labs
#' @export
#' @rdname plot_species_specific_ogs
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_species_specific_ogs(stats_list)
plot_species_specific_ogs <- function(stats_list = NULL) {

    stats_table <- stats_list$stats
    p <- ggplot(stats_table) +
        geom_col(
            aes(x = .data$N_ssOGs, y = .data$Species),
            fill = "#BB0021FF", color = "black"
        ) +
        theme_bw() +
        labs(
            x = "Absolute frequency (#)", y = "",
            title = "Species-specific orthogroups"
        )

    return(p)
}


#' Plot species-specific duplications
#'
#' @param stats_list A list of data frames with Orthofinder summary stats
#' as returned by the function \code{read_orthofinder_stats}.
#'
#' @return A ggplot object with a barplot of number of species-specific
#' duplications.
#' @importFrom ggplot2 ggplot aes geom_col theme_bw labs
#' @export
#' @rdname plot_duplications
#' @examples
#' dir <- system.file("extdata", package = "cogeqc")
#' stats_list <- read_orthofinder_stats(dir)
#' plot_duplications(stats_list)
plot_duplications <- function(stats_list = NULL) {

    stats_table <- stats_list$stats
    p <- ggplot(stats_table) +
        geom_col(
            aes(x = .data$Dups, y = .data$Species),
            fill = "grey40", color = "black"
        ) +
        theme_bw() +
        labs(
            x = "Absolute frequency (#)", y = "",
            title = "Species-specific duplications"
        )

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
        return(theme(axis.text.y = element_blank()))
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
#' @importFrom ggplot2 ggplot aes geom_tile theme_minimal geom_text labs
#' scale_fill_gradient coord_fixed scale_color_manual
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
    p <- ggplot(
        ovm, aes(x = .data$Species1, y = .data$Species2,fill = .data$N)
    ) +
        geom_tile() +
        scale_fill_gradient(low = "#E5F5E0", high = "#00441B",
                                     name = "Overlap size") +
        theme_minimal() +
        labs(
            title = "Orthogroup overlap",
            x = "",
            y = ""
        ) +
        geom_text(
            aes(
                x = .data$Species1, y = .data$Species2, label = .data$N,
                color = .data$high
            ), size = 4
        ) +
        scale_color_manual(
            values = c('no' = 'grey20', 'yes' = "grey90"), guide = "none"
        )

    return(p)
}


#' Plot orthogroup sizes per species
#'
#' @param orthogroups A 3-column data frame with columns \strong{Orthogroup},
#' \strong{Species}, and \strong{Gene}. This data frame can be created from
#' the 'Orthogroups.tsv' file generated by OrthoFinder with the function
#' \code{read_orthogroups()}.
#' @param log Logical indicating whether to transform orthogroups sizes with
#' natural logarithms. Default: FALSE.
#' @param max_size Numeric indicating the maximum orthogroup size to consider.
#' If this parameter is not NULL, orthogroups larger
#' than `max_size` (e.g., 100) will not be considered. Default: NULL.
#'
#' @return A ggplot object with a violin plot.
#' @export
#' @rdname plot_og_sizes
#' @importFrom ggplot2 aes ggplot geom_violin geom_boxplot theme_bw labs
#' @examples
#' data(og)
#' plot_og_sizes(og, log = TRUE)
#' plot_og_sizes(og, max_size = 100)
#' plot_og_sizes(og, log = TRUE, max_size = 100)
plot_og_sizes <- function(orthogroups = NULL, log = FALSE, max_size = NULL) {

    og_species <- split(orthogroups, orthogroups$Species)
    sizes <- Reduce(rbind, lapply(seq_along(og_species), function(x) {
        freqs <- as.data.frame(table(og_species[[x]]$Orthogroup))
        freqs$Species <- names(og_species)[x]
        return(freqs)
    }))
    names(sizes) <- c("OG", "Frequency", "Species")
    if(!is.null(max_size) & is.numeric(max_size)) {
        sizes <- sizes[sizes$Frequency <= max_size, ]
    }

    # Define palette
    pal <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF",
             "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF",
             "#AEC7E8FF", "#FFBB78FF", "#98DF8AFF", "#FF9896FF", "#C5B0D5FF",
             "#C49C94FF", "#F7B6D2FF", "#C7C7C7FF", "#DBDB8DFF", "#9EDAE5FF")

    if(length(unique(og_species)) > 20) {
        pal <- rep(pal, 3)
    }

    if(log) {
        sizes$Frequency <- log(sizes$Frequency + 1)
        p <- ggplot(sizes, aes(x = .data$Frequency, y = .data$Species)) +
            geom_violin(aes(fill = .data$Species), show.legend = FALSE) +
            geom_boxplot(fill = "white", color = "black", width = 0.08) +
            theme_bw() +
            labs(
                title = "OG sizes per species",
                x = "OG size (log scale)", y = ""
            )
    } else {
        p <- ggplot(sizes, aes(x = .data$Frequency, y = .data$Species)) +
            geom_violin(aes(fill = .data$Species), show.legend = FALSE) +
            geom_boxplot(fill = "white", color = "black", width = 0.08) +
            theme_bw() +
            labs(
                title = "OG sizes per species",
                x = "OG size", y = ""
            )
    }
    p <- p +
        scale_fill_manual(values = pal)

    return(p)
}


#' Plot statistics on genome assemblies on the NCBI
#'
#' @param ncbi_stats A data frame of summary statistics for a particular
#' taxon obtained from the NCBI, as obtained with the
#' function \code{get_genome_stats}.
#' @param user_stats (Optional) A data frame with assembly statistics obtained
#' by the user. Statistics in this data frame are highlighted in red
#' if this data frame is passed.
#' A column named \strong{accession} is mandatory, and it must
#' contain unique identifiers for the genome(s) analyzed by the user. Dummy
#' variables can be used as identifiers (e.g., "my_genome_001"), as long as
#' they are unique. All other column containing assembly stats must have the
#' same names as their corresponding columns in the data frame specified
#' in \strong{ncbi_stats}. For instance, stats on total number of genes and
#' sequence length must be in columns named "gene_count_total" and
#' "sequence_length", as in the \strong{ncbi_stats} data frame.
#'
#' @return A composition of ggplot objects made with patchwork.
#'
#' @export
#' @rdname plot_genome_stats
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_discrete theme ggtitle labs
#' scale_y_continuous scale_color_identity element_blank theme_minimal
#' scale_alpha_identity
#' @importFrom stats reshape
#' @importFrom scales label_number
#' @importFrom ggbeeswarm geom_quasirandom
#' @importFrom patchwork wrap_plots
#' @examples
#' # Example 1: plot stats on maize genomes on the NCBI
#' ## Obtain stats for maize genomes on the NCBI
#' ncbi_stats <- get_genome_stats(taxon = "Zea mays")
#'
#' plot_genome_stats(ncbi_stats)
#'
#' ## Plot stats
#' # Example 2: highlight user-defined stats in the distribution
#' ## Create a data frame of stats for fictional maize genome
#' user_stats <- data.frame(
#'     accession = "my_lovely_maize",
#'     sequence_length = 2.4 * 1e9,
#'     gene_count_total = 50000,
#'     CC_ratio = 1
#' )
#'
#' plot_genome_stats(ncbi_stats, user_stats)
#'
plot_genome_stats <- function(ncbi_stats = NULL, user_stats = NULL) {

    # Plot assembly level
    p_al <- ggplot(ncbi_stats, aes(y = .data$assembly_level)) +
        geom_bar(
            aes(fill = .data$assembly_level), show.legend = FALSE,
            stat = "count"
        ) +
        theme_minimal() +
        scale_y_discrete(drop = FALSE) +
        scale_fill_manual(
            values = c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF"),
            limits = c("Complete", "Chromosome", "Scaffold", "Contig")
        ) +
        labs(
            y = "", x = "# genomes",
            title = "Overall", subtitle = "Assembly level"
        )

    # Plot continuous variables
    distro_cols <- c(
        "sequence_length", "ungapped_length", "GC_percent", "gene_count_total",
        "CC_ratio", "contig_count", "contig_N50", "contig_L50",
        "scaffold_count", "scaffold_N50", "scaffold_L50"
    )
    stats_df <- ncbi_stats[, c("accession", distro_cols)]

    ## Reshape to long, clean variable names, and remove NAs
    stats_df <- stats::reshape(
        data = stats_df,
        direction = "long",
        varying = seq(2, ncol(stats_df)),
        v.names = "value",
        times = names(stats_df)[seq(2, ncol(stats_df))],
        timevar = "stat",
        idvar = "accession"
    )
    stats_df$highlight <- FALSE

    if(!is.null(user_stats)) {
        ustats_df <- stats::reshape(
            data = user_stats,
            direction = "long",
            varying = seq(2, ncol(user_stats)),
            v.names = "value",
            times = names(user_stats)[seq(2, ncol(user_stats))],
            timevar = "stat",
            idvar = "accession"
        )
        ustats_df$highlight <- TRUE
        stats_df <- rbind(stats_df, ustats_df)
    }
    stats_df <- stats_df[!is.na(stats_df$value), ]

    pds <- lapply(distro_cols, function(x) {

        p_data <- stats_df[stats_df$stat == x, ]
        p_data$order <- seq_len(nrow(p_data))

        ## Clean variable names for plotting
        p_data$stat <- gsub("_", " ", gsub(
            "(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", p_data$stat, perl = TRUE
        ))

        ## Scale to thousands (K), millions (M), or billions (G)?
        if(max(p_data$value) > 1e9) {
            s <- scale_y_continuous(labels = label_number(suffix = " G", scale = 1e-9))
        } else if(max(p_data$value) > 1e6) {
            s <- scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))
        } else if(max(p_data$value) > 1e3) {
            s <- scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))
        } else {
            s <- NULL
        }

        p <- ggplot(
            p_data, aes(x = .data$stat, y = .data$value)
        ) +
            ggbeeswarm::geom_quasirandom(
                aes(
                    color = ifelse(.data$highlight, "brown2", "deepskyblue4"),
                    alpha = ifelse(.data$highlight, 1, 0.4)
                ), show.legend = FALSE
            ) +
            scale_color_identity() +
            scale_alpha_identity() +
            theme_minimal() +
            labs(x = "", y = "", subtitle = unique(p_data$stat)) +
            theme(axis.text.x = element_blank()) +
            s

        return(p)
    })

    # Add title for each category and combine plots into one
    pds[[6]] <- pds[[6]] + ggtitle("Contig")
    pds[[9]] <- pds[[9]] + ggtitle("Scaffold")
    p_all <- wrap_plots(
        wrap_plots(c(list(p_al), pds[1:5]), nrow = 1),
        wrap_plots(
            wrap_plots(pds[6:8], nrow = 1),
            wrap_plots(pds[9:11], nrow = 1),
            nrow = 1
        ),
        ncol = 1
    )

    return(p_all)

}


