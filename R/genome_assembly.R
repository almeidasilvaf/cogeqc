

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
#' @importFrom ggplot2 ggplot aes geom_col ylim xlim theme_bw labs
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
        p <- ggplot(summary_df) +
            geom_col(
                aes(x = .data$Frequency, y = .data$File, fill = .data$Class),
                color = "black"
            ) +
            xlim(c(0, 100)) +
            labs(
                title = "Percentage of BUSCOs for each class across files",
                subtitle = paste0("Lineage dataset: ", lineage),
                y = "Sequence file", x = "% BUSCOs"
            ) +
            geom_text(
                aes(label = .data$Label, x = 50, y = .data$File),
                color = "grey90"
            )

    } else {
        total <- sum(summary_df$Frequency)
        summary_df$Perc <- round((summary_df$Frequency / total) * 100, 2)

        p <- ggplot(summary_df) +
            geom_col(
                aes(x = .data$Class, y = .data$Perc, fill = .data$Class),
                color = "black", show.legend = FALSE
            ) +
            ylim(c(0, 105)) +
            labs(
                title = "Percentage of BUSCOs for each class",
                subtitle = paste0("Lineage dataset: ", lineage, ", N = ", total),
                x = "", y = "% BUSCOs"
            ) +
            geom_text(
                aes(label = .data$Perc, y = .data$Perc, x = .data$Class),
                vjust = -0.5
            )
    }

    p <- p +
        scale_fill_manual(labels = c(
            "Complete & SC", "Complete & duplicate", "Fragmented", "Missing"
        ),
        values = c(
            "#32709a", "#59AAE1", "darkgoldenrod2", "#db5850"
        )) +
        theme_bw()
    return(p)
}


#' Extract columns with useful information from the NCBI Datasets JSON file
#'
#' @param json_df Data frame of search results obtained
#' with \code{jsonlite::read_json(x, simplifyVector = TRUE)$reports}.
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{accession}{character, accession number.}
#'   \item{source}{character, data source.}
#'   \item{species_taxid}{numeric, NCBI Taxonomy ID.}
#'   \item{species_name}{character, species' scientific name.}
#'   \item{species_common_name}{character, species' common name.}
#'   \item{species_ecotype}{character, species' ecotype.}
#'   \item{species_strain}{character, species' strain.}
#'   \item{species_isolate}{character, species' isolate.}
#'   \item{species_cultivar}{character, species' cultivar.}
#'   \item{assembly_level}{factor, assembly level ("Complete", "Chromosome", "Scaffold", or "Contig").}
#'   \item{assembly_status}{character, assembly status.}
#'   \item{assembly_name}{character, assembly name.}
#'   \item{assembly_type}{character, assembly type.}
#'   \item{submission_date}{character, submission date (YYYY-MM-DD).}
#'   \item{submitter}{character, submitter name.}
#'   \item{sequencing_technology}{character, sequencing technology.}
#'   \item{atypical}{logical, indicator of wheter the genome is atypical.}
#'   \item{refseq_category}{character, RefSeq category.}
#'   \item{chromosome_count}{numeric, number of chromosomes.}
#'   \item{sequence_length}{numeric, total sequence length.}
#'   \item{ungapped_length}{numeric, ungapped sequence length.}
#'   \item{contig_count}{numeric, number of contigs.}
#'   \item{contig_N50}{numeric, contig N50.}
#'   \item{contig_L50}{numeric, contig L50.}
#'   \item{scaffold_N50}{numeric, contig N50.}
#'   \item{scaffold_L50}{numeric, contig L50.}
#'   \item{GC_percent}{numeric, GC percentage (0-100).}
#'   \item{annotation_provider}{character, name of annotation provider.}
#'   \item{annotation_release_date}{character, annotation release date (YYYY-MM-DD).}
#'   \item{gene_count_total}{numeric, total number of genes.}
#'   \item{gene_count_coding}{numeric, number of protein-coding genes.}
#'   \item{gene_count_noncoding}{numeric, number of non-coding genes.}
#'   \item{gene_count_pseudogene}{numeric, number of pseudogenes.}
#'   \item{gene_count_other}{numeric, number of other genes.}
#'   \item{CC_ratio}{numeric, ratio of the number of contigs to the number of chromosomes.}
#' }
#'
#' @noRd
extract_columns <- function(json_df) {

    get_col <- function(vec) {
        values <- NA
        if(!is.null(vec)) { values <- vec }
        return(values)
    }
    final_df <- data.frame(
        accession = get_col(json_df$current_accession),
        source = get_col(json_df$source_database),
        species_taxid = get_col(json_df$organism$tax_id),
        species_name = get_col(json_df$organism$organism_name),
        species_common_name = get_col(json_df$organism$common_name),
        species_ecotype = get_col(json_df$organism$infraspecific_names$ecotype),
        species_strain = get_col(json_df$organism$infraspecific_names$strain),
        species_isolate = get_col(json_df$organism$infraspecific_names$isolate),
        species_cultivar = get_col(json_df$organism$infraspecific_names$cultivar),
        assembly_level = get_col(json_df$assembly_info$assembly_level),
        assembly_status = get_col(json_df$assembly_info$assembly_status),
        assembly_name = get_col(json_df$assembly_info$assembly_name),
        assembly_type = get_col(json_df$assembly_info$assembly_type),
        submission_date = get_col(json_df$assembly_info$submission_date),
        submitter = get_col(json_df$assembly_info$submitter),
        sequencing_technology = get_col(json_df$assembly_info$sequencing_tech),
        atypical = get_col(json_df$assembly_info$atypical$is_atypical),
        refseq_category = get_col(json_df$assembly_info$refseq_category),
        chromosome_count = get_col(json_df$assembly_stats$total_number_of_chromosomes),
        sequence_length = as.numeric(get_col(json_df$assembly_stats$total_sequence_length)),
        ungapped_length = as.numeric(get_col(json_df$assembly_stats$total_ungapped_length)),
        contig_count = get_col(json_df$assembly_stats$number_of_contigs),
        contig_N50 = get_col(json_df$assembly_stats$contig_n50),
        contig_L50 = get_col(json_df$assembly_stats$contig_l50),
        scaffold_count = get_col(json_df$assembly_stats$number_of_scaffolds),
        scaffold_N50 = get_col(json_df$assembly_stats$scaffold_n50),
        scaffold_L50 = get_col(json_df$assembly_stats$scaffold_l50),
        GC_percent = get_col(json_df$assembly_stats$gc_percent),
        annotation_provider = get_col(json_df$annotation_info$provider),
        annotation_release_date = get_col(json_df$annotation_info$release_date),
        gene_count_total = get_col(json_df$annotation_info$stats$gene_counts$total),
        gene_count_coding = get_col(json_df$annotation_info$stats$gene_counts$protein_coding),
        gene_count_noncoding = get_col(json_df$annotation_info$stats$gene_counts$non_coding),
        gene_count_pseudogene = get_col(json_df$annotation_info$stats$gene_counts$pseudogene),
        gene_count_other = get_col(json_df$annotation_info$stats$gene_counts$other)
    )

    final_df$atypical[is.na(final_df$atypical)] <- FALSE
    final_df$source <- gsub("SOURCE_DATABASE_", "", final_df$source)
    final_df$CC_ratio <- final_df$contig_count / final_df$chromosome_count
    final_df$assembly_level <- factor(
        final_df$assembly_level, levels = c(
            "Complete", "Chromosome", "Scaffold", "Contig"
        )
    )

    return(final_df)
}


#' Get summary statistics for genomes on NCBI using the NCBI Datasets API
#'
#' @param taxon Taxon for which summary statistics will be
#' retrieved, either as a character scalar (e.g., "brassicaceae") or
#' as a numeric scalar representing NCBI Taxonomy ID (e.g., 3700).
#' @param filters (optional) A list of filters to use when querying the API
#' in the form of key-value pairs, with keys in list names and values in list
#' elements (e.g., \code{list(filters.reference_only = "true")}, see
#' examples for details).
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{accession}{character, accession number.}
#'   \item{source}{character, data source.}
#'   \item{species_taxid}{numeric, NCBI Taxonomy ID.}
#'   \item{species_name}{character, species' scientific name.}
#'   \item{species_common_name}{character, species' common name.}
#'   \item{species_ecotype}{character, species' ecotype.}
#'   \item{species_strain}{character, species' strain.}
#'   \item{species_isolate}{character, species' isolate.}
#'   \item{species_cultivar}{character, species' cultivar.}
#'   \item{assembly_level}{factor, assembly level ("Complete", "Chromosome", "Scaffold", or "Contig").}
#'   \item{assembly_status}{character, assembly status.}
#'   \item{assembly_name}{character, assembly name.}
#'   \item{assembly_type}{character, assembly type.}
#'   \item{submission_date}{character, submission date (YYYY-MM-DD).}
#'   \item{submitter}{character, submitter name.}
#'   \item{sequencing_technology}{character, sequencing technology.}
#'   \item{atypical}{logical, indicator of wheter the genome is atypical.}
#'   \item{refseq_category}{character, RefSeq category.}
#'   \item{chromosome_count}{numeric, number of chromosomes.}
#'   \item{sequence_length}{numeric, total sequence length.}
#'   \item{ungapped_length}{numeric, ungapped sequence length.}
#'   \item{contig_count}{numeric, number of contigs.}
#'   \item{contig_N50}{numeric, contig N50.}
#'   \item{contig_L50}{numeric, contig L50.}
#'   \item{scaffold_N50}{numeric, contig N50.}
#'   \item{scaffold_L50}{numeric, contig L50.}
#'   \item{GC_percent}{numeric, GC percentage (0-100).}
#'   \item{annotation_provider}{character, name of annotation provider.}
#'   \item{annotation_release_date}{character, annotation release date (YYYY-MM-DD).}
#'   \item{gene_count_total}{numeric, total number of genes.}
#'   \item{gene_count_coding}{numeric, number of protein-coding genes.}
#'   \item{gene_count_noncoding}{numeric, number of non-coding genes.}
#'   \item{gene_count_pseudogene}{numeric, number of pseudogenes.}
#'   \item{gene_count_other}{numeric, number of other genes.}
#'   \item{CC_ratio}{numeric, ratio of the number of contigs to the number of chromosomes.}
#' }
#'
#' @details
#' Possible filters for the \strong{filters} parameter can be accessed
#' at https://www.ncbi.nlm.nih.gov/datasets/docs/v2/reference-docs/rest-api/#get-/genome/taxon/-taxons-/dataset_report.
#'
#' @importFrom jsonlite read_json
#' @export
#' @rdname get_genome_stats
#' @examples
#' # Example 1: Search for A. thaliana genomes by tax ID
#' ex1 <- get_genome_stats(taxon = 3702)
#'
#' # Example 2: Search for A. thaliana genomes by name
#' ex2 <- get_genome_stats(taxon = "Arabidopsis thaliana")
#'
#' # Example 3: Search for chromosome-level Brassicaeae genomes
#' ex3 <- get_genome_stats(
#'     taxon = "brassicaceae",
#'     filters = list(filters.assembly_level = "chromosome")
#' )
#'
get_genome_stats <- function(taxon = NULL, filters = NULL) {

    if(is.null(taxon)) { stop("Please, specify an input taxon.") }
    taxon <- gsub(" ", "%20", taxon) # replace space with "%20"

    # Read JSON file from the NCBI Datasets API
    base_url <- url <- paste0(
        "https://api.ncbi.nlm.nih.gov/datasets/v2alpha/genome/taxon/",
        taxon, "/dataset_report?page_size=1000"
    )
    # Apply user-defined filters (if any)
    if(is.list(filters)) {
        params <- data.frame(key = names(filters), value = unlist(filters))
        params <- paste(params$key, params$value, sep = "=")
        params <- paste(params, collapse = "&")
        base_url <- paste0(base_url, "&", params)
    }
    js <- jsonlite::read_json(base_url, simplifyVector = TRUE)
    if(length(js) < 2) { stop("Could not find any result for your search.") }

    # Handle requests of more than 1000 genomes
    j_n <- NULL
    if(js$total_count > 1000) {
        n <- ceiling(js$total_count / 1000) - 1
        token <- js$next_page_token

        j_n <- vector("list", length = n)
        for(page in seq_len(n)) {
            ## Read resulting JSON file
            result <- jsonlite::read_json(
                paste0(base_url, "&page_token=", token), simplifyVector = TRUE
            )
            ## Update `j_n` and `token`
            j_n[[page]] <- extract_columns(result$reports)
            if("next_page_token" %in% names(result)) {
                token <- result$next_page_token
            }
        }

        j_n <- Reduce(rbind, j_n)

    }

    final_js <- rbind(extract_columns(js$reports), j_n)
    return(final_js)

}


#' Compare user-defined assembly statistics with statistics of NCBI genomes
#'
#' This function helps users analyze their genome assembly stats in a context
#' by comparing metrics obtained by users with "reference" metrics in
#' closely-related organisms.
#'
#' @param ncbi_stats A data frame of summary statistics for a particular
#' taxon obtained from the NCBI, as obtained with the
#' function \code{get_genome_stats}.
#' @param user_stats A data frame with assembly statistics obtained by
#' the user. A column named \strong{accession} is mandatory, and it must contain
#' unique identifiers for the genome(s) analyzed by the user. Dummy variables
#' can be used as identifiers (e.g., "my_genome_001"), as long as they are
#' unique. All other column containing assembly stats must have the same names
#' as their corresponding columns in the data frame specified
#' in \strong{ncbi_stats}. For instance, stats on total number of genes and
#' sequence length must be in columns named "gene_count_total" and
#' "sequence_length", as in the \strong{ncbi_stats} data frame.
#'
#' @details
#' For each genome assembly statistic (e.g., "gene_count_total"), values
#' in \strong{user_stats} are compared to a distribution of values
#' from \strong{ncbi_stats}, and their percentile and rank in the distributions
#' are reported.
#'
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{accession}{character, unique identifier as in user_stats$accession.}
#'   \item{variable}{character, name of the genome assembly metric (e.g., "CC_ratio").}
#'   \item{percentile}{numeric, percentile in the distribution.}
#'   \item{rank}{numeric, rank in the distribution (highest to lowest). For the variable "CC_ratio", ranks go from lowest to highest.}
#' }
#' @importFrom stats ecdf
#' @importFrom utils tail
#' @export
#' @rdname compare_genome_stats
#' @examples
#' # Use case: user assembled a maize (Zea mays) genome
#'
#' ## Obtain stats for maize genomes on the NCBI
#' ncbi_stats <- get_genome_stats(taxon = "Zea mays")
#'
#' ## Create a data frame of stats for fictional maize genome
#' user_stats <- data.frame(
#'     accession = "my_lovely_maize",
#'     sequence_length = 2.4 * 1e9,
#'     gene_count_total = 50000,
#'     CC_ratio = 1
#' )
#'
#' # Compare stats
#' compare_genome_stats(ncbi_stats, user_stats)
#'
compare_genome_stats <- function(ncbi_stats = NULL, user_stats = NULL) {

    # Checking data format
    if(is.null(ncbi_stats) | is.null(user_stats)) {
        stop("Parameters 'ncbi_stats' and 'user_stats' are mandatory.")
    }
    if(!"accession" %in% names(user_stats)) {
        stop("Could not find 'accession' column in 'user_stats' data frame.")
    }
    diffs <- setdiff(names(user_stats), names(ncbi_stats))
    if(length(diffs) > 0) {
        stop("Column '", diffs, "' from 'user_stats' is not in 'ncbi_stats'")
    }

    # Compare stats
    comparison_df <- Reduce(rbind, lapply(names(user_stats)[-1], function(x) {

        all_values <- c(ncbi_stats[, x], user_stats[, x])
        cdf <- ecdf(all_values)
        perc <- cdf(user_stats[, x])
        r <- tail(rank(-all_values), nrow(user_stats))
        if(x == "CC_ratio") { r <- tail(rank(all_values), nrow(user_stats)) }

        df <- data.frame(
            accession = user_stats$accession,
            variable = x,
            percentile = perc,
            rank = r
        )
        return(df)
    }))

    return(comparison_df)
}




