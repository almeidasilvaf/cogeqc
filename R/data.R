

#' Orthogroups between Arabidopsis thaliana and Brassica oleraceae
#'
#' Data obtained from PLAZA Dicots 5.0.
#'
#' @name og
#' @format A 3-column data frame with the following variables:
#' \describe{
#'   \item{Orthogroup}{Orthogroup ID.}
#'   \item{Species}{Abbreviation for species' name.}
#'   \item{Gene}{Gene ID}
#' }
#' @references
#' Van Bel, M., Silvestri, F., Weitz, E. M., Kreft, L., Botzki, A.,
#' Coppens, F., & Vandepoele, K. (2021). PLAZA 5.0: extending the scope
#' and power of comparative and functional genomics in plants.
#' Nucleic acids research.
#' @examples
#' data(og)
#' @usage data(og)
"og"


#' Intepro annotation for Arabidopsis thaliana's genes
#'
#' The annotation data were retrieved from PLAZA Dicots 5.0.
#'
#' @name interpro_ath
#' @format A 2-column data frame:
#' \describe{
#'   \item{Gene}{Character of gene IDs.}
#'   \item{Annotation}{Character of Interpro domains.}
#' }
#' @references
#' Van Bel, M., Silvestri, F., Weitz, E. M., Kreft, L., Botzki, A.,
#' Coppens, F., & Vandepoele, K. (2021). PLAZA 5.0: extending the scope
#' and power of comparative and functional genomics in plants.
#' Nucleic acids research.
#' @examples
#' data(interpro_ath)
#' @usage data(interpro_ath)
"interpro_ath"


#' Intepro annotation for Brassica oleraceae's genes
#'
#' The annotation data were retrieved from PLAZA Dicots 5.0.
#'
#' @name interpro_bol
#' @format A 2-column data frame:
#' \describe{
#'   \item{Gene}{Character of gene IDs.}
#'   \item{Annotation}{Character of Interpro domains.}
#' }
#' @references
#' Van Bel, M., Silvestri, F., Weitz, E. M., Kreft, L., Botzki, A.,
#' Coppens, F., & Vandepoele, K. (2021). PLAZA 5.0: extending the scope
#' and power of comparative and functional genomics in plants.
#' Nucleic acids research.
#' @examples
#' data(interpro_bol)
#' @usage data(interpro_bol)
"interpro_bol"


#' BUSCO summary output for batch mode
#'
#' This object was created with the function \code{read_busco()} using
#' a batch run of BUSCO on the genomes of Herbaspirillum seropedicae SmR1
#' and Herbaspirillum rubrisubalbicans M1.
#'
#' @name batch_summary
#' @format A 2-column data frame with the following variables:
#' \describe{
#'   \item{Class}{Factor of BUSCO classes}
#'   \item{Frequency}{Numeric with the percentage of BUSCOs in each class.}
#'   \item{Lineage}{Character with the lineage dataset used.}
#'   \item{File}{Character with the name of the FASTA file used.}
#' }
#' @examples
#' data(batch_summary)
#' @usage data(batch_summary)
"batch_summary"


#' Species tree for model species
#'
#' The data used to create this object was retrieved from Orthofinder's
#' example output for model species, available in
#' https://bioinformatics.plants.ox.ac.uk/davidemms/public_data/.
#'
#' @name tree
#' @format An object of class "phylo" as returned by \code{treeio::read.tree()}.
#' @references
#' Emms, D. M., & Kelly, S. (2019). OrthoFinder: phylogenetic orthology
#' inference for comparative genomics. Genome biology, 20(1), 1-14.
#' @examples
#' data(tree)
#' @usage data(tree)
"tree"


#' Synteny network for Brassica oleraceae, B. napus, and B. rapa
#'
#' @name synnet
#' @format A 2-column data frame with the variables \strong{anchor1}
#' and \strong{anchor2}, containing names of loci in anchor 1 and anchor 2,
#' respectively.
#' @references
#' Zhao, T., & Schranz, M. E. (2019). Network-based microsynteny
#' analysis identifies major differences and genomic outliers
#' in mammalian and angiosperm genomes. Proceedings of the National Academy
#' of Sciences, 116(6), 2165-2174.
#' @examples
#' data(synnet)
#' @usage data(synnet)
"synnet"
