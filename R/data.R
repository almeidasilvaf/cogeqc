

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


