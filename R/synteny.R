
#' Assess synteny network based on clustering coefficient and node frequency
#'
#' @param synnet Edgelist for the synteny network in a 2-column data frame,
#' with variables \strong{anchor1} and \strong{anchor2} representing
#' names of loci in anchor 1 and anchor 2, respectively.
#' @param cc_type Type of clustering coefficient to be calculated.
#' One of 'global' or 'average'. Default: 'average'.
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{CC}{Numeric representing clustering coefficient.}
#'   \item{Node_number}{Numeric representing number of nodes in the network.}
#'   \item{Score}{Numeric representing network score, which is
#'   the product of 'CC' and 'Node_number'.}
#' }
#' @importFrom igraph graph_from_data_frame transitivity vcount
#' @export
#' @rdname assess_synnet
#' @examples
#' data(synnet)
#' assess_synnet(synnet)
assess_synnet <- function(synnet = NULL, cc_type = "average") {

    if(sum(c("anchor1", "anchor2") %in% names(synnet)) != 2) {
        stop("Could not find variables 'anchor1' and 'anchor2' in synnet.")
    }
    graph <- igraph::graph_from_data_frame(synnet, directed = FALSE)

    # Calculate clustering coefficient
    cc <- igraph::transitivity(graph, type = cc_type)

    # Number of nodes
    nn <- igraph::vcount(graph)

    # Summary stats
    stats <- data.frame(
        CC = cc,
        Node_number = nn,
        Score = cc * nn
    )
    return(stats)
}


#' Assess list of synteny networks as in \code{assess_synnet}
#'
#' @param synnet_list A list of networks, each network being
#' an edgelist as a 2-column data frame, with variables \strong{anchor1}
#' and \strong{anchor2} representing names of loci in anchor 1 and
#' anchor 2, respectively.
#' @param cc_type Type of clustering coefficient to be calculated.
#' One of 'global' or 'average'. Default: 'average'.
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{CC}{Numeric representing clustering coefficient.}
#'   \item{Node_number}{Numeric representing number of nodes in the network.}
#'   \item{Score}{Numeric representing network score, which is
#'   the product of 'CC' and 'Node_number'.}
#'   \item{Network}{Character of network name.}
#' }
#' @importFrom methods is
#' @export
#' @rdname assess_synnet_list
#' @examples
#' set.seed(123)
#' data(synnet)
#' net1 <- synnet
#' net2 <- synnet[-sample(1:10000, 500), ]
#' net3 <- synnet[-sample(1:10000, 1000), ]
#' synnet_list <- list(net1 = net1, net2 = net2, net3 = net3)
#' assess_synnet_list(synnet_list)
assess_synnet_list <- function(synnet_list = NULL, cc_type = "average") {

    if(!methods::is(synnet_list, "list")) {
        stop("Argument 'synnet_list' is not a list.")
    }

    if(is.null(names(synnet_list))) {
        names(synnet_list) <- paste0("network", seq_along(synnet_list))
    }

    stats <- Reduce(rbind, lapply(seq_along(synnet_list), function(x) {
        s <- assess_synnet(synnet_list[[x]], cc_type = cc_type)
        s$Network <- names(synnet_list)[x]
        return(s)
    }))
    return(stats)
}



