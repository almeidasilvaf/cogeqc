
#' Goodness of fit test for the scale-free topology model
#'
#' @param edges A 2-column data frame with network edges represented in each.
#' Columns 1 and 2 represent nodes 1 and 2 of each edge.
#'
#' @return A numeric scalar with the R squared for the scale-free topology fit.
#'
#' @importFrom igraph graph_from_data_frame degree
#' @importFrom stats lm
#' @importFrom graphics hist
#' @rdname fit_sft
#' @export
#' @examples
#' data(synnet)
#' edges <- synnet
#' fit_sft(edges)
fit_sft <- function(edges) {

    # Get degree distribution
    g <- igraph::graph_from_data_frame(edges[, c(1, 2)], directed = FALSE)
    k <- igraph::degree(g)

    # Fit scale-free topology model
    disk <- cut(k, 10)
    dk <- tapply(k, disk, mean)
    pdk <- as.numeric(tapply(k, disk, length) / length(k))
    breaks1 <- seq(from = min(k), to = max(k), length = 11)
    hist1 <- hist(k, breaks = breaks1, plot = FALSE, right = TRUE)
    dk2 <- hist1$mids
    dk <- ifelse(is.na(dk), dk2, dk)
    dk <- ifelse(dk == 0, dk2, dk)
    pdk <- ifelse(is.na(pdk), 0, pdk)
    logdk <- as.numeric(log10(dk))
    log_pdk <- as.numeric(log10(pdk + 1e-09))

    # Get R^2
    r2 <- NA
    r2 <- summary(lm(log_pdk ~ logdk))$r.squared
    return(r2)
}


#' Assess synteny network based on graph properties
#'
#' @param synnet Edge list for the synteny network in a 2-column data frame,
#' with columns 1 and 2 representing names of loci in anchor 1 and
#' anchor 2, respectively.
#' @param cc_type Type of clustering coefficient to be calculated.
#' One of 'global' or 'average'. Default: 'average'.
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{CC}{Numeric representing clustering coefficient.}
#'   \item{Node_count}{Numeric representing number of nodes in the network.}
#'   \item{Rsquared}{Numeric indicating the coefficient of determination
#'   for the scale-free topology fit.}
#'   \item{Score}{Numeric representing network score, which is
#'   the product of 'CC' and 'Node_number'.}
#' }
#'
#' @details Network score is the product of the network's clustering
#' coefficient, node count, and R squared for the scale-free topology fit.
#'
#' @importFrom igraph graph_from_data_frame transitivity vcount
#' @importFrom methods is
#' @export
#' @rdname assess_synnet
#' @examples
#' data(synnet)
#' assess_synnet(synnet)
assess_synnet <- function(synnet = NULL, cc_type = "average") {

    if(!is(synnet, "data.frame") | ncol(synnet) != 2) {
        stop("'synnet' must be a 2-column data frame.")
    }
    names(synnet) <- c("anchor1", "anchor2")
    graph <- igraph::graph_from_data_frame(synnet, directed = FALSE)

    # Calculate clustering coefficient
    cc <- igraph::transitivity(graph, type = cc_type)

    # Number of nodes
    nn <- igraph::vcount(graph)

    # R squared for SFT fit
    r2 <- fit_sft(synnet)

    # Summary stats
    stats <- data.frame(
        CC = cc,
        Node_count = nn,
        Rsquared = r2,
        Score = cc * nn * r2
    )
    return(stats)
}


#' Assess list of synteny networks as in \code{assess_synnet}
#'
#' @param synnet_list A list of networks, each network being
#' an edge list as a 2-column data frame, with columns 1 and 2
#' representing names of loci in anchor 1 and anchor 2, respectively.
#' @param cc_type Type of clustering coefficient to be calculated.
#' One of 'global' or 'average'. Default: 'average'.
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{CC}{Numeric representing clustering coefficient.}
#'   \item{Node_count}{Numeric representing number of nodes in the network.}
#'   \item{Rsquared}{Numeric indicating the coefficient of determination
#'   for the scale-free topology fit.}
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



