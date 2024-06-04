#' Reorder adjacency matrix
#'
#' Reorder the adjacency matrix according to the given order.
#'
#' @param adj_mat a symmetric adjacency matrix
#' @param order a permutation of the row and column indices
#'
#' @return a symmetric adjacency matrix with rows and columns reordered according to the given order
#' @export
#'
#' @examples
#' adj_mat <- generate_partial_clique(10,0.5,0.5)$adj_mat
#' order <- sample(10,10,replace=FALSE)
#' reorder_clique(adj_mat, order)
reorder_clique <- function(adj_mat, order){
  stopifnot(is.matrix(adj_mat), is.numeric(order))
  stopifnot(nrow(adj_mat) == ncol(adj_mat), length(order) == nrow(adj_mat))
  return(adj_mat[order, order])
}
