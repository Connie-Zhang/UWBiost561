#' Generate a partial clique in a matrix
#'
#' Generate a symmetric adjacent matrix of the specified size
#' Replace the upper left corner of the original matrix with 1s by the clique_edge_density to create a partial clique. Symmetrize the matrix.
#' Randomize the matrix and keep track of the order of randomization
#'
#' @param n the size of the matrix
#'
#' @param clique_fraction the fraction of the matrix that should be a clique
#' @param clique_edge_density the edge density of the clique
#'
#' @return a list containing the matrix with a partial clique, the randomized matrix, and the order of randomization
#' @export
generate_partial_clique <- function(n,clique_fraction=0.5,clique_edge_density=1){
  stopifnot(n %% 1 == 0, n >= 0,
            clique_fraction >= 0, clique_fraction <= 1,
            clique_edge_density >= 0, clique_edge_density <= 1)

  adj_mat <- matrix(sample(x = c(0,1),
                           size = n^2,
                           prob = c(0.5, 0.5),
                           replace = TRUE),
                    nrow = n, ncol = n)
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1

  clique_size <- round(n*clique_fraction)
  partial_clique <- matrix(sample(x=c(0,1),
                                  size=clique_size^2,
                                  prob=c(1-clique_edge_density,clique_edge_density),replace=TRUE),
                                  nrow=clique_size,ncol=clique_size)
  partial_clique <- partial_clique+ t(partial_clique)
  partial_clique[partial_clique>0] <- 1
  diag(partial_clique) <- 1
  adj_mat[1:clique_size, 1:clique_size] <- partial_clique

  #symmetrize the adjacent matrix with partial clique
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1

  sample_idx <- sample(1:n)
  rev_mat <- adj_mat[sample_idx, sample_idx]

  rev_order <- sapply(1:n, function(i){
    which(sample_idx == i)
  })

  return(list(adj_mat=adj_mat, rev_mat = rev_mat,rev_order=rev_order))
}


