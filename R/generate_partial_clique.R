#' @param clique fraction, clique edge density
#' @return a list
#' @export
generate_partial_clique <- function(n,clique_fraction=0.5,clique_edge_density=1){
  stopifnot(n %% 1 == 0, n >= 0,
            clique_fraction >= 0, clique_fraction <= 1,
            clique_edge_density >= 0, clique_edge_density <= 1)

  adj_mat <- matrix(sample(x = c(0,1),
                           size = n^2,
                           prob = c(1-0.1, 0.1),
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
  partial_clique <- partial_cluqe+ t(partial_clique)
  partial_clique[partial_clique>0] <- 1
  diag(partial_clique) <- 1
  adj_mat[1:clique_size, 1:clique_size] <- partial_clique

  sample_idx <- sample(1:n)
  adj_mat <- adj_mat[sample_idx, sample_idx]

  rev_order <- sapply(1:n, function(i){
    which(sample_idx == i)
  })

  return(list(adj_mat=adj_mat, rev_order=rev_order))
}


