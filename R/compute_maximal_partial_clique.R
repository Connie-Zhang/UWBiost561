#' @param clique fraction, clique edge density
#' @return a list
#' @export
'%!in%' <- function(x,y)!('%in%'(x,y))
compute_maximal_partial_clique <- function(adj_mat, alpha){
  stopifnot(!!c(0,1) %in% adj_mat,
            t(adj_mat)==adj_mat,
            diag(adj_mat)==1,
            is.null(rownames(adj_mat)), is.null(colnames(adj_mat)),
            nrow(adj_mat) >= 5, nrow(adj_mat) <= 50,
            ncol(adj_mat) >= 5, ncol(adj_mat) <= 50,
            length(alpha)==1, alpha >= 0.5, alpha <= 1)
  # compute where the edges are
  # edges <- list()
  # for (i in 1:nrow(adj_mat)){
  #   edges[[i]] <- which(adj_mat[i,]==1)
  # }
  # start from row1 and find the connection to the other nodes
  partial_cliques <- list()
  partial_cliques[[1]] <- adj_mat[1,]
  for (i in 2:nrow(adj_mat)){
    for(j in 1:length(partial_cliques)){
      if (1 %in% adj_mat[i,unique(partial_cliques[[j]])]){
        partial_cliques[[j]] <- c(partial_cliques[[j]], adj_mat[i,])
      }
      else{
        partial_cliques[[length(partial_cliques)+1]] <- adj_mat[i,]
      }
    }
  }
  partial_clique_sizes <- sapply(partial_cliques,length)
  clique_idx <- unique(partial_cliques[[which(partial_clique_sizes==max(partial_clique_sizes))]])
  # compute the edge density
  edge_density <- sum(adj_mat[clique_idx, clique_idx])/length(clique_idx)^2
  # first row is its own clique
  # check if second row connect with first row, if not start its own clique
  # check if third row connect with 1 and 2, if it does merge to old cliques
  # repeat for all rows
  return(list(clique_idx = clique_idx, edge_density = edge_density))
}



