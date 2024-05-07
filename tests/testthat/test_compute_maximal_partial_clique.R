context("Testing compute_maximal_partial_clique")

test_that("compute_maximal_partial_clique works",{
  set.seed(10)
  simulation <- generate_partial_clique(
    n = 30,
    clique_fraction = 0.5,
    clique_edge_density = 0.7
  )

  adj_mat <- simulation$adj_mat

  res <- compute_maximal_partial_clique(
    adj_mat = adj_mat,
    alpha = 0.7
  )

  expect_true(is.list(res))
  expect_true(length(res$clique_idx)<=30)
  expect_true(res$edge_density>=0.7)
})

test_that("compute_maximal_partial_clique stops if given incorrect input",{
  set.seed(10)
  simulation <- cbind(c(1,0,2),c(0,1,0),c(2,0,1))
  simulation2 <- rep(c(1,0,1,0,1),50)

  expect_error(compute_maximal_partial_clique(
    adj_mat = simulation,
    alpha = 0.7
  ))
  expect_error(compute_maximal_partial_clique(
    adj_mat = simulation,
    alpha = 1.7
  ))
  expect_error(compute_maximal_partial_clique(
    adj_mat = simulation2,
    alpha = 0.7
  ))
})

test_that("compute_maximal_partial_clique takes less than 30 seconds to compute a 30 by 30 matrix",{
  set.seed(10)
  simulation <- generate_partial_clique(
    n = 30,
    clique_fraction = 0.5,
    clique_edge_density = 0.7
  )

  adj_mat <- simulation$adj_mat

  start_time <- Sys.time()
  res <- compute_maximal_partial_clique(
    adj_mat = adj_mat,
    alpha = 0.7
  )
  end_time <- Sys.time()

  expect_true(difftime(end_time, start_time, units = "secs") < 30)
})

test_that("compute_maximal_partial_clique work for matrices of all 1s",{
  set.seed(10)

  simulation2 <- matrix(1, nrow = 30, ncol = 30)

  res2 <- compute_maximal_partial_clique(
    adj_mat = simulation2,
    alpha = 0.7
  )

  expect_true(is.list(res2))
  expect_true(length(res2$clique_idx)==30)
  expect_true(res2$edge_density==1)
})

test_that("compute_maximal_partial_clique will find the maximal clique with edge
          density greater than or equal to alpha",{
  set.seed(10)
  simulation <- generate_partial_clique(
    n = 30,
    clique_fraction = 0.5,
    clique_edge_density = 0.7
    )
  simulation2 <- generate_partial_clique(
    n =25,
    clique_fraction = 0.5,
    clique_edge_density = 0.5)

  alpha <- c(0.7,0.5)
  res <- compute_maximal_partial_clique(
    adj_mat = simulation$adj_mat,
    alpha = alpha[1]
  )
  res2 <- compute_maximal_partial_clique(
    adj_mat = simulation2$adj_mat,
    alpha = alpha[2]
  )
  m <- c(length(res$clique_idx), length(res2$clique_idx))


  expect_true((sum(simulation$adj_mat[res$clique_idx,res$clique_idx])-m[1])/2 >= alpha[1]*m[1]*(m[1]-1)/2)
  expect_true((sum(simulation2$adj_mat[res2$clique_idx,res2$clique_idx])-m[2])/2 >= alpha[2]*m[2]*(m[2]-1)/2)
  })
