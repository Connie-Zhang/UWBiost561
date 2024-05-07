context("Testing generate_partial_clique")

test_that("generate_partial_clique works for small matrix with medium
          clique_fraction and large clique_edge_density",{
  set.seed(10)
  res <- generate_partial_clique(n=10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)
  expect_true(is.list(res)) # output is a list
  expect_true(is.matrix(res$adj_mat)) # output is a matrix
  expect_true(all(dim(res$adj_mat)==c(10,10))) # output retains original dimension
  expect_true(all.equal(t(res$adj_mat), res$adj_mat)) # output is a symmetric matrix
  expect_true(sum(res$adj_mat)>=10^2*0.5^2*0.9) # output has the correct number of edges
})

test_that("generate_partial_clique works for big matrix with small clique_fraction
          and medium clique_edge_density", {
  set.seed(10)
  res <- generate_partial_clique(n=50,
                                 clique_fraction = 0.1,
                                 clique_edge_density = 0.7)
  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat)==c(50,50)))
  expect_true(all.equal(t(res$adj_mat), res$adj_mat))
  expect_true(sum(res$adj_mat)>=50^2*0.1^2*0.7)
})
