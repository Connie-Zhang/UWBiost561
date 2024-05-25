context("Testing simulation")

test_that("test that simulation stops if given incorrect input",{
  set.seed(10)
  expect_error(simulation(c(0.7,0.7),2,1:25))
  expect_error(simulation(c(0.5,0.2),1,1:25))
  expect_error(simulation(c(0.5,0.2),2,1:3))
})

test_that("test that simulation returns a list of trials for different partial clique densities",{
  set.seed(10)
  result <- simulation(c(0.1,0.2),2,1:25)
  expect_is(result,"list")
  expect_is(result[[1]],"list")
  expect_is(result[[2]],"list")
  expect_is(result[[1]][[1]],"list")
  expect_is(result[[1]][[2]],"list")
  expect_is(result[[2]][[1]],"list")
  expect_is(result[[2]][[2]],"list")
})

