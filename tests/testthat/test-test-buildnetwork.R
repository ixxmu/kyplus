

# Tests (place in tests/testthat/test-build_regulatory_network.R)
library(testthat)
library(kyplus)

test_that("build_regulatory_network works with correlation method", {
  set.seed(123)
  expr_matrix <- matrix(rnorm(100), nrow=10)
  colnames(expr_matrix) <- paste0("gene", 1:10)

  result <- build_regulatory_network(
    expr_matrix,
    method = "correlation",
    threshold = 0.3
  )

  expect_type(result, "list")
  expect_s4_class(result$graph, "graphNEL")
  expect_equal(dim(result$adjacency_matrix), c(10, 10))
})

test_that("build_regulatory_network handles collinearity", {
  set.seed(123)
  expr_matrix <- matrix(rnorm(100), nrow=10)
  colnames(expr_matrix) <- paste0("gene", 1:10)

  result <- build_regulatory_network(
    expr_matrix,
    method = "pcor",
    threshold = 0.3,
    handle_colinearity = TRUE
  )

  expect_type(result, "list")
  expect_s4_class(result$graph, "graphNEL")
  expect_false(any(is.na(result$strength_matrix)))
})
