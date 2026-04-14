test_that("init_mirai works with basic settings", {
  skip_if_not_installed("mirai")

  n <- init_mirai(n_cores = 2, verbose = FALSE)
  expect_equal(n, 2)

  cleanup_mirai(verbose = FALSE)
})

test_that("auto_init_mirai exports variables", {
  skip_if_not_installed("mirai")

  test_var <- 42
  test_func <- function(x) x * 2

  expect_silent({
    auto_init_mirai(test_var, test_func, n_cores = 2, verbose = FALSE)
  })

  cleanup_mirai(verbose = FALSE)
})

test_that("cleanup_mirai works", {
  skip_if_not_installed("mirai")

  init_mirai(n_cores = 2, verbose = FALSE)
  expect_silent(cleanup_mirai(verbose = FALSE))
})
