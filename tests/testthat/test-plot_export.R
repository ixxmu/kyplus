# tests/testthat/test-plot_export.R

test_that("plot_export works with basic ggplot", {
  skip_if_not_installed("ggplot2")
  
  library(ggplot2)
  
  # Create a simple plot
  p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()
  
  # Create temporary directory for test files
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_plot")
  
  # Test basic export
  result <- plot_export(p, test_file, verbose = FALSE)
  
  expect_true(file.exists(paste0(test_file, ".pdf")))
  expect_true(file.exists(paste0(test_file, ".jpg")))
  expect_length(result, 2)
  
  # Clean up
  file.remove(result)
})

test_that("quality settings work correctly", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  temp_dir <- tempdir()

  # Test different quality settings
  qualities <- c("low", "medium", "high")
  for (q in qualities) {
    test_file <- file.path(temp_dir, paste0("test_", q))
    result <- plot_export(p, test_file, quality = q, verbose = FALSE)

    expect_true(file.exists(paste0(test_file, ".pdf")))
    expect_true(file.exists(paste0(test_file, ".jpg")))

    # Clean up
    file.remove(result)
  }
})

test_that("plot_export rejects invalid quality values", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()

  expect_error(
    plot_export(p, tempfile("bad_quality"), quality = "ultra", verbose = FALSE),
    "quality must be one of"
  )
})

test_that("plot_export rejects non-ggplot objects", {
  expect_error(
    plot_export(list(a = 1), tempfile("bad_plot"), verbose = FALSE),
    "plot_object must be a ggplot object"
  )
})