# Write this content to "tests/testthat/test-create_project.R"
test_that("project creation works correctly", {
  # Create a temporary directory for testing
  temp_dir <- tempfile("test_project")

  # Test analysis project creation
  result <- create_project_structure(
    path = temp_dir,
    type = "analysis",
    create_project_file = TRUE
  )

  # Verify return value
  expect_type(result, "list")
  expect_equal(result$type, "analysis")

  # Verify directory structure exists
  expect_true(dir.exists(file.path(temp_dir, "R")))
  expect_true(dir.exists(file.path(temp_dir, "data")))
  expect_true(dir.exists(file.path(temp_dir, "docs")))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
