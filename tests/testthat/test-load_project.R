test_that("load_project loads config and keeps working directory", {
  temp_dir <- tempfile("load_project")

  create_project_structure(
    path = temp_dir,
    type = "analysis",
    create_project_file = FALSE
  )

  old_wd <- getwd()
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  result <- load_project(temp_dir)

  expect_type(result, "list")
  expect_equal(
    normalizePath(result$path, winslash = "/"),
    normalizePath(temp_dir, winslash = "/")
  )
  expect_equal(getwd(), old_wd)

  cfg <- get_project_config()
  expect_true(is.list(cfg))
  expect_equal(cfg$project$name, basename(temp_dir))
})
