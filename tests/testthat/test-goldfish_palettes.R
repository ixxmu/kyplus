test_that("goldfish_colors returns correct colors", {
  # Test basic functionality
  colors <- goldfish_colors("classic")
  expect_type(colors, "character")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  
  # Test with n parameter
  colors_3 <- goldfish_colors("classic", n = 3)
  expect_length(colors_3, 3)
  
  # Test reverse
  colors_rev <- goldfish_colors("classic", reverse = TRUE)
  expect_equal(colors_rev, rev(goldfish_colors("classic")))
  
  # Test invalid palette
  expect_error(goldfish_colors("invalid"))
})

test_that("goldfish scales work with ggplot2", {
  skip_if_not_installed("ggplot2")
  
  library(ggplot2)
  
  # Test discrete scale
  p1 <- ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
    geom_point() +
    scale_color_goldfish()
  
  expect_s3_class(p1, "ggplot")
  
  # Test continuous scale
  p2 <- ggplot(mtcars, aes(x = mpg, y = hp, color = wt)) +
    geom_point() +
    scale_color_goldfish_c()
  
  expect_s3_class(p2, "ggplot")
})