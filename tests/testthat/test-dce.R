test_that("build_regulatory_network works with different methods", {
  # Generate test data
  set.seed(123)
  n_samples <- 30
  n_genes <- 10
  expr_data <- matrix(rnorm(n_samples * n_genes), nrow = n_samples, ncol = n_genes)
  colnames(expr_data) <- paste0("Gene", 1:n_genes)

  # Test correlation method
  result_cor <- build_regulatory_network(
    expr_data,
    method = "correlation",
    threshold = 0.3,
    max_parents = 3
  )

  expect_true(is.list(result_cor))
  expect_true("graph" %in% names(result_cor))
  expect_true("adjacency_matrix" %in% names(result_cor))
  expect_true("strength_matrix" %in% names(result_cor))
  expect_s4_class(result_cor$graph, "graphNEL")

  # Test random method (should always work)
  result_rand <- build_regulatory_network(
    expr_data,
    method = "random",
    threshold = 0.5,
    max_parents = 2
  )

  expect_true(is.list(result_rand))
  expect_s4_class(result_rand$graph, "graphNEL")
})


test_that("build_regulatory_network handles edge cases", {
  set.seed(123)

  # Test with minimum data
  small_data <- matrix(rnorm(20), nrow = 5, ncol = 4)
  colnames(small_data) <- paste0("G", 1:4)

  expect_no_error({
    result <- build_regulatory_network(
      small_data,
      method = "correlation",
      threshold = 0.3
    )
  })

  # Test with data frame input
  df_data <- as.data.frame(small_data)
  expect_no_error({
    result_df <- build_regulatory_network(
      df_data,
      method = "correlation",
      threshold = 0.3
    )
  })

  # Test error on insufficient samples
  expect_error(
    build_regulatory_network(
      matrix(rnorm(10), nrow = 2, ncol = 5),
      method = "correlation"
    ),
    "at least 3 samples"
  )

  # Test error on insufficient genes
  expect_error(
    build_regulatory_network(
      matrix(rnorm(10), nrow = 5, ncol = 1),
      method = "correlation"
    ),
    "at least 2 genes"
  )
})


test_that("build_regulatory_network parameter validation works", {
  set.seed(123)
  expr_data <- matrix(rnorm(200), nrow = 20, ncol = 10)
  colnames(expr_data) <- paste0("Gene", 1:10)

  # Test invalid method
  expect_error(
    build_regulatory_network(expr_data, method = "invalid_method"),
    "Unsupported method"
  )

  # Test invalid threshold
  expect_error(
    build_regulatory_network(expr_data, threshold = -0.5),
    "between 0 and 1"
  )

  expect_error(
    build_regulatory_network(expr_data, threshold = 1.5),
    "between 0 and 1"
  )

  # Test invalid max_parents
  expect_error(
    build_regulatory_network(expr_data, max_parents = -1),
    "positive integer"
  )

  # Test invalid ridge_lambda
  expect_error(
    build_regulatory_network(expr_data, ridge_lambda = -0.1),
    "non-negative"
  )
})


test_that("split_expression_by_gene works correctly", {
  set.seed(123)
  n_samples <- 50
  n_genes <- 8
  expr_data <- matrix(rnorm(n_samples * n_genes), nrow = n_samples, ncol = n_genes)
  colnames(expr_data) <- paste0("Gene", 1:n_genes)

  # Test basic splitting
  result <- split_expression_by_gene(
    expr_data,
    target_gene = "Gene1",
    quantiles = c(0, 0.33, 0.67, 1),
    group_labels = c("Low", "Medium", "High")
  )

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_true(all(c("Low", "Medium", "High") %in% names(result)))

  # Check that all samples are accounted for
  total_samples <- sum(sapply(result, nrow))
  expect_equal(total_samples, n_samples)

  # Test return_groups option
  result_with_groups <- split_expression_by_gene(
    expr_data,
    target_gene = "Gene1",
    return_groups = TRUE
  )

  expect_true("data" %in% names(result_with_groups))
  expect_true("groups" %in% names(result_with_groups))
  expect_s3_class(result_with_groups$groups, "factor")
})


test_that("split_expression_by_gene handles errors", {
  set.seed(123)
  expr_data <- matrix(rnorm(100), nrow = 20, ncol = 5)
  colnames(expr_data) <- paste0("Gene", 1:5)

  # Test missing gene
  expect_error(
    split_expression_by_gene(expr_data, target_gene = "NonExistent"),
    "not found"
  )

  # Test mismatched quantiles and labels
  expect_error(
    split_expression_by_gene(
      expr_data,
      target_gene = "Gene1",
      quantiles = c(0, 0.5, 1),
      group_labels = c("Low", "High", "Extra")
    ),
    "Length of quantiles"
  )

  # Test invalid input type
  expect_error(
    split_expression_by_gene("not a matrix", target_gene = "Gene1"),
    "must be a matrix or data frame"
  )
})


test_that("filter_dce_results works with mock data", {
  skip_if_not_installed("dce")

  # Create mock DCE results as a data frame
  mock_results <- data.frame(
    source = rep(paste0("Gene", 1:5), each = 4),
    target = rep(paste0("Gene", 1:4), times = 5),
    dce = rnorm(20, mean = 0, sd = 0.5),
    dce_pvalue = runif(20, 0, 0.2),
    stringsAsFactors = FALSE
  )

  # Convert to matrix-like structure that can be converted to data frame
  class(mock_results) <- c("dce", "data.frame")

  # Test basic filtering
  filtered <- filter_dce_results(
    mock_results,
    pvalue_threshold = 0.05,
    sort_by = "dce"
  )

  expect_true(is.data.frame(filtered))
  expect_true(all(filtered$dce_pvalue < 0.05))

  # Test filtering by minimum DCE
  filtered_dce <- filter_dce_results(
    mock_results,
    min_abs_dce = 0.3,
    pvalue_threshold = 1
  )

  expect_true(all(abs(filtered_dce$dce) >= 0.3))

  # Test filtering by source genes
  filtered_source <- filter_dce_results(
    mock_results,
    source_genes = c("Gene1", "Gene2"),
    pvalue_threshold = 1
  )

  expect_true(all(filtered_source$source %in% c("Gene1", "Gene2")))

  # Test filtering by target genes
  filtered_target <- filter_dce_results(
    mock_results,
    target_genes = c("Gene1", "Gene2"),
    pvalue_threshold = 1
  )

  expect_true(all(filtered_target$target %in% c("Gene1", "Gene2")))
})


test_that("run_dce_analysis integration test", {
  skip_if_not_installed("dce")
  skip_on_cran()

  set.seed(123)

  # Generate test data for two conditions
  n_samples <- 40
  n_genes <- 10

  # Wild-type data
  wt_data <- matrix(rnorm(n_samples * n_genes), nrow = n_samples, ncol = n_genes)
  colnames(wt_data) <- paste0("Gene", 1:n_genes)

  # Mutant data with some differential effects
  mt_data <- wt_data + matrix(rnorm(n_samples * n_genes, mean = 0.3, sd = 0.5),
                               nrow = n_samples, ncol = n_genes)
  colnames(mt_data) <- paste0("Gene", 1:n_genes)

  # Run DCE analysis
  expect_message({
    result <- run_dce_analysis(
      wt_data = wt_data,
      mt_data = mt_data,
      network_method = "correlation",
      threshold = 0.3,
      max_parents = 3,
      verbose = TRUE,
      filter_results = TRUE,
      pvalue_threshold = 1  # Accept all for testing
    )
  })

  expect_true(is.list(result))
  expect_true("dce_results" %in% names(result))
  expect_true("network" %in% names(result))
  expect_true("filtered_results" %in% names(result))
})


test_that("batch_dce_analysis works with multiple genes", {
  skip_if_not_installed("dce")
  skip_on_cran()

  set.seed(123)

  # Generate test data
  n_samples <- 60
  n_genes <- 8
  expr_data <- matrix(rnorm(n_samples * n_genes), nrow = n_samples, ncol = n_genes)
  colnames(expr_data) <- paste0("Gene", 1:n_genes)

  # Run batch analysis
  target_genes <- c("Gene1", "Gene2")

  expect_message({
    results <- batch_dce_analysis(
      expr_matrix = expr_data,
      target_genes = target_genes,
      network_method = "correlation",
      quantiles = c(0, 0.4, 1),  # Just Low and High groups
      compare_groups = c("Low", "High"),
      verbose = TRUE
    )
  })

  expect_true(is.list(results))
  expect_equal(length(results), length(target_genes))
  expect_true(all(target_genes %in% names(results)))

  # Check structure of each result
  for(gene in target_genes) {
    expect_true("dce_results" %in% names(results[[gene]]))
    expect_true("target_gene" %in% names(results[[gene]]))
    expect_equal(results[[gene]]$target_gene, gene)
  }
})


test_that("build_regulatory_network handles constant genes", {
  set.seed(123)

  # Create data with one constant gene
  expr_data <- matrix(rnorm(100), nrow = 20, ncol = 5)
  expr_data[, 3] <- 5  # Make third gene constant
  colnames(expr_data) <- paste0("Gene", 1:5)

  # Should warn and remove constant gene
  expect_warning({
    result <- build_regulatory_network(
      expr_data,
      method = "correlation",
      threshold = 0.3
    )
  }, "zero or NA variance")

  # Network should be built with remaining genes
  expect_s4_class(result$graph, "graphNEL")
  expect_equal(ncol(result$filtered_expr), 4)  # One gene removed
})


test_that("build_regulatory_network max_genes parameter works", {
  set.seed(123)

  # Create data with variable genes
  n_samples <- 30
  n_genes <- 15
  expr_data <- matrix(rnorm(n_samples * n_genes), nrow = n_samples, ncol = n_genes)

  # Add different variances to genes
  for(i in 1:n_genes) {
    expr_data[, i] <- expr_data[, i] * i
  }
  colnames(expr_data) <- paste0("Gene", 1:n_genes)

  # Select top 5 genes by variance
  result <- build_regulatory_network(
    expr_data,
    method = "correlation",
    threshold = 0.3,
    max_genes = 5
  )

  expect_equal(ncol(result$filtered_expr), 5)

  # Verify high-variance genes are selected
  gene_names <- colnames(result$filtered_expr)
  # Higher numbered genes should be selected (higher variance)
  expect_true(all(as.numeric(gsub("Gene", "", gene_names)) > 10))
})
