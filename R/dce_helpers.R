#' Split Expression Data by Target Gene Expression
#'
#' @description
#' Splits expression data into groups (Low, Medium, High) based on the expression
#' levels of a target gene using quantile-based cutoffs.
#'
#' @param expr_matrix A numeric matrix of expression data where rows are samples
#'   and columns are genes. Can also be a data frame.
#' @param target_gene Character string specifying the name of the target gene to use for grouping.
#' @param quantiles Numeric vector of length 3 specifying the quantile cutoffs.
#'   Default is c(0, 0.4, 0.85, 1) which creates unequal groups emphasizing extremes.
#' @param group_labels Character vector of group names. Default is c("Low", "Medium", "High").
#' @param return_groups Logical. If TRUE, returns the group assignments along with split data.
#'
#' @return If return_groups=FALSE (default), a named list of expression matrices split by groups.
#'   If return_groups=TRUE, a list containing 'data' (split matrices) and 'groups' (factor of assignments).
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' set.seed(123)
#' expr_data <- matrix(rnorm(1000), nrow=50, ncol=20)
#' colnames(expr_data) <- paste0("Gene", 1:20)
#'
#' # Split by Gene1 expression
#' split_data <- split_expression_by_gene(expr_data, "Gene1")
#'
#' # Check group sizes
#' lapply(split_data, nrow)
#'
#' # Custom quantiles for equal groups
#' split_equal <- split_expression_by_gene(
#'   expr_data,
#'   "Gene1",
#'   quantiles = c(0, 0.33, 0.67, 1)
#' )
#' }
#'
#' @export
split_expression_by_gene <- function(expr_matrix,
                                     target_gene,
                                     quantiles = c(0, 0.4, 0.85, 1),
                                     group_labels = c("Low", "Medium", "High"),
                                     return_groups = FALSE) {

  # Input validation
  if(!is.matrix(expr_matrix) && !is.data.frame(expr_matrix)) {
    stop("expr_matrix must be a matrix or data frame")
  }

  if(!target_gene %in% colnames(expr_matrix)) {
    stop("Target gene '", target_gene, "' not found in expression matrix columns")
  }

  if(length(quantiles) != length(group_labels) + 1) {
    stop("Length of quantiles should be length of group_labels + 1")
  }

  # Convert to matrix if data frame
  if(is.data.frame(expr_matrix)) {
    expr_matrix <- as.matrix(expr_matrix)
  }

  # Get target gene expression values
  target_expr <- expr_matrix[, target_gene]

  # Calculate quantile breaks
  breaks <- stats::quantile(target_expr, probs = quantiles)

  # Create groups
  groups <- cut(target_expr,
                breaks = breaks,
                include.lowest = TRUE,
                labels = group_labels)

  # Split data by groups
  expr_df <- as.data.frame(expr_matrix)
  split_data <- split(expr_df, groups)

  # Convert back to matrices
  split_data <- lapply(split_data, as.matrix)

  # Print summary
  message("Data split summary:")
  for(i in seq_along(split_data)) {
    message("  ", names(split_data)[i], ": ", nrow(split_data[[i]]), " samples")
  }

  if(return_groups) {
    return(list(data = split_data, groups = groups))
  } else {
    return(split_data)
  }
}


#' Filter and Sort DCE Analysis Results
#'
#' @description
#' Filters DCE analysis results by p-value threshold and sorts by differential causal effect size.
#'
#' @param dce_results A DCE results object from dce::dce() function.
#' @param pvalue_threshold Numeric threshold for filtering by DCE p-value. Default is 0.05.
#' @param sort_by Character string specifying how to sort results. Options are:
#'   \itemize{
#'     \item "dce": Sort by absolute DCE value (default)
#'     \item "pvalue": Sort by p-value
#'     \item "source": Sort by source gene name
#'     \item "target": Sort by target gene name
#'   }
#' @param decreasing Logical. If TRUE, sort in decreasing order. Default is TRUE.
#' @param min_abs_dce Numeric. Minimum absolute DCE value to include. Default is NULL (no filtering).
#' @param source_genes Character vector. If provided, only return edges from these source genes.
#' @param target_genes Character vector. If provided, only return edges to these target genes.
#' @param remove_na Logical. If TRUE, remove rows with NA values. Default is TRUE.
#'
#' @return A data frame of filtered and sorted DCE results.
#'
#' @examples
#' \dontrun{
#' # After running DCE analysis
#' dce_results <- dce::dce(network, wt_data, mt_data)
#'
#' # Filter significant results
#' sig_results <- filter_dce_results(dce_results, pvalue_threshold = 0.05)
#'
#' # Get edges from specific gene
#' isg15_edges <- filter_dce_results(
#'   dce_results,
#'   source_genes = "ISG15",
#'   pvalue_threshold = 0.1
#' )
#'
#' # Filter by effect size
#' strong_effects <- filter_dce_results(
#'   dce_results,
#'   min_abs_dce = 0.5,
#'   pvalue_threshold = 0.05
#' )
#' }
#'
#' @export
filter_dce_results <- function(dce_results,
                               pvalue_threshold = 0.05,
                               sort_by = "dce",
                               decreasing = TRUE,
                               min_abs_dce = NULL,
                               source_genes = NULL,
                               target_genes = NULL,
                               remove_na = TRUE) {

  # Convert to data frame
  results_df <- as.data.frame(dce_results)

  # Remove NA if requested
  if(remove_na) {
    results_df <- results_df[stats::complete.cases(results_df), ]
  }

  # Filter by p-value
  if(!is.null(pvalue_threshold)) {
    results_df <- results_df[results_df$dce_pvalue < pvalue_threshold, ]
  }

  # Filter by minimum absolute DCE
  if(!is.null(min_abs_dce)) {
    results_df <- results_df[abs(results_df$dce) >= min_abs_dce, ]
  }

  # Filter by source genes
  if(!is.null(source_genes)) {
    results_df <- results_df[results_df$source %in% source_genes, ]
  }

  # Filter by target genes
  if(!is.null(target_genes)) {
    results_df <- results_df[results_df$target %in% target_genes, ]
  }

  # Sort results
  if(sort_by == "dce") {
    results_df <- results_df[order(abs(results_df$dce), decreasing = decreasing), ]
  } else if(sort_by == "pvalue") {
    results_df <- results_df[order(results_df$dce_pvalue, decreasing = !decreasing), ]
  } else if(sort_by == "source") {
    results_df <- results_df[order(results_df$source, decreasing = decreasing), ]
  } else if(sort_by == "target") {
    results_df <- results_df[order(results_df$target, decreasing = decreasing), ]
  }

  # Print summary
  message("Filtered DCE results summary:")
  message("  Total edges: ", nrow(results_df))
  if(nrow(results_df) > 0) {
    message("  DCE range: [", round(min(results_df$dce), 3), ", ",
            round(max(results_df$dce), 3), "]")
    message("  Significant edges (p<", pvalue_threshold, "): ",
            sum(results_df$dce_pvalue < pvalue_threshold, na.rm = TRUE))
  }

  return(results_df)
}


#' Run Complete DCE Analysis Pipeline
#'
#' @description
#' A wrapper function that runs the complete DCE analysis pipeline including network
#' construction and differential causal effect estimation.
#'
#' @param wt_data A numeric matrix of wild-type (control) expression data where rows
#'   are samples and columns are genes.
#' @param mt_data A numeric matrix of mutant (treatment) expression data with the same
#'   structure as wt_data.
#' @param network_method Character string specifying the network construction method.
#'   Default is "aracne". See build_regulatory_network() for options.
#' @param threshold Numeric value for edge inclusion threshold. Default is 0.3.
#' @param max_parents Integer specifying maximum number of parent nodes. Default is 3.
#' @param use_same_network Logical. If TRUE, constructs network using combined data.
#'   If FALSE, constructs separate networks. Default is TRUE.
#' @param filter_results Logical. If TRUE, applies default filtering to results. Default is TRUE.
#' @param pvalue_threshold P-value threshold for filtering. Default is 0.05.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#' @param ... Additional arguments passed to build_regulatory_network().
#'
#' @return A list containing:
#' \itemize{
#'   \item dce_results: The DCE analysis results
#'   \item filtered_results: Filtered results (if filter_results=TRUE)
#'   \item network: The regulatory network used
#'   \item wt_network: Wild-type network (if use_same_network=FALSE)
#'   \item mt_network: Mutant network (if use_same_network=FALSE)
#' }
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' set.seed(123)
#' wt_data <- matrix(rnorm(500), nrow=50, ncol=10)
#' mt_data <- matrix(rnorm(500, mean=0.5), nrow=50, ncol=10)
#' colnames(wt_data) <- colnames(mt_data) <- paste0("Gene", 1:10)
#'
#' # Run DCE analysis
#' results <- run_dce_analysis(wt_data, mt_data, network_method = "correlation")
#'
#' # View significant results
#' print(results$filtered_results)
#' }
#'
#' @export
run_dce_analysis <- function(wt_data,
                             mt_data,
                             network_method = "aracne",
                             threshold = 0.3,
                             max_parents = 3,
                             use_same_network = TRUE,
                             filter_results = TRUE,
                             pvalue_threshold = 0.05,
                             verbose = TRUE,
                             ...) {

  # Check if dce package is available
  if(!requireNamespace("dce", quietly = TRUE)) {
    stop("Package 'dce' is required for this function. Please install it with:\n",
         "  if (!requireNamespace('BiocManager', quietly = TRUE))\n",
         "    install.packages('BiocManager')\n",
         "  BiocManager::install('dce')")
  }

  # Input validation
  if(!is.matrix(wt_data) && !is.data.frame(wt_data)) {
    stop("wt_data must be a matrix or data frame")
  }
  if(!is.matrix(mt_data) && !is.data.frame(mt_data)) {
    stop("mt_data must be a matrix or data frame")
  }

  if(ncol(wt_data) != ncol(mt_data)) {
    stop("wt_data and mt_data must have the same number of genes")
  }

  if(!identical(colnames(wt_data), colnames(mt_data))) {
    warning("Column names differ between wt_data and mt_data. Using wt_data names.")
    colnames(mt_data) <- colnames(wt_data)
  }

  # Convert to matrices
  if(is.data.frame(wt_data)) wt_data <- as.matrix(wt_data)
  if(is.data.frame(mt_data)) mt_data <- as.matrix(mt_data)

  # Build network
  if(verbose) message("Building regulatory network using method: ", network_method)

  if(use_same_network) {
    # Use combined data for network construction
    combined_data <- rbind(wt_data, mt_data)
    network_result <- build_regulatory_network(
      combined_data,
      method = network_method,
      threshold = threshold,
      max_parents = max_parents,
      ...
    )

    result_list <- list(network = network_result)

  } else {
    # Build separate networks
    if(verbose) message("Building separate networks for WT and MT data")

    wt_network <- build_regulatory_network(
      wt_data,
      method = network_method,
      threshold = threshold,
      max_parents = max_parents,
      ...
    )

    mt_network <- build_regulatory_network(
      mt_data,
      method = network_method,
      threshold = threshold,
      max_parents = max_parents,
      ...
    )

    # Use WT network for DCE analysis
    network_result <- wt_network
    result_list <- list(
      network = network_result,
      wt_network = wt_network,
      mt_network = mt_network
    )
  }

  # Run DCE analysis
  if(verbose) message("Running DCE analysis...")

  tryCatch({
    dce_results <- dce::dce(
      graph = network_result$graph,
      wt = wt_data,
      mt = mt_data
    )

    result_list$dce_results <- dce_results

    # Filter results if requested
    if(filter_results) {
      if(verbose) message("Filtering results with p-value threshold: ", pvalue_threshold)

      filtered <- filter_dce_results(
        dce_results,
        pvalue_threshold = pvalue_threshold,
        sort_by = "dce",
        decreasing = TRUE
      )

      result_list$filtered_results <- filtered
    }

    if(verbose) message("DCE analysis complete!")

    return(result_list)

  }, error = function(e) {
    stop("Error in DCE analysis: ", e$message)
  })
}


#' Batch DCE Analysis for Multiple Target Genes
#'
#' @description
#' Performs DCE analysis for multiple target genes by splitting expression data based on
#' each target gene's expression levels and comparing high vs low expression groups.
#'
#' @param expr_matrix A numeric matrix of expression data where rows are samples and
#'   columns are genes.
#' @param target_genes Character vector of target gene names to analyze.
#' @param network_method Character string specifying the network construction method.
#'   Default is "aracne".
#' @param quantiles Numeric vector of quantile cutoffs for grouping. Default creates
#'   Low/Medium/High groups with emphasis on extremes.
#' @param compare_groups Character vector of length 2 specifying which groups to compare.
#'   Default is c("Low", "High").
#' @param filter_results Logical. If TRUE, applies default filtering. Default is TRUE.
#' @param pvalue_threshold P-value threshold for filtering. Default is 0.05.
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#' @param parallel Logical. If TRUE, runs analyses in parallel (requires future package).
#'   Default is FALSE.
#' @param ... Additional arguments passed to run_dce_analysis().
#'
#' @return A named list where each element corresponds to one target gene and contains
#'   the DCE analysis results.
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' set.seed(123)
#' expr_data <- matrix(rnorm(2000), nrow=100, ncol=20)
#' colnames(expr_data) <- paste0("Gene", 1:20)
#'
#' # Analyze multiple target genes
#' target_genes <- c("Gene1", "Gene5", "Gene10")
#' batch_results <- batch_dce_analysis(
#'   expr_data,
#'   target_genes,
#'   network_method = "correlation"
#' )
#'
#' # View results for each gene
#' lapply(batch_results, function(x) head(x$filtered_results))
#' }
#'
#' @export
batch_dce_analysis <- function(expr_matrix,
                               target_genes,
                               network_method = "aracne",
                               quantiles = c(0, 0.4, 0.85, 1),
                               compare_groups = c("Low", "High"),
                               filter_results = TRUE,
                               pvalue_threshold = 0.05,
                               verbose = TRUE,
                               parallel = FALSE,
                               ...) {

  # Input validation
  if(!is.matrix(expr_matrix) && !is.data.frame(expr_matrix)) {
    stop("expr_matrix must be a matrix or data frame")
  }

  missing_genes <- setdiff(target_genes, colnames(expr_matrix))
  if(length(missing_genes) > 0) {
    stop("Target genes not found in expression matrix: ",
         paste(missing_genes, collapse = ", "))
  }

  if(length(compare_groups) != 2) {
    stop("compare_groups must specify exactly 2 groups to compare")
  }

  # Convert to matrix if needed
  if(is.data.frame(expr_matrix)) {
    expr_matrix <- as.matrix(expr_matrix)
  }

  # Function to analyze one target gene
  analyze_one_gene <- function(target_gene) {
    if(verbose) {
      message("\n========================================")
      message("Analyzing target gene: ", target_gene)
      message("========================================")
    }

    tryCatch({
      # Split data by target gene expression
      split_data <- split_expression_by_gene(
        expr_matrix = expr_matrix,
        target_gene = target_gene,
        quantiles = quantiles,
        return_groups = FALSE
      )

      # Check if both comparison groups exist
      if(!all(compare_groups %in% names(split_data))) {
        stop("Compare groups not found in split data. Available groups: ",
             paste(names(split_data), collapse = ", "))
      }

      # Get the two groups to compare
      group1_data <- split_data[[compare_groups[1]]]
      group2_data <- split_data[[compare_groups[2]]]

      # Run DCE analysis
      result <- run_dce_analysis(
        wt_data = group1_data,
        mt_data = group2_data,
        network_method = network_method,
        filter_results = filter_results,
        pvalue_threshold = pvalue_threshold,
        verbose = verbose,
        ...
      )

      # Add metadata
      result$target_gene <- target_gene
      result$comparison <- paste(compare_groups, collapse = " vs ")
      result$group_sizes <- c(
        setNames(nrow(group1_data), compare_groups[1]),
        setNames(nrow(group2_data), compare_groups[2])
      )

      return(result)

    }, error = function(e) {
      warning("Error analyzing ", target_gene, ": ", e$message)
      return(NULL)
    })
  }

  # Run analysis for all target genes
  if(parallel) {
    if(!requireNamespace("future.apply", quietly = TRUE)) {
      warning("Package 'future.apply' not available. Running sequentially instead.")
      results <- lapply(target_genes, analyze_one_gene)
    } else {
      if(verbose) message("Running analyses in parallel...")
      results <- future.apply::future_lapply(target_genes, analyze_one_gene)
    }
  } else {
    results <- lapply(target_genes, analyze_one_gene)
  }

  # Name results by target gene
  names(results) <- target_genes

  # Remove NULL results (failed analyses)
  results <- results[!sapply(results, is.null)]

  if(verbose) {
    message("\n========================================")
    message("Batch analysis complete!")
    message("Successfully analyzed: ", length(results), "/", length(target_genes), " genes")
    message("========================================")
  }

  return(results)
}
