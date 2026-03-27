#' Build Gene Regulatory Network from Expression Data
#'
#' @description
#' Constructs a gene regulatory network from expression data using various methods including
#' correlation, partial correlation, mutual information, and ARACNE. The function handles
#' collinearity issues and provides options for network size control.
#'
#' @param expr_matrix A numeric matrix of expression data where rows are samples and columns are genes.
#' @param method Character string specifying the network construction method. Options are:
#'   \itemize{
#'     \item "correlation": Pearson correlation
#'     \item "pcor": Partial correlation (with ridge regression option for collinearity)
#'     \item "mi": Mutual information
#'     \item "aracne": ARACNE algorithm
#'     \item "random": Random network (for testing)
#'   }
#' @param threshold Numeric value between 0 and 1 specifying the threshold for edge inclusion.
#' @param max_parents Integer specifying the maximum number of parent nodes allowed for each gene.
#' @param max_genes Integer or NULL specifying the maximum number of genes to include (selected by variance).
#' @param eff_min Numeric value specifying the minimum edge weight.
#' @param eff_max Numeric value specifying the maximum edge weight.
#' @param handle_colinearity Logical indicating whether to use ridge regression for handling collinearity in pcor method.
#' @param ridge_lambda Numeric value specifying the lambda parameter for ridge regression (default: 0.1).
#'
#' @return A list containing:
#' \itemize{
#'   \item graph: A graphNEL object representing the network
#'   \item filtered_expr: The filtered expression matrix used for network construction
#'   \item adjacency_matrix: The weighted adjacency matrix of the network
#'   \item strength_matrix: The matrix of association strengths between genes
#'   \item method_used: The method used for network construction
#' }
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' set.seed(123)
#' expr_matrix <- matrix(rnorm(1000), nrow=50)
#' colnames(expr_matrix) <- paste0("gene", 1:20)
#'
#' # Build network using correlation
#' net_cor <- build_regulatory_network(
#'   expr_matrix,
#'   method = "correlation",
#'   threshold = 0.3
#' )
#'
#' # Build network using partial correlation with collinearity handling
#' net_pcor <- build_regulatory_network(
#'   expr_matrix,
#'   method = "pcor",
#'   threshold = 0.3,
#'   handle_colinearity = TRUE
#' )
#' }
#'
#' @import ridge
#' @import naturalsort
#' @import graph
#' @import infotheo
#' @import minet
#' @importFrom stats cor predict runif var
#'
#' @export
#'
build_regulatory_network <- function(expr_matrix,
                                     method = "correlation",
                                     threshold = 0.3,
                                     max_parents = 3,
                                     max_genes = NULL,
                                     eff_min = -1,
                                     eff_max = 1,
                                     handle_colinearity = TRUE,
                                     ridge_lambda = 0.1) {
  
  # Input validation
  if(!is.matrix(expr_matrix) && !is.data.frame(expr_matrix)) {
    stop("Expression data must be a matrix or data frame")
  }

  # Convert to matrix if data frame
  if(is.data.frame(expr_matrix)) {
    expr_matrix <- as.matrix(expr_matrix)
  }

  # Check dimensions
  if(nrow(expr_matrix) < 3) {
    stop("Expression matrix must have at least 3 samples (rows)")
  }

  if(ncol(expr_matrix) < 2) {
    stop("Expression matrix must have at least 2 genes (columns)")
  }

  # Check for NA values
  if(any(is.na(expr_matrix))) {
    warning("Expression matrix contains NA values. These may cause errors in network construction.")
  }

  # Check for constant genes
  gene_vars <- apply(expr_matrix, 2, stats::var, na.rm = TRUE)
  constant_genes <- which(gene_vars == 0 | is.na(gene_vars))
  if(length(constant_genes) > 0) {
    warning("Removing ", length(constant_genes),
            " genes with zero or NA variance: ",
            paste(head(colnames(expr_matrix)[constant_genes], 3), collapse = ", "),
            if(length(constant_genes) > 3) " ..." else "")
    expr_matrix <- expr_matrix[, -constant_genes, drop = FALSE]
  }

  if(!method %in% c("correlation", "pcor", "mi", "aracne", "random")) {
    stop("Unsupported method. Choose from: correlation, pcor, mi, aracne, random")
  }

  if(!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("Threshold must be a number between 0 and 1")
  }

  if(!is.numeric(max_parents) || max_parents < 1) {
    stop("max_parents must be a positive integer")
  }

  if(!is.numeric(ridge_lambda) || ridge_lambda < 0) {
    stop("ridge_lambda must be a non-negative number")
  }
  
  # Gene selection and filtering
  if(is.null(max_genes) || max_genes >= ncol(expr_matrix)) {
    expr_filtered <- expr_matrix
    gene_names <- colnames(expr_filtered)
  } else {
    var_values <- apply(expr_matrix, 2, var)
    top_genes <- order(var_values, decreasing = TRUE)[1:max_genes]
    expr_filtered <- expr_matrix[, top_genes, drop=FALSE]
    gene_names <- colnames(expr_filtered)
  }
  
  node_num <- length(gene_names)
  
  # Check if gene names exist
  if(is.null(gene_names)) {
    warning("Expression matrix has no column names. Using generic gene names.")
    gene_names <- paste0("gene", 1:ncol(expr_filtered))
    colnames(expr_filtered) <- gene_names
  }
  
  # Sort genes using natural order
  sorted_genes <- gene_names[naturalsort::naturalorder(gene_names)]
  expr_filtered <- expr_filtered[, sorted_genes, drop=FALSE]
  
  # Initialize adjacency matrix
  adj_mat <- matrix(0, node_num, node_num)
  colnames(adj_mat) <- rownames(adj_mat) <- sorted_genes
  
  # Calculate strength matrix based on selected method
  if(method == "correlation") {
    # Correlation method
    cor_mat <- stats::cor(expr_filtered)
    diag(cor_mat) <- 0
    strength_mat <- cor_mat
    
  } else if(method == "pcor") {
    if(handle_colinearity) {
      # Ridge regression approach for collinearity
      if(!requireNamespace("ridge", quietly = TRUE)) {
        stop("Package 'ridge' is required for collinearity handling. Please install it with: install.packages('ridge')")
      }
      
      strength_mat <- matrix(0, node_num, node_num)
      colnames(strength_mat) <- rownames(strength_mat) <- sorted_genes
      
      for(i in 1:node_num) {
        for(j in 1:node_num) {
          if(i != j) {
            # Use other genes as covariates for each gene pair
            other_genes <- setdiff(1:node_num, c(i,j))
            if(length(other_genes) > 0) {
              # Prepare data frame for regression
              X <- expr_filtered[, other_genes, drop=FALSE]
              X_df <- as.data.frame(X)
              
              # Ridge regression for gene i
              y_i <- expr_filtered[, i]
              data_i <- cbind(X_df, target=y_i)
              formula_i <- stats::as.formula("target ~ .")
              fit_i <- ridge::linearRidge(formula_i, data = data_i, lambda = ridge_lambda)
              res_i <- y_i - stats::predict(fit_i, X_df)
              
              # Ridge regression for gene j
              y_j <- expr_filtered[, j]
              data_j <- cbind(X_df, target=y_j)
              formula_j <- stats::as.formula("target ~ .")
              fit_j <- ridge::linearRidge(formula_j, data = data_j, lambda = ridge_lambda)
              res_j <- y_j - stats::predict(fit_j, X_df)
              
              # Ensure residuals are numeric vectors
              res_i <- as.numeric(res_i)
              res_j <- as.numeric(res_j)
              
              # Calculate correlation between residuals as partial correlation
              if(length(res_i) > 1 && length(res_j) > 1) {
                strength_mat[i,j] <- stats::cor(res_i, res_j)
              }
            }
          }
        }
      }
    } else {
      # Standard partial correlation approach
      if(!requireNamespace("ppcor", quietly = TRUE)) {
        stop("Package 'ppcor' is required for partial correlation. Please install it with: install.packages('ppcor')")
      }
      
      tryCatch({
        pcor_result <- ppcor::pcor(expr_filtered)
        strength_mat <- pcor_result$estimate
        strength_mat[is.na(strength_mat)] <- 0
      }, error = function(e) {
        warning("Error in partial correlation calculation: ", e$message, ". Switching to regular correlation.")
        strength_mat <- stats::cor(expr_filtered)
      })
    }
    diag(strength_mat) <- 0
    
  } else if(method == "mi") {
    # Mutual information method
    if(!requireNamespace("infotheo", quietly = TRUE)) {
      stop("Package 'infotheo' is required for mutual information calculation. Please install it with: install.packages('infotheo')")
    }
    
    disc_data <- infotheo::discretize(expr_filtered)
    mi_mat <- infotheo::mutinformation(disc_data)
    diag(mi_mat) <- 0
    strength_mat <- mi_mat
    
  } else if(method == "aracne") {
    # ARACNE algorithm
    if(!requireNamespace("minet", quietly = TRUE)) {
      stop("Package 'minet' is required for ARACNE algorithm. Please install it with: BiocManager::install('minet')")
    }
    if(!requireNamespace("infotheo", quietly = TRUE)) {
      stop("Package 'infotheo' is required for ARACNE algorithm. Please install it with: install.packages('infotheo')")
    }
    
    disc_data <- infotheo::discretize(expr_filtered)
    mi_mat <- minet::build.mim(disc_data, estimator="mi.empirical")
    aracne_mat <- minet::aracne(mi_mat)
    diag(aracne_mat) <- 0
    strength_mat <- aracne_mat
    
  } else if(method == "random") {
    # Random network for testing
    strength_mat <- matrix(stats::runif(node_num^2), node_num, node_num)
    diag(strength_mat) <- 0
  }
  
  # Apply threshold to determine edges
  for(i in 1:(node_num-1)) {
    for(j in (i+1):ncol(adj_mat)) {
      if(method == "random") {
        if(stats::runif(1) < threshold) {
          adj_mat[i,j] <- 1
        }
      } else {
        if(abs(strength_mat[i,j]) > threshold) {
          adj_mat[i,j] <- 1
        }
      }
    }
  }
  
  # Limit parent nodes
  for(j in 1:node_num) {
    parents <- which(adj_mat[,j] != 0)
    if(length(parents) > max_parents) {
      if(method == "random") {
        selected_parents <- sample(parents, max_parents)
      } else {
        parent_strengths <- abs(strength_mat[parents,j])
        selected_parents <- parents[order(parent_strengths, decreasing=TRUE)[1:max_parents]]
      }
      adj_mat[,j] <- 0
      adj_mat[selected_parents,j] <- 1
    }
  }
  
  # Apply edge weights
  weight_mat <- adj_mat
  nonzero_edges <- which(weight_mat != 0)

  if(length(nonzero_edges) == 0) {
    warning("No edges pass the threshold. Consider lowering the threshold or using a different method.")
  }

  if(method == "random") {
    weight_mat[nonzero_edges] <- stats::runif(length(nonzero_edges), min = eff_min, max = eff_max)
  } else if(length(nonzero_edges) > 0) {
    weights <- strength_mat[nonzero_edges]
    abs_weights <- abs(weights)

    # Handle case where all weights are equal or very close
    weight_range <- max(abs_weights) - min(abs_weights)
    if(weight_range > 1e-10) {
      scaled_weights <- (abs_weights - min(abs_weights)) / weight_range
    } else {
      # All weights are essentially equal
      scaled_weights <- rep(0.5, length(abs_weights))
      if(length(abs_weights) > 0) {
        message("All edge weights are nearly identical. Using uniform scaling.")
      }
    }

    # Scale to desired range while preserving sign
    weights <- sign(weights) * scaled_weights * (eff_max - eff_min) + eff_min

    # Handle any remaining NA or Inf values
    weights[is.na(weights) | is.infinite(weights)] <- 0

    weight_mat[nonzero_edges] <- weights
  }
  
  # Create graphNEL object
  if(!requireNamespace("graph", quietly = TRUE)) {
    stop("Package 'graph' is required. Please install it with: BiocManager::install('graph')")
  }
  
  g <- methods::new("graphNEL", nodes = sorted_genes, edgemode = "directed")
  edges <- which(weight_mat != 0, arr.ind = TRUE)
  
  if(nrow(edges) > 0) {
    for(i in 1:nrow(edges)) {
      from <- rownames(weight_mat)[edges[i,1]]
      to <- colnames(weight_mat)[edges[i,2]]
      g <- graph::addEdge(from, to, g, weight_mat[edges[i,1], edges[i,2]])
    }
  }
  
  # Return results
  return(list(
    graph = g,
    filtered_expr = expr_filtered,
    adjacency_matrix = weight_mat,
    strength_matrix = strength_mat,
    method_used = method
  ))
}