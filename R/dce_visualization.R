#' Enhanced DCE Network Visualization
#'
#' @description
#' Creates an enhanced visualization of DCE results with customizable highlighting
#' of significant edges and target nodes.
#'
#' @param dce_results DCE results object from dce::dce() function.
#' @param pvalue_threshold Numeric threshold for highlighting significant edges. Default is 0.05.
#' @param highlight_nodes Character vector of node names to highlight. Default is NULL.
#' @param min_abs_dce Minimum absolute DCE value to display edges. Default is NULL.
#' @param node_size Numeric value for node size. Default is 5.
#' @param arrow_size Numeric value for arrow size. Default is 0.02.
#' @param edge_width_scale Scale factor for edge widths based on DCE magnitude. Default is 2.
#' @param use_shadowtext Logical. If TRUE, uses shadow text for better readability. Default is TRUE.
#' @param color_palette Character vector of length 2 specifying colors for negative and
#'   positive DCE values. Default is c("blue", "red").
#' @param layout Character string specifying the layout algorithm. Default is "auto".
#' @param show_labels Logical. If TRUE, shows node labels. Default is TRUE.
#' @param label_size Numeric value for label size. Default is 3.
#' @param title Character string for plot title. Default is "DCE Network".
#' @param ... Additional arguments passed to dce::plot_network().
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # After running DCE analysis
#' dce_results <- dce::dce(network, wt_data, mt_data)
#'
#' # Basic plot
#' plot_dce_network(dce_results)
#'
#' # Highlight specific nodes and filter edges
#' plot_dce_network(
#'   dce_results,
#'   highlight_nodes = c("ISG15", "STAT1"),
#'   min_abs_dce = 0.3,
#'   pvalue_threshold = 0.01
#' )
#' }
#'
#' @export
plot_dce_network <- function(dce_results,
                             pvalue_threshold = 0.05,
                             highlight_nodes = NULL,
                             min_abs_dce = NULL,
                             node_size = 5,
                             arrow_size = 0.02,
                             edge_width_scale = 2,
                             use_shadowtext = TRUE,
                             color_palette = c("blue", "red"),
                             layout = "auto",
                             show_labels = TRUE,
                             label_size = 3,
                             title = "DCE Network",
                             ...) {

  # Check if dce package is available
  if(!requireNamespace("dce", quietly = TRUE)) {
    stop("Package 'dce' is required. Please install it with: BiocManager::install('dce')")
  }

  # Get graph from results
  graph <- dce_results$graph

  # Convert to matrix for visualization
  graph_matrix <- methods::as(graph, "matrix")

  # Filter results for significant edges
  results_df <- filter_dce_results(
    dce_results,
    pvalue_threshold = pvalue_threshold,
    min_abs_dce = min_abs_dce,
    remove_na = TRUE
  )

  # Create value matrix for edge coloring
  if(nrow(results_df) > 0) {
    value_matrix <- matrix(0, nrow = nrow(graph_matrix), ncol = ncol(graph_matrix))
    rownames(value_matrix) <- rownames(graph_matrix)
    colnames(value_matrix) <- colnames(graph_matrix)

    for(i in 1:nrow(results_df)) {
      src <- results_df$source[i]
      tgt <- results_df$target[i]
      if(src %in% rownames(value_matrix) && tgt %in% colnames(value_matrix)) {
        value_matrix[src, tgt] <- results_df$dce[i]
      }
    }
  } else {
    value_matrix <- NULL
    warning("No edges pass the significance threshold. Showing network structure only.")
  }

  # Create plot using dce::plot_network
  tryCatch({
    p <- dce::plot_network(
      graph_matrix,
      highlighted_nodes = highlight_nodes,
      visualize_edge_weights = !is.null(value_matrix),
      arrow_size = arrow_size,
      shadowtext = use_shadowtext,
      value_matrix = value_matrix,
      ...
    )

    # Add title if using ggplot2
    if(inherits(p, "gg")) {
      p <- p + ggplot2::labs(title = title)
    }

    return(p)

  }, error = function(e) {
    stop("Error creating network plot: ", e$message)
  })
}


#' Plot DCE Results as Heatmap
#'
#' @description
#' Creates a heatmap visualization of differential causal effects showing the
#' magnitude and direction of changes for all edges.
#'
#' @param dce_results DCE results object from dce::dce() function.
#' @param pvalue_threshold Numeric threshold for filtering edges. Default is 0.05.
#' @param min_abs_dce Minimum absolute DCE value to include. Default is NULL.
#' @param cluster_rows Logical. If TRUE, clusters rows. Default is TRUE.
#' @param cluster_cols Logical. If TRUE, clusters columns. Default is TRUE.
#' @param show_values Logical. If TRUE, displays DCE values in cells. Default is FALSE.
#' @param color_scale Character string specifying color scale. Options are "RdBu" (default),
#'   "viridis", "magma", or any ggplot2 scale name.
#' @param title Character string for plot title. Default is "Differential Causal Effects Heatmap".
#'
#' @return A ggplot2 heatmap object.
#'
#' @examples
#' \dontrun{
#' dce_results <- dce::dce(network, wt_data, mt_data)
#' plot_dce_heatmap(dce_results, pvalue_threshold = 0.01)
#' }
#'
#' @export
plot_dce_heatmap <- function(dce_results,
                             pvalue_threshold = 0.05,
                             min_abs_dce = NULL,
                             cluster_rows = TRUE,
                             cluster_cols = TRUE,
                             show_values = FALSE,
                             color_scale = "RdBu",
                             title = "Differential Causal Effects Heatmap") {

  # Check required packages
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Filter results
  results_df <- filter_dce_results(
    dce_results,
    pvalue_threshold = pvalue_threshold,
    min_abs_dce = min_abs_dce,
    remove_na = TRUE
  )

  if(nrow(results_df) == 0) {
    stop("No edges pass the filtering criteria")
  }

  # Create matrix for heatmap
  all_genes <- unique(c(results_df$source, results_df$target))
  dce_matrix <- matrix(0, nrow = length(all_genes), ncol = length(all_genes))
  rownames(dce_matrix) <- colnames(dce_matrix) <- all_genes

  for(i in 1:nrow(results_df)) {
    src <- results_df$source[i]
    tgt <- results_df$target[i]
    dce_matrix[src, tgt] <- results_df$dce[i]
  }

  # Remove rows/columns with all zeros
  row_sums <- rowSums(abs(dce_matrix))
  col_sums <- colSums(abs(dce_matrix))
  keep_rows <- row_sums > 0
  keep_cols <- col_sums > 0
  dce_matrix <- dce_matrix[keep_rows, keep_cols, drop = FALSE]

  if(nrow(dce_matrix) == 0 || ncol(dce_matrix) == 0) {
    stop("No non-zero DCE values after filtering")
  }

  # Clustering if requested
  if(cluster_rows && nrow(dce_matrix) > 1) {
    row_order <- stats::hclust(stats::dist(dce_matrix))$order
    dce_matrix <- dce_matrix[row_order, , drop = FALSE]
  }

  if(cluster_cols && ncol(dce_matrix) > 1) {
    col_order <- stats::hclust(stats::dist(t(dce_matrix)))$order
    dce_matrix <- dce_matrix[, col_order, drop = FALSE]
  }

  # Convert to long format for ggplot2
  dce_long <- reshape2::melt(dce_matrix)
  colnames(dce_long) <- c("Source", "Target", "DCE")

  # Create heatmap
  p <- ggplot2::ggplot(dce_long, ggplot2::aes(x = Target, y = Source, fill = DCE)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::scale_x_discrete(position = "bottom") +
    ggplot2::scale_y_discrete(limits = rev(levels(dce_long$Source))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    ggplot2::labs(
      title = title,
      x = "Target Gene",
      y = "Source Gene",
      fill = "DCE"
    )

  # Add color scale
  if(color_scale == "RdBu") {
    max_abs <- max(abs(dce_long$DCE))
    p <- p + ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-max_abs, max_abs)
    )
  } else if(color_scale == "viridis") {
    p <- p + ggplot2::scale_fill_viridis_c()
  } else if(color_scale == "magma") {
    p <- p + ggplot2::scale_fill_viridis_c(option = "magma")
  }

  # Add values if requested
  if(show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(DCE, 2)),
      size = 3,
      color = "black"
    )
  }

  return(p)
}


#' Plot DCE Volcano Plot
#'
#' @description
#' Creates a volcano plot showing the relationship between DCE magnitude and
#' statistical significance.
#'
#' @param dce_results DCE results object from dce::dce() function.
#' @param pvalue_threshold Numeric threshold for highlighting significant edges.
#'   Default is 0.05.
#' @param dce_threshold Numeric threshold for highlighting edges with large effects.
#'   Default is 0.3.
#' @param label_top Integer. Number of top edges to label. Default is 10.
#' @param point_size Numeric value for point size. Default is 2.
#' @param point_alpha Numeric value for point transparency. Default is 0.6.
#' @param highlight_color Character vector of colors for significant points. Default
#'   is c("grey", "red").
#' @param title Character string for plot title. Default is "DCE Volcano Plot".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' dce_results <- dce::dce(network, wt_data, mt_data)
#' plot_dce_volcano(dce_results, pvalue_threshold = 0.01)
#' }
#'
#' @export
plot_dce_volcano <- function(dce_results,
                             pvalue_threshold = 0.05,
                             dce_threshold = 0.3,
                             label_top = 10,
                             point_size = 2,
                             point_alpha = 0.6,
                             highlight_color = c("grey", "red"),
                             title = "DCE Volcano Plot") {

  # Check required packages
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  # Convert results to data frame
  results_df <- as.data.frame(dce_results)
  results_df <- results_df[stats::complete.cases(results_df), ]

  if(nrow(results_df) == 0) {
    stop("No complete DCE results available")
  }

  # Add -log10(p-value) column
  results_df$neg_log10_p <- -log10(results_df$dce_pvalue)

  # Replace infinite values
  results_df$neg_log10_p[is.infinite(results_df$neg_log10_p)] <- max(
    results_df$neg_log10_p[!is.infinite(results_df$neg_log10_p)]
  ) + 1

  # Determine significance
  results_df$significant <- (results_df$dce_pvalue < pvalue_threshold) &
                            (abs(results_df$dce) > dce_threshold)

  # Add edge labels
  results_df$edge_label <- paste(results_df$source, "->", results_df$target)

  # Select top edges to label
  if(label_top > 0) {
    results_df <- results_df[order(results_df$dce_pvalue), ]
    results_df$show_label <- FALSE
    results_df$show_label[1:min(label_top, nrow(results_df))] <- TRUE
  } else {
    results_df$show_label <- FALSE
  }

  # Create volcano plot
  p <- ggplot2::ggplot(results_df, ggplot2::aes(x = dce, y = neg_log10_p)) +
    ggplot2::geom_point(
      ggplot2::aes(color = significant),
      size = point_size,
      alpha = point_alpha
    ) +
    ggplot2::scale_color_manual(
      values = highlight_color,
      labels = c("Not significant", "Significant")
    ) +
    ggplot2::geom_hline(
      yintercept = -log10(pvalue_threshold),
      linetype = "dashed",
      color = "blue",
      alpha = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = c(-dce_threshold, dce_threshold),
      linetype = "dashed",
      color = "blue",
      alpha = 0.7
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = "Differential Causal Effect (DCE)",
      y = "-log10(p-value)",
      color = "Status"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )

  # Add labels for top edges
  if(any(results_df$show_label)) {
    if(requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        data = results_df[results_df$show_label, ],
        ggplot2::aes(label = edge_label),
        size = 3,
        max.overlaps = 20
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = results_df[results_df$show_label, ],
        ggplot2::aes(label = edge_label),
        size = 3,
        vjust = -0.5
      )
    }
  }

  return(p)
}


#' Compare Two Regulatory Networks
#'
#' @description
#' Creates side-by-side visualizations comparing two regulatory networks
#' (e.g., wild-type vs mutant, or low vs high expression groups).
#'
#' @param network1 First network result from build_regulatory_network().
#' @param network2 Second network result from build_regulatory_network().
#' @param network1_name Character string for first network name. Default is "Network 1".
#' @param network2_name Character string for second network name. Default is "Network 2".
#' @param highlight_nodes Character vector of nodes to highlight. Default is NULL.
#' @param show_diff Logical. If TRUE, also shows a difference network. Default is TRUE.
#' @param layout Character string specifying layout algorithm. Default is "auto".
#' @param ... Additional arguments passed to visualization functions.
#'
#' @return A combined plot object showing both networks and optionally their difference.
#'
#' @examples
#' \dontrun{
#' # Build networks for two conditions
#' net1 <- build_regulatory_network(expr_low, method = "aracne")
#' net2 <- build_regulatory_network(expr_high, method = "aracne")
#'
#' # Compare them
#' compare_networks(net1, net2, "Low Expression", "High Expression")
#' }
#'
#' @export
compare_networks <- function(network1,
                            network2,
                            network1_name = "Network 1",
                            network2_name = "Network 2",
                            highlight_nodes = NULL,
                            show_diff = TRUE,
                            layout = "auto",
                            ...) {

  # Check if dce package is available
  if(!requireNamespace("dce", quietly = TRUE)) {
    stop("Package 'dce' is required. Please install it with: BiocManager::install('dce')")
  }

  # Check if patchwork is available for combining plots
  use_patchwork <- requireNamespace("patchwork", quietly = TRUE)

  if(!use_patchwork && show_diff) {
    warning("Package 'patchwork' not available. Showing networks separately.")
  }

  # Extract adjacency matrices
  adj1 <- network1$adjacency_matrix
  adj2 <- network2$adjacency_matrix

  # Ensure matrices have same dimensions
  all_genes <- union(rownames(adj1), rownames(adj2))
  n <- length(all_genes)

  # Create expanded matrices
  adj1_expanded <- matrix(0, n, n)
  adj2_expanded <- matrix(0, n, n)
  rownames(adj1_expanded) <- colnames(adj1_expanded) <- all_genes
  rownames(adj2_expanded) <- colnames(adj2_expanded) <- all_genes

  # Fill in values
  genes1 <- intersect(all_genes, rownames(adj1))
  genes2 <- intersect(all_genes, rownames(adj2))
  adj1_expanded[genes1, genes1] <- adj1[genes1, genes1]
  adj2_expanded[genes2, genes2] <- adj2[genes2, genes2]

  # Calculate difference
  diff_matrix <- adj2_expanded - adj1_expanded

  # Create visualizations
  message("Creating network comparison plots...")

  # Plot network 1
  tryCatch({
    p1 <- dce::plot_network(
      adj1_expanded,
      highlighted_nodes = highlight_nodes,
      visualize_edge_weights = TRUE,
      value_matrix = adj1_expanded,
      ...
    )
    if(inherits(p1, "gg")) {
      p1 <- p1 + ggplot2::labs(title = network1_name)
    }
  }, error = function(e) {
    warning("Error creating plot for network 1: ", e$message)
    p1 <- NULL
  })

  # Plot network 2
  tryCatch({
    p2 <- dce::plot_network(
      adj2_expanded,
      highlighted_nodes = highlight_nodes,
      visualize_edge_weights = TRUE,
      value_matrix = adj2_expanded,
      ...
    )
    if(inherits(p2, "gg")) {
      p2 <- p2 + ggplot2::labs(title = network2_name)
    }
  }, error = function(e) {
    warning("Error creating plot for network 2: ", e$message)
    p2 <- NULL
  })

  # Plot difference if requested
  if(show_diff) {
    tryCatch({
      p3 <- dce::plot_network(
        adj2_expanded,
        highlighted_nodes = highlight_nodes,
        visualize_edge_weights = TRUE,
        value_matrix = diff_matrix,
        ...
      )
      if(inherits(p3, "gg")) {
        p3 <- p3 + ggplot2::labs(title = paste(network2_name, "-", network1_name))
      }
    }, error = function(e) {
      warning("Error creating difference plot: ", e$message)
      p3 <- NULL
    })
  } else {
    p3 <- NULL
  }

  # Combine plots if patchwork is available
  if(use_patchwork && !is.null(p1) && !is.null(p2)) {
    if(!is.null(p3)) {
      combined <- p1 + p2 + p3 + patchwork::plot_layout(ncol = 3)
    } else {
      combined <- p1 + p2 + patchwork::plot_layout(ncol = 2)
    }
    return(combined)
  } else {
    # Return list of plots
    return(list(
      network1 = p1,
      network2 = p2,
      difference = p3
    ))
  }
}
