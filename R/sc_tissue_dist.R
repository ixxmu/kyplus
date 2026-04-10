#' Calculate Cell Type Distribution Across Tissue/Sample Types
#'
#' @description
#' Analyzes the preferential distribution of cell types (clusters) across
#' different tissue/sample types. Tests whether certain cell types are
#' enriched or depleted in specific tissue/sample categories.
#'
#' Inspired by \code{Startrac::calTissueDist}, rewritten using base R and
#' tidyverse with zero additional dependencies.
#'
#' @param metadata A data.frame containing per-cell metadata. Typically
#'   extracted from a Seurat object via \code{seu@@meta.data}.
#' @param col_cluster Character. Column name for cell type / cluster identity.
#'   Default is \code{"seurat_clusters"}.
#' @param col_tissue Character. Column name for the tissue or sample type
#'   grouping variable. Default is \code{"sample_type"}.
#' @param col_sample Character. Column name for sample ID. Required when
#'   \code{method = "freq"}. Default is \code{"orig.ident"}.
#' @param col_patient Character. Column name for patient ID. Used when
#'   \code{by_patient = TRUE}. Default is \code{"patient"}.
#' @param method Character. Statistical method to use. One of:
#'   \describe{
#'     \item{\code{"chisq"}}{Chi-squared test. Returns Observed/Expected (O/E)
#'       ratio matrix. O/E > 1 means enrichment, O/E < 1 means depletion.
#'       Fast and simple.}
#'     \item{\code{"fisher"}}{Fisher's exact test for each cell in the
#'       contingency table. Returns Odds Ratios, p-values, and adjusted
#'       p-values. More robust for small counts.}
#'     \item{\code{"freq"}}{Frequency-based approach treating each sample as a
#'       biological replicate. Performs t-test and Wilcoxon test comparing
#'       within-tissue vs. between-tissue frequencies. Most statistically
#'       rigorous but requires >= 3 samples per tissue.}
#'   }
#' @param by_patient Logical. If \code{TRUE}, performs the analysis separately
#'   for each patient. Only applicable for \code{"chisq"} and \code{"fisher"}
#'   methods. Default is \code{FALSE}.
#' @param min_row_sum Numeric. Minimum total cell count for a cluster to be
#'   included. Clusters with fewer cells are filtered out. Only used in
#'   \code{"fisher"} method. Default is \code{0}.
#'
#' @return Depends on method:
#'   \describe{
#'     \item{\code{"chisq"}, \code{by_patient = FALSE}}{A matrix of O/E ratios
#'       (cluster x tissue).}
#'     \item{\code{"chisq"}, \code{by_patient = TRUE}}{A 3D array of O/E ratios
#'       (patient x cluster x tissue).}
#'     \item{\code{"fisher"}, \code{by_patient = FALSE}}{A list containing:
#'       \code{detail} (full results data.frame), \code{p.tb}, \code{padj.tb},
#'       \code{OR.tb}, \code{charSig.tb} (wide-format tables).}
#'     \item{\code{"fisher"}, \code{by_patient = TRUE}}{A list of per-patient
#'       OR wide tables.}
#'     \item{\code{"freq"}}{A list containing: \code{detail} (full results),
#'       \code{logFC.tb}, \code{FDR.tb}, \code{charSig.tb}
#'       (wide-format tables).}
#'   }
#'
#' @details
#' ## Significance markers
#' For \code{"fisher"} and \code{"freq"} methods, significance is marked as:
#' \itemize{
#'   \item \code{""} — not significant
#'   \item \code{"+"} — FDR < 0.1 and p < 0.05 (marginal)
#'   \item \code{"*"} — FDR < 0.05
#'   \item \code{"**"} — FDR < 0.01
#' }
#'
#' ## Method recommendations
#' \itemize{
#'   \item Use \code{"chisq"} for a quick overview (e.g., heatmap of O/E ratios)
#'   \item Use \code{"fisher"} when you need p-values and have few categories
#'   \item Use \code{"freq"} when you have biological replicates (multiple
#'     samples per tissue type) and want the most rigorous analysis
#' }
#'
#' @examples
#' \dontrun{
#' # From Seurat object
#' meta <- seu@meta.data
#'
#' # Quick O/E ratio heatmap
#' oe <- sc_calc_tissue_dist(meta,
#'   col_cluster = "celltype",
#'   col_tissue  = "sample_type",
#'   method      = "chisq"
#' )
#' pheatmap::pheatmap(oe, display_numbers = TRUE)
#'
#' # Fisher's exact test with significance
#' res_fisher <- sc_calc_tissue_dist(meta,
#'   col_cluster = "celltype",
#'   col_tissue  = "sample_type",
#'   method      = "fisher"
#' )
#'
#' # Frequency-based analysis with biological replicates
#' res_freq <- sc_calc_tissue_dist(meta,
#'   col_cluster = "celltype",
#'   col_tissue  = "sample_type",
#'   col_sample  = "orig.ident",
#'   method      = "freq"
#' )
#' }
#'
#' @seealso \code{\link[stats]{chisq.test}}, \code{\link[stats]{fisher.test}}
#'
#' @importFrom dplyr group_by summarise mutate left_join n arrange filter
#' @importFrom tidyr complete
#' @importFrom reshape2 dcast
#' @importFrom stats chisq.test fisher.test t.test wilcox.test p.adjust
#' @export
sc_calc_tissue_dist <- function(metadata,
                             col_cluster = "seurat_clusters",
                             col_tissue = "sample_type",
                             col_sample = "orig.ident",
                             col_patient = "patient",
                             method = c("chisq", "fisher", "freq"),
                             by_patient = FALSE,
                             min_row_sum = 0) {

  method <- match.arg(method)

  # --- Validate columns ---
  .validate_cols <- function(df, cols, context = "") {
    missing <- setdiff(cols, colnames(df))
    if (length(missing) > 0) {
      stop("Column(s) not found in metadata: ",
           paste(missing, collapse = ", "),
           if (nchar(context) > 0) paste0(" (needed for ", context, ")"),
           call. = FALSE)
    }
  }

  required <- c(col_cluster, col_tissue)
  if (method == "freq") required <- c(required, col_sample)
  if (by_patient) required <- c(required, col_patient)
  .validate_cols(metadata, required)

  # --- Dispatch ---
  if (method == "freq") {
    .tissue_dist_freq(metadata, col_cluster, col_tissue, col_sample)
  } else if (by_patient) {
    .tissue_dist_by_patient(metadata, col_cluster, col_tissue, col_patient,
                            method)
  } else if (method == "chisq") {
    .tissue_dist_chisq(metadata, col_cluster, col_tissue)
  } else {
    .tissue_dist_fisher(metadata, col_cluster, col_tissue, min_row_sum)
  }
}


# ============================================================================
# Internal: Chi-squared method (O/E ratio)
# ============================================================================
#' @keywords internal
.tissue_dist_chisq <- function(metadata, col_cluster, col_tissue) {
  contingency <- table(metadata[[col_cluster]], metadata[[col_tissue]])
  res <- stats::chisq.test(contingency)
  oe <- res$observed / res$expected
  return(oe)
}


# ============================================================================
# Internal: Fisher's exact test method
# ============================================================================
#' @keywords internal
.tissue_dist_fisher <- function(metadata, col_cluster, col_tissue,
                                min_row_sum) {
  contingency <- table(metadata[[col_cluster]], metadata[[col_tissue]])

  # Filter low-count clusters
  if (min_row_sum > 0) {
    keep <- rowSums(contingency) > min_row_sum
    if (sum(keep) == 0) {
      stop("No clusters remain after filtering with min_row_sum = ",
           min_row_sum, call. = FALSE)
    }
    contingency <- contingency[keep, , drop = FALSE]
  }

  # Run pairwise 2x2 Fisher tests
  detail <- .pairwise_fisher(contingency)

  # Add significance markers
  detail <- .add_sig_markers(detail, p_col = "p.value", padj_col = "p.adj")

  # Wide-format tables
  p_tb      <- reshape2::dcast(detail, rid ~ cid, value.var = "p.value")
  padj_tb   <- reshape2::dcast(detail, rid ~ cid, value.var = "p.adj")
  or_tb     <- reshape2::dcast(detail, rid ~ cid, value.var = "OR")
  sig_tb    <- reshape2::dcast(detail, rid ~ cid, value.var = "sig")

  list(
    detail     = detail,
    p.tb       = p_tb,
    padj.tb    = padj_tb,
    OR.tb      = or_tb,
    charSig.tb = sig_tb
  )
}


# ============================================================================
# Internal: Frequency-based method (t-test + Wilcoxon)
# ============================================================================
#' @keywords internal
.tissue_dist_freq <- function(metadata, col_cluster, col_tissue, col_sample) {
  df <- data.frame(
    sample  = metadata[[col_sample]],
    tissue  = metadata[[col_tissue]],
    cluster = metadata[[col_cluster]],
    stringsAsFactors = FALSE
  )

  # Step 1: Count total cells per sample-tissue
  n_total <- dplyr::group_by(df, sample, tissue) %>%
    dplyr::summarise(N_tot = dplyr::n(), .groups = "drop")

  # Step 2: Count cells per sample-tissue-cluster
  n_clust <- dplyr::group_by(df, sample, tissue, cluster) %>%
    dplyr::summarise(N_cls = dplyr::n(), .groups = "drop")

  # Step 3: Complete all combinations (fill missing with 0)
  n_clust <- tidyr::complete(
    n_clust,
    tidyr::nesting(sample, tissue),
    cluster,
    fill = list(N_cls = 0)
  )

  # Step 4: Calculate frequency
  n_clust <- dplyr::left_join(n_clust, n_total, by = c("sample", "tissue"))
  n_clust <- dplyr::mutate(n_clust, freq = N_cls / N_tot)

  # Step 5: For each cluster x tissue, compare vs. other tissues
  all_clusters <- sort(unique(n_clust$cluster))
  all_tissues  <- sort(unique(n_clust$tissue))

  results <- do.call(rbind, lapply(all_clusters, function(cls) {
    sub <- n_clust[n_clust$cluster == cls, ]
    do.call(rbind, lapply(all_tissues, function(tis) {
      freq_in   <- sub$freq[sub$tissue == tis]
      freq_out  <- sub$freq[sub$tissue != tis]

      if (length(freq_in) < 3) return(NULL)

      tt <- tryCatch(stats::t.test(freq_in, freq_out),
                     error = function(e) NULL)
      wt <- tryCatch(stats::wilcox.test(freq_in, freq_out),
                      error = function(e) NULL)

      mean_in  <- mean(freq_in)
      mean_out <- mean(freq_out)
      lfc <- if (mean_out > 0) log2(mean_in / mean_out) else NA_real_

      data.frame(
        cluster   = cls,
        tissue    = tis,
        logFC     = lfc,
        mean_freq = mean_in,
        p.value.t = if (!is.null(tt)) tt$p.value else NA_real_,
        p.value.w = if (!is.null(wt)) wt$p.value else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  }))

  if (is.null(results) || nrow(results) == 0) {
    warning("No valid comparisons (need >= 3 samples per tissue).",
            call. = FALSE)
    return(NULL)
  }

  # Step 6: Multiple testing correction
  results$FDR.t <- stats::p.adjust(results$p.value.t, method = "BH")
  results$FDR.w <- stats::p.adjust(results$p.value.w, method = "BH")

  # Significance markers (based on t-test FDR)
  results <- .add_sig_markers(results, p_col = "p.value.t", padj_col = "FDR.t")

  # Wide-format tables
  logfc_tb <- reshape2::dcast(results, cluster ~ tissue, value.var = "logFC")
  fdr_tb   <- reshape2::dcast(results, cluster ~ tissue, value.var = "FDR.t")
  sig_tb   <- reshape2::dcast(results, cluster ~ tissue, value.var = "sig")

  list(
    detail     = results,
    logFC.tb   = logfc_tb,
    FDR.tb     = fdr_tb,
    charSig.tb = sig_tb
  )
}


# ============================================================================
# Internal: Per-patient analysis
# ============================================================================
#' @keywords internal
.tissue_dist_by_patient <- function(metadata, col_cluster, col_tissue,
                                    col_patient, method) {
  patients <- sort(unique(metadata[[col_patient]]))
  message("Running per-patient analysis for ", length(patients), " patients...")

  result_list <- lapply(patients, function(pat) {
    sub <- metadata[metadata[[col_patient]] == pat, , drop = FALSE]
    contingency <- table(sub[[col_cluster]], sub[[col_tissue]])

    if (nrow(contingency) < 2 || ncol(contingency) < 2) {
      warning("Patient '", pat, "': insufficient categories, skipping.",
              call. = FALSE)
      return(NULL)
    }

    if (method == "chisq") {
      res <- tryCatch({
        ct <- stats::chisq.test(contingency)
        ct$observed / ct$expected
      }, error = function(e) {
        warning("Patient '", pat, "': chisq.test failed: ", e$message,
                call. = FALSE)
        NULL
      })
    } else {
      # fisher
      detail <- .pairwise_fisher(contingency)
      res <- reshape2::dcast(detail, rid ~ cid, value.var = "OR")
    }
    res
  })

  names(result_list) <- patients
  result_list <- result_list[!vapply(result_list, is.null, logical(1))]

  if (length(result_list) == 0) {
    warning("No valid per-patient results.", call. = FALSE)
    return(NULL)
  }

  result_list
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Run pairwise 2x2 Fisher tests on a contingency table
#' @keywords internal
.pairwise_fisher <- function(contingency) {
  total    <- sum(contingency)
  row_sums <- rowSums(contingency)
  col_sums <- colSums(contingency)
  rnames   <- rownames(contingency)
  cnames   <- colnames(contingency)

  results <- vector("list", nrow(contingency) * ncol(contingency))
  idx <- 1L

  for (i in seq_len(nrow(contingency))) {
    for (j in seq_len(ncol(contingency))) {
      a <- contingency[i, j]
      b <- row_sums[i] - a
      cc <- col_sums[j] - a
      d <- total - a - b - cc

      mat_2x2 <- matrix(c(a, cc, b, d), nrow = 2)
      ft <- tryCatch(
        stats::fisher.test(mat_2x2),
        error = function(e) list(estimate = NA_real_, p.value = NA_real_)
      )

      results[[idx]] <- data.frame(
        rid     = rnames[i],
        cid     = cnames[j],
        count   = a,
        OR      = as.numeric(ft$estimate),
        p.value = ft$p.value,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  result_df <- do.call(rbind, results)
  result_df$p.adj <- stats::p.adjust(result_df$p.value, method = "BH")
  result_df
}


#' Add significance markers to a results data.frame
#' @keywords internal
.add_sig_markers <- function(df, p_col = "p.value", padj_col = "p.adj") {
  df$sig <- ""
  df$sig[!is.na(df[[padj_col]]) &
         df[[padj_col]] < 0.1 &
         df[[p_col]] < 0.05] <- "\u2020"     # dagger
  df$sig[!is.na(df[[padj_col]]) &
         df[[padj_col]] < 0.05] <- "\u2731"  # *
  df$sig[!is.na(df[[padj_col]]) &
         df[[padj_col]] < 0.01] <- "\u2731\u2731"
  df
}


# ============================================================================
# Plot: Tissue distribution heatmap
# ============================================================================

#' Plot Tissue Distribution Heatmap
#'
#' @description
#' Creates a ComplexHeatmap visualization for tissue distribution results.
#' Supports O/E ratio (from chisq), OR (from fisher), and logFC (from freq)
#' methods. Optionally overlays cell annotations using customizable symbol
#' markers (e.g., +++, ++, +, +/-, -) based on value ranges.
#'
#' @param result Output from \code{\link{sc_calc_tissue_dist}}.
#'   \itemize{
#'     \item If \code{method = "chisq"}: the O/E ratio matrix directly.
#'     \item If \code{method = "fisher"}: the list; uses \code{OR.tb}.
#'     \item If \code{method = "freq"}: the list; uses \code{logFC.tb}.
#'   }
#' @param method Character. Must match the method used in
#'   \code{sc_calc_tissue_dist}. One of \code{"chisq"}, \code{"fisher"},
#'   or \code{"freq"}.
#' @param show_values Logical. If \code{TRUE}, display the numeric values
#'   in each cell. Default is \code{TRUE}.
#' @param show_symbols Logical. If \code{TRUE}, display symbol markers
#'   (e.g., +++, ++, +, +/-, -) based on value ranges. Replaces numeric
#'   values. Default is \code{FALSE}.
#' @param symbol_breaks Numeric vector (length 4) defining boundaries for
#'   symbol annotation from low to high. Default is \code{c(0, 0.2, 0.8, 1)}.
#'   Mapping: \code{0} = "-", \code{(0, 0.2)} = "+/-", \code{[0.2, 0.8]} = "+",
#'   \code{(0.8, 1]} = "++", \code{> 1} = "+++". You can customize these for
#'   your specific data range.
#' @param legend_title Character. Title for the color legend. If \code{NULL},
#'   auto-determined from method. Default is \code{NULL}.
#' @param colors Character vector of length 3 for the color gradient:
#'   \code{c(low, mid, high)}. Default is \code{c("#2166AC", "white", "#B2182B")}
#'   (blue-white-red).
#' @param mid_value Numeric. The midpoint value for centering the color scale.
#'   Default is \code{1} for chisq/fisher (O/E = 1 means no enrichment),
#'   \code{0} for freq (logFC = 0 means no change). If \code{NULL},
#'   auto-determined from method.
#' @param cluster_rows Logical. Cluster rows. Default is \code{TRUE}.
#' @param cluster_columns Logical. Cluster columns. Default is \code{TRUE}.
#' @param fontsize_cell Numeric. Font size for cell text. Default is \code{8}.
#' @param fontsize_labels Numeric. Font size for row/column labels.
#'   Default is \code{10}.
#' @param ... Additional arguments passed to
#'   \code{ComplexHeatmap::Heatmap()}.
#'
#' @return A \code{ComplexHeatmap::Heatmap} object (invisibly). The heatmap
#'   is drawn as a side effect.
#'
#' @examples
#' \dontrun{
#' # Chisq O/E ratio heatmap
#' oe <- sc_calc_tissue_dist(meta, col_cluster = "celltype",
#'                        col_tissue = "sample_type", method = "chisq")
#' sc_plot_tissue_dist(oe, method = "chisq")
#'
#' # With symbol annotations
#' sc_plot_tissue_dist(oe, method = "chisq", show_symbols = TRUE)
#'
#' # Custom symbol breaks (for values ranging 0~3)
#' sc_plot_tissue_dist(oe, method = "chisq", show_symbols = TRUE,
#'                  symbol_breaks = c(0, 0.5, 1.0, 1.5))
#'
#' # Fisher method
#' res_f <- sc_calc_tissue_dist(meta, col_cluster = "celltype",
#'                           col_tissue = "sample_type", method = "fisher")
#' sc_plot_tissue_dist(res_f, method = "fisher")
#'
#' # Freq method
#' res_q <- sc_calc_tissue_dist(meta, col_cluster = "celltype",
#'                           col_tissue = "sample_type",
#'                           col_sample = "orig.ident", method = "freq")
#' sc_plot_tissue_dist(res_q, method = "freq")
#' }
#'
#' @seealso \code{\link{sc_calc_tissue_dist}}
#' @export
sc_plot_tissue_dist <- function(result,
                             method = c("chisq", "fisher", "freq"),
                             show_values = TRUE,
                             show_symbols = FALSE,
                             symbol_breaks = c(0, 0.2, 0.8, 1),
                             legend_title = NULL,
                             colors = c("#2166AC", "white", "#B2182B"),
                             mid_value = NULL,
                             cluster_rows = TRUE,
                             cluster_columns = TRUE,
                             fontsize_cell = 8,
                             fontsize_labels = 10,
                             ...) {

  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("ComplexHeatmap is required. Install with:\n",
         "  BiocManager::install('ComplexHeatmap')", call. = FALSE)
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("circlize is required. Install with:\n",
         "  install.packages('circlize')", call. = FALSE)
  }

  method <- match.arg(method)

  # --- Extract matrix ---
  if (method == "chisq") {
    if (!is.matrix(result)) {
      stop("For method='chisq', result should be the O/E matrix from ",
           "sc_calc_tissue_dist().", call. = FALSE)
    }
    mat <- result
    if (is.null(legend_title)) legend_title <- "Ro/e"
    if (is.null(mid_value)) mid_value <- 1
  } else if (method == "fisher") {
    if (!is.list(result) || is.null(result$OR.tb)) {
      stop("For method='fisher', result should be the list from ",
           "sc_calc_tissue_dist(method='fisher').", call. = FALSE)
    }
    or_df <- result$OR.tb
    rn <- or_df[[1]]
    mat <- as.matrix(or_df[, -1, drop = FALSE])
    rownames(mat) <- rn
    if (is.null(legend_title)) legend_title <- "Odds Ratio"
    if (is.null(mid_value)) mid_value <- 1
  } else {
    # freq
    if (!is.list(result) || is.null(result$logFC.tb)) {
      stop("For method='freq', result should be the list from ",
           "sc_calc_tissue_dist(method='freq').", call. = FALSE)
    }
    lfc_df <- result$logFC.tb
    rn <- lfc_df[[1]]
    mat <- as.matrix(lfc_df[, -1, drop = FALSE])
    rownames(mat) <- rn
    if (is.null(legend_title)) legend_title <- "log2FC"
    if (is.null(mid_value)) mid_value <- 0
  }

  # --- Color function ---
  val_min <- min(mat, na.rm = TRUE)
  val_max <- max(mat, na.rm = TRUE)
  col_fun <- circlize::colorRamp2(
    c(val_min, mid_value, val_max),
    colors
  )

  # --- Cell annotation function ---
  cell_fn <- NULL
  if (show_symbols) {
    if (length(symbol_breaks) != 4) {
      stop("symbol_breaks must be a numeric vector of length 4.", call. = FALSE)
    }
    sb <- sort(symbol_breaks)
    cell_fn <- function(j, i, x, y, width, height, fill) {
      v <- mat[i, j]
      if (is.na(v)) {
        label <- "NA"
      } else if (v == sb[1]) {
        label <- "-"
      } else if (v > sb[1] & v < sb[2]) {
        label <- "+/-"
      } else if (v >= sb[2] & v <= sb[3]) {
        label <- "+"
      } else if (v > sb[3] & v <= sb[4]) {
        label <- "++"
      } else if (v > sb[4]) {
        label <- "+++"
      } else {
        label <- "-"
      }
      grid::grid.text(label, x, y,
                      gp = grid::gpar(fontsize = fontsize_cell, col = "black"))
    }
  } else if (show_values) {
    cell_fn <- function(j, i, x, y, width, height, fill) {
      v <- mat[i, j]
      label <- if (is.na(v)) "NA" else sprintf("%.2f", v)
      grid::grid.text(label, x, y,
                      gp = grid::gpar(fontsize = fontsize_cell, col = "black"))
    }
  }

  # --- Build heatmap ---
  ht <- ComplexHeatmap::Heatmap(
    mat,
    name                   = legend_title,
    col                    = col_fun,
    cluster_rows           = cluster_rows,
    cluster_columns        = cluster_columns,
    show_row_names         = TRUE,
    show_column_names      = TRUE,
    row_names_side         = "right",
    row_names_gp           = grid::gpar(fontsize = fontsize_labels),
    column_names_gp        = grid::gpar(fontsize = fontsize_labels),
    cell_fun               = cell_fn,
    heatmap_legend_param   = list(title = legend_title),
    ...
  )

  ComplexHeatmap::draw(ht)
  invisible(ht)
}
