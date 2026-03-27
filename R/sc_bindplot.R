#' Calculate Cell Type Proportions Per Sample
#'
#' @description
#' Computes the proportion of each cell type (cluster) within each sample,
#' grouped by experimental conditions. Input can be a Seurat object or a
#' data.frame.
#'
#' @param input A Seurat object, or a data.frame containing at least the
#'   columns specified by \code{cluster_col}, \code{group_col}, and
#'   \code{sample_col}.
#' @param cluster_col Character. Column name for cell type / cluster
#'   annotation. Default is \code{"seurat_clusters"}.
#' @param group_col Character. Column name for experimental group / condition.
#'   Default is \code{"group"}.
#' @param sample_col Character. Column name for sample ID.
#'   Default is \code{"orig.ident"}.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{sample}{Sample ID}
#'     \item{group}{Experimental group}
#'     \item{cluster}{Cell type / cluster}
#'     \item{n}{Cell count}
#'     \item{prop}{Proportion within the sample (0-1)}
#'   }
#'
#' @examples
#' \dontrun{
#' # From Seurat object
#' prop_df <- sc_bindprop(seurat_obj,
#'     cluster_col = "subtype_anno",
#'     group_col = "Treatment",
#'     sample_col = "orig.ident"
#' )
#'
#' # From a data.frame
#' meta <- seurat_obj@meta.data
#' prop_df <- sc_bindprop(meta,
#'     cluster_col = "celltype",
#'     group_col = "condition",
#'     sample_col = "sample_id"
#' )
#' }
#'
#' @export
sc_bindprop <- function(input,
                        cluster_col = "seurat_clusters",
                        group_col = "group",
                        sample_col = "orig.ident") {
    # --- Extract metadata ---
    if (inherits(input, "Seurat")) {
        if (!requireNamespace("Seurat", quietly = TRUE) &&
            !requireNamespace("SeuratObject", quietly = TRUE)) {
            stop("Seurat or SeuratObject is required for Seurat input.", call. = FALSE)
        }
        meta <- input@meta.data
    } else if (is.data.frame(input)) {
        meta <- input
    } else {
        stop("input must be a Seurat object or a data.frame.", call. = FALSE)
    }

    # --- Validate columns ---
    required_cols <- c(cluster_col, group_col, sample_col)
    missing <- setdiff(required_cols, colnames(meta))
    if (length(missing) > 0) {
        stop("Missing columns in metadata: ",
            paste(missing, collapse = ", "),
            "\nAvailable columns: ",
            paste(head(colnames(meta), 20), collapse = ", "),
            call. = FALSE
        )
    }

    # --- Build working data.frame ---
    df <- data.frame(
        sample = meta[[sample_col]],
        group = meta[[group_col]],
        cluster = meta[[cluster_col]],
        stringsAsFactors = FALSE
    )

    # Remove NAs
    n_before <- nrow(df)
    df <- df[complete.cases(df), ]
    n_removed <- n_before - nrow(df)
    if (n_removed > 0) {
        message("Removed ", n_removed, " cells with NA values.")
    }

    # --- Calculate proportions ---
    #
    # ====== Reference: dplyr approach 1 (group_by + summarise) ======
    # prop_sample <-
    #   df %>%
    #   group_by(sample, group, cluster) %>%
    #   summarise(n = n(), .groups = "drop") %>%
    #   group_by(sample, group) %>%
    #   mutate(prop = n / sum(n)) %>%
    #   ungroup()
    #
    # ====== Reference: dplyr approach 2 (count + mutate) ======
    # cell_pro_new <-
    #   df %>%
    #   dplyr::count(cluster, group, sample, name = "n", sort = TRUE) %>%
    #   group_by(sample, group) %>%
    #   mutate(prop = n / sum(n)) %>%
    #   ungroup()
    #
    # # verify: should all be 1
    # cell_pro_new %>% group_by(sample, group) %>% summarise(total = sum(prop))
    #
    # ====== Actual implementation: base R (no dplyr dependency) ======
    counts <- stats::aggregate(
        list(n = rep(1, nrow(df))),
        by = list(sample = df$sample, group = df$group, cluster = df$cluster),
        FUN = sum
    )

    # Calculate total per sample
    totals <- stats::aggregate(n ~ sample + group, data = counts, FUN = sum)
    colnames(totals)[3] <- "total"

    # Merge and compute proportion
    result <- merge(counts, totals, by = c("sample", "group"))
    result$prop <- result$n / result$total
    result$total <- NULL

    # Sort
    result <- result[order(result$sample, result$cluster), ]
    rownames(result) <- NULL

    return(result)
}


#' Bindplot: Cell Type Proportion Comparison
#'
#' @description
#' Creates a publication-quality boxplot comparing cell type proportions
#' across experimental groups. Each facet represents a cell type, with
#' individual sample points overlaid and statistical comparisons.
#'
#' Input can be a Seurat object, a data.frame of raw metadata, or the
#' output of \code{\link{sc_bindprop}}.
#'
#' @param input A Seurat object, a data.frame with metadata columns, or
#'   the output of \code{sc_bindprop}.
#' @param cluster_col Character. Column name for cell type / cluster.
#'   Default is \code{"seurat_clusters"}. Ignored if input is from
#'   \code{sc_bindprop}.
#' @param group_col Character. Column name for experimental group.
#'   Default is \code{"group"}. Ignored if input is from \code{sc_bindprop}.
#' @param sample_col Character. Column name for sample ID.
#'   Default is \code{"orig.ident"}. Ignored if input is from
#'   \code{sc_bindprop}.
#' @param palette Character. Goldfish palette name for fill colors.
#'   One of: "classic", "warm", "balanced", "bright", "sunset".
#'   Default is \code{"balanced"}. Set to \code{NULL} to use ggplot2 defaults.
#' @param comparisons List of length-2 character vectors specifying
#'   pairwise comparisons for statistical testing, e.g.,
#'   \code{list(c("Ctrl", "Treat"))}. Default is \code{NULL} (global
#'   Kruskal-Wallis test only).
#' @param test_method Character. Statistical test method for pairwise
#'   comparisons. One of \code{"wilcox.test"} or \code{"t.test"}.
#'   Default is \code{"wilcox.test"}.
#' @param free_y Logical. If \code{TRUE}, each facet has a free y-axis
#'   scale. Default is \code{FALSE} (shared scale for easier comparison).
#' @param ncol Integer. Number of columns in facet layout. Default is
#'   \code{NULL} (auto).
#' @param point_size Numeric. Size of individual sample points.
#'   Default is 2.
#' @param title Character. Plot title. Default is \code{NULL}.
#' @param cluster_filter Character vector. If provided, only plot these
#'   clusters. Default is \code{NULL} (plot all).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Direct from Seurat object (simplest)
#' sc_bindplot(seurat_obj,
#'     cluster_col = "subtype_anno",
#'     group_col = "Treatment",
#'     sample_col = "orig.ident"
#' )
#'
#' # With pairwise comparisons
#' sc_bindplot(seurat_obj,
#'     cluster_col = "major_lineage",
#'     group_col = "condition",
#'     comparisons = list(c("Ctrl", "Drug_A"), c("Ctrl", "Drug_B")),
#'     palette = "warm"
#' )
#'
#' # From pre-computed proportions
#' prop_df <- sc_bindprop(seurat_obj,
#'     cluster_col = "celltype",
#'     group_col = "Treatment", sample_col = "orig.ident"
#' )
#' sc_bindplot(prop_df)
#'
#' # Filter specific cell types
#' sc_bindplot(seurat_obj,
#'     cluster_col = "subtype_anno",
#'     group_col = "Treatment",
#'     cluster_filter = c("CD8_T", "NK", "Treg")
#' )
#' }
#'
#' @seealso \code{\link{sc_bindprop}}
#'
#' @export
sc_bindplot <- function(input,
                        cluster_col = "seurat_clusters",
                        group_col = "group",
                        sample_col = "orig.ident",
                        palette = "balanced",
                        comparisons = NULL,
                        test_method = "wilcox.test",
                        free_y = FALSE,
                        ncol = NULL,
                        point_size = 2,
                        title = NULL,
                        cluster_filter = NULL) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required.", call. = FALSE)
    }

    # --- Prepare proportion data ---
    # Check if input is already from sc_bindprop
    is_prop_df <- is.data.frame(input) &&
        all(c("sample", "group", "cluster", "n", "prop") %in% colnames(input))

    if (is_prop_df) {
        prop_df <- input
    } else {
        prop_df <- sc_bindprop(input,
            cluster_col = cluster_col,
            group_col = group_col,
            sample_col = sample_col
        )
    }

    # --- Filter clusters if requested ---
    if (!is.null(cluster_filter)) {
        prop_df <- prop_df[prop_df$cluster %in% cluster_filter, ]
        if (nrow(prop_df) == 0) {
            stop("No data remaining after filtering. ",
                "Check cluster_filter values.",
                call. = FALSE
            )
        }
    }

    # --- Determine facet scale ---
    facet_scales <- if (free_y) "free_y" else "fixed"

    # --- Number of groups for determining plot style ---
    n_groups <- length(unique(prop_df$group))

    # --- Build the plot ---
    p <- ggplot2::ggplot(
        prop_df,
        ggplot2::aes(x = .data$group, y = .data$prop)
    )

    # Boxplot layer
    p <- p + ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data$group),
        alpha = 0.7,
        outlier.shape = NA,
        width = 0.6,
        linewidth = 0.4
    )

    # Jitter points
    if (n_groups <= 2) {
        # Few groups: use jitter
        p <- p + ggplot2::geom_jitter(
            ggplot2::aes(fill = .data$group),
            shape = 21,
            size = point_size,
            alpha = 0.8,
            width = 0.15,
            stroke = 0.3
        )
    } else {
        # Many groups: use plain points
        p <- p + ggplot2::geom_point(
            ggplot2::aes(fill = .data$group),
            shape = 21,
            size = point_size,
            alpha = 0.8,
            position = ggplot2::position_jitterdodge(jitter.width = 0.1),
            stroke = 0.3
        )
    }

    # Facet by cluster
    p <- p + ggplot2::facet_wrap(~cluster, scales = facet_scales, ncol = ncol)

    # --- Statistical comparisons ---
    if (!is.null(comparisons)) {
        if (requireNamespace("ggpubr", quietly = TRUE)) {
            p <- p + ggpubr::stat_compare_means(
                comparisons = comparisons,
                method = test_method,
                label = "p.signif",
                size = 3.5
            )
        } else {
            warning("Install ggpubr for pairwise comparisons: ",
                "install.packages('ggpubr')",
                call. = FALSE
            )
        }
    } else {
        # Global test (Kruskal-Wallis or Wilcoxon for 2 groups)
        if (requireNamespace("ggpubr", quietly = TRUE)) {
            p <- p + ggpubr::stat_compare_means(
                size = 3,
                label.x.npc = "left",
                label.y.npc = "top"
            )
        }
    }

    # --- Goldfish palette ---
    if (!is.null(palette)) {
        tryCatch(
            {
                p <- p + scale_fill_goldfish(palette = palette)
            },
            error = function(e) {
                # Fallback if palette not found
                NULL
            }
        )
    }

    # --- Theme ---
    p <- p +
        ggplot2::labs(
            x = NULL,
            y = "Proportion",
            fill = "Group",
            title = title
        ) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
            # Facet strips
            strip.background = ggplot2::element_rect(
                fill = "#f0f0f0", color = NA
            ),
            strip.text = ggplot2::element_text(
                face = "bold", size = 10, margin = ggplot2::margin(4, 4, 4, 4)
            ),
            # Axis
            axis.text.x = ggplot2::element_text(
                angle = 45, vjust = 1, hjust = 1, size = 9
            ),
            axis.text.y = ggplot2::element_text(size = 9),
            axis.title.y = ggplot2::element_text(size = 11, face = "bold"),
            # Legend
            legend.position = "bottom",
            legend.title = ggplot2::element_text(face = "bold", size = 10),
            legend.text = ggplot2::element_text(size = 9),
            # Spacing
            panel.spacing = ggplot2::unit(0.6, "lines"),
            # Title
            plot.title = ggplot2::element_text(
                hjust = 0.5, face = "bold", size = 14
            ),
            plot.margin = ggplot2::margin(10, 10, 10, 10)
        )

    return(p)
}
