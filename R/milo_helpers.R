#' Evaluate Milo Neighbourhood-Level DA Importance
#'
#' @description
#' Scores and ranks cell subtypes from Milo differential abundance (DA)
#' analysis results. Filters neighbourhoods by subtype purity, then
#' summarises and scores each subtype across six dimensions (SpatialFDR,
#' percent significant, logFC consistency, purity, effect size, and
#' neighbourhood count) to assign importance labels.
#'
#' @param da_results A data.frame of Milo DA results, typically from
#'   \code{miloR::testNhoods()}. Must contain columns \code{logFC},
#'   \code{SpatialFDR}, and the columns specified by \code{subtype_col}
#'   and \code{fraction_col}.
#' @param subtype_col Character. Column name in \code{da_results} that
#'   holds subtype annotations (e.g. from \code{annotateNhoods()}).
#'   Default is \code{"subtype_anno"}.
#' @param fraction_col Character. Column name holding the subtype
#'   annotation fraction (purity) per neighbourhood. Default is
#'   \code{"subtype_anno_fraction"}.
#' @param spatialfdr_thresh Numeric. SpatialFDR threshold for calling
#'   a neighbourhood significant. Default is 0.1.
#' @param purity_thresh Numeric. Minimum subtype purity fraction to
#'   retain a neighbourhood. Default is 0.7.
#' @param min_nhoods Integer. Minimum number of retained neighbourhoods
#'   for a subtype to be considered reliable. Default is 3.
#'
#' @return A data.frame with one row per subtype, sorted by priority
#'   and total score (descending). Columns include:
#'   \describe{
#'     \item{subtype_anno}{Subtype annotation}
#'     \item{importance}{Importance label (e.g. "\u2b50\u2b50\u2b50 \u5173\u952e\u4e9a\u7fa4")}
#'     \item{total_score}{Composite score (0-9)}
#'     \item{direction}{"enriched" or "depleted"}
#'     \item{n_nhoods}{Number of retained neighbourhoods}
#'     \item{mean_logFC, logFC_cv, pct_sig, mean_purity, mean_sfdr}{
#'       Summary statistics}
#'     \item{score_sfdr, score_pct, score_cv, score_purity, score_effect,
#'       score_nhood}{Individual dimension scores}
#'   }
#'
#' @details
#' The scoring system works as follows:
#' \itemize{
#'   \item \strong{Core dimensions} (0-2 points each, max 6):
#'     SpatialFDR level, percent significant neighbourhoods,
#'     logFC coefficient of variation
#'   \item \strong{Auxiliary dimensions} (0-1 point each, max 3):
#'     mean purity, effect size (|logFC|), neighbourhood count
#' }
#'
#' @examples
#' \dontrun{
#' # After running Milo DA analysis
#' da_results <- testNhoods(milo_obj, design, model.contrasts = contrast)
#' da_results <- annotateNhoods(milo_obj, da_results, coldata_col = "subtype_anno")
#'
#' importance <- sc_evaluate_milo_importance(da_results)
#' print(importance)
#' }
#'
#' @importFrom magrittr %>%
#' @seealso \code{\link{sc_plot_milo_DA}}
#' @export
sc_evaluate_milo_importance <- function(
    da_results,
    subtype_col       = "subtype_anno",
    fraction_col      = "subtype_anno_fraction",
    spatialfdr_thresh = 0.1,
    purity_thresh     = 0.7,
    min_nhoods        = 3
) {
  # ── 第一步：neighbourhood 层面过滤 ────────────────────────────────────────
  dat <- da_results %>%
    dplyr::filter(.data[[fraction_col]] >= purity_thresh)

  if (nrow(dat) == 0) stop("过滤后无数据，请降低 purity_thresh", call. = FALSE)

  # ── 第二步：亚群层面汇总 ───────────────────────────────────────────────────
  summary_df <- dat %>%
    dplyr::group_by(.data[[subtype_col]]) %>%
    dplyr::summarise(
      n_nhoods     = dplyr::n(),
      mean_logFC   = mean(logFC,     na.rm = TRUE),
      logFC_sd     = stats::sd(logFC, na.rm = TRUE),
      mean_purity  = mean(.data[[fraction_col]], na.rm = TRUE),
      mean_sfdr    = mean(SpatialFDR, na.rm = TRUE),
      pct_sig      = mean(SpatialFDR < spatialfdr_thresh, na.rm = TRUE),
      direction    = ifelse(mean(logFC, na.rm = TRUE) > 0,
                            "enriched", "depleted"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      logFC_cv = ifelse(abs(mean_logFC) < 1e-6, Inf,
                        logFC_sd / abs(mean_logFC))
    )

  # ── 第三步：打分系统 ───────────────────────────────────────────────────────
  score_df <- summary_df %>%
    dplyr::mutate(
      # 核心维度（各 0~2 分，共 6 分）
      score_sfdr = dplyr::case_when(
        mean_sfdr < 0.05  ~ 2,
        mean_sfdr < 0.1   ~ 1,
        TRUE              ~ 0
      ),
      score_pct = dplyr::case_when(
        pct_sig >= 0.7    ~ 2,
        pct_sig >= 0.4    ~ 1,
        TRUE              ~ 0
      ),
      score_cv = dplyr::case_when(
        logFC_cv < 0.5    ~ 2,
        logFC_cv < 1.0    ~ 1,
        TRUE              ~ 0
      ),
      # 辅助维度（各 0~1 分，共 3 分）
      score_purity = dplyr::case_when(
        mean_purity >= 0.8  ~ 1,
        mean_purity >= 0.6  ~ 0.5,
        TRUE                ~ 0
      ),
      score_effect = dplyr::case_when(
        abs(mean_logFC) >= 2  ~ 1,
        abs(mean_logFC) >= 1  ~ 0.5,
        TRUE                  ~ 0
      ),
      score_nhood = dplyr::case_when(
        n_nhoods >= 10  ~ 1,
        n_nhoods >= 5   ~ 0.5,
        TRUE            ~ 0
      ),
      # 总分（满分 9）
      total_score = score_sfdr + score_pct + score_cv +
        score_purity + score_effect + score_nhood,
      # 重要性等级
      importance = dplyr::case_when(
        n_nhoods < min_nhoods           ~ "\u274c \u6837\u672c\u4e0d\u8db3",
        logFC_cv == Inf                 ~ "\u274c \u4f4e\u53ef\u4fe1\uff08logFC\u22480\uff09",
        total_score >= 7                ~ "\u2b50\u2b50\u2b50 \u5173\u952e\u4e9a\u7fa4",
        total_score >= 5                ~ "\u2b50\u2b50 \u91cd\u8981\u4e9a\u7fa4",
        total_score >= 3                ~ "\u2b50 \u5019\u9009\u4e9a\u7fa4",
        TRUE                            ~ "\u2014 \u4f4e\u53ef\u4fe1"
      ),
      # 方便排序的优先级数值
      priority = dplyr::case_when(
        grepl("\u5173\u952e", importance) ~ 4,
        grepl("\u91cd\u8981", importance) ~ 3,
        grepl("\u5019\u9009", importance) ~ 2,
        TRUE                              ~ 1
      )
    ) %>%
    dplyr::arrange(dplyr::desc(priority), dplyr::desc(total_score))

  # ── 第四步：整理输出 ───────────────────────────────────────────────────────
  result <- score_df %>%
    dplyr::select(
      subtype_anno   = .data[[subtype_col]],
      importance,
      total_score,
      direction,
      n_nhoods,
      mean_logFC,
      logFC_cv,
      pct_sig,
      mean_purity,
      mean_sfdr,
      score_sfdr, score_pct, score_cv,
      score_purity, score_effect, score_nhood
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.double), ~ round(.x, 3)))

  return(result)
}


#' Plot Milo Differential Abundance Results
#'
#' @description
#' Creates a comprehensive set of Milo DA visualisation plots including
#' QC plots (p-value histogram, volcano), UMAP with group coloring,
#' neighbourhood graph coloured by logFC, and beeswarm plot by subtype.
#'
#' @param milo_obj A \code{Milo} object from the miloR package.
#' @param da_results A data.frame of DA results from \code{testNhoods()}.
#'   Must contain columns \code{logFC}, \code{SpatialFDR}, and \code{PValue}.
#' @param comparison_name Character. Label for the comparison, used in
#'   plot titles and output filenames (e.g. \code{"CRT_vs_Tumor"}).
#' @param group_col Character. Column name in \code{colData(milo_obj)}
#'   for sample group. Default is \code{"Treatshort"}.
#' @param subtype_col Character. Column name for subtype annotation in
#'   both \code{colData} and \code{da_results}. Default is
#'   \code{"subtype_anno"}.
#' @param dimred Character. Name of the dimensionality reduction to use
#'   for UMAP and nhood graph layout. Default is \code{"UMAP.T_HARMONY"}.
#' @param groups_to_show Character vector. Subset of groups to display
#'   on the UMAP. \code{NULL} (default) shows all groups.
#' @param group_colors Named character vector of colours for groups.
#'   \code{NULL} (default) uses ggplot2 defaults.
#' @param fdr_thresh Numeric. FDR threshold for volcano plot dashed line.
#'   Default is 0.1.
#' @param alpha_nhood Numeric. Transparency for \code{plotNhoodGraphDA()}.
#'   Default is 0.2.
#' @param text_size Numeric. Text size for UMAP labels. Default is 3.
#' @param point_size Numeric. Point size for UMAP. Default is 0.5.
#' @param bee_alpha Numeric. Alpha for beeswarm points. Default is 0.50.
#' @param save_plots Logical. Whether to save plots to disk. Default
#'   is \code{FALSE}.
#' @param out_dir Character. Output directory when \code{save_plots = TRUE}.
#'   Default is \code{"."}.
#'
#' @return A named list (invisible) with elements:
#'   \describe{
#'     \item{qc}{Patchwork of p-value histogram + volcano plot}
#'     \item{umap_nhood}{Patchwork of UMAP + nhood graph}
#'     \item{beeswarm}{Beeswarm ggplot (or \code{NULL} if subtype column
#'       not found in \code{da_results})}
#'   }
#'
#' @details
#' This function requires the \pkg{miloR}, \pkg{patchwork}, \pkg{scater},
#' and \pkg{SingleCellExperiment} packages. If not installed, the function
#' will stop with an informative error.
#'
#' @examples
#' \dontrun{
#' # After Milo DA analysis
#' plots <- sc_plot_milo_DA(
#'   milo_obj, da_results,
#'   comparison_name = "CRT_vs_Tumor",
#'   group_col       = "Treatshort",
#'   subtype_col     = "subtype_anno",
#'   dimred          = "UMAP.T_HARMONY",
#'   groups_to_show  = c("CRT", "Tumor"),
#'   save_plots      = TRUE,
#'   out_dir         = "milo_plots"
#' )
#'
#' # Access individual plots
#' plots$qc
#' plots$umap_nhood
#' plots$beeswarm
#' }
#'
#' @seealso \code{\link{sc_evaluate_milo_importance}}
#' @export
sc_plot_milo_DA <- function(
    milo_obj,
    da_results,
    comparison_name,
    group_col    = "Treatshort",
    subtype_col  = "subtype_anno",
    dimred       = "UMAP.T_HARMONY",
    groups_to_show = NULL,
    group_colors = NULL,
    fdr_thresh   = 0.1,
    alpha_nhood  = 0.2,
    text_size    = 3,
    point_size   = 0.5,
    bee_alpha    = 0.50,
    save_plots   = FALSE,
    out_dir      = "."
) {
  # ── 0. 检查依赖 ─────────────────────────────────────────────────────────────
  for (pkg in c("miloR", "patchwork", "SingleCellExperiment", "scater")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required but not installed.\n",
           "  Install with: BiocManager::install('", pkg, "')",
           call. = FALSE)
    }
  }

  # ── 0b. 输入检查 ────────────────────────────────────────────────────────────
  stopifnot(
    inherits(milo_obj, "Milo"),
    is.data.frame(da_results),
    all(c("logFC", "SpatialFDR", "PValue") %in% colnames(da_results))
  )

  message("\u2500\u2500 [", comparison_name, "] \u5f00\u59cb\u7ed8\u56fe \u2500\u2500")

  # ── 1. 建 nhood graph（若尚未建） ────────────────────────────────────────────
  if (!"graph" %in% names(miloR::nhoodGraph(milo_obj))) {
    message("  buildNhoodGraph ...")
    milo_obj <- miloR::buildNhoodGraph(milo_obj)
  }

  # ── 2. QC 图：p 值分布 + volcano ────────────────────────────────────────────
  p_hist <- ggplot2::ggplot(da_results, ggplot2::aes(PValue)) +
    ggplot2::geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    ggplot2::labs(title = paste0(comparison_name, " | p-value distribution")) +
    ggplot2::theme_classic()

  p_volcano <- ggplot2::ggplot(
    da_results,
    ggplot2::aes(logFC, -log10(SpatialFDR))
  ) +
    ggplot2::geom_point(alpha = 0.6, color = "grey40") +
    ggplot2::geom_hline(
      yintercept = -log10(fdr_thresh),
      linetype   = "dashed",
      color      = "red"
    ) +
    ggplot2::labs(
      title = paste0(comparison_name, " | Volcano"),
      y     = "-log10(SpatialFDR)"
    ) +
    ggplot2::theme_classic()

  p_qc <- p_hist + p_volcano
  message("  QC \u56fe\u5b8c\u6210")

  # ── 3. UMAP 图（可 subset 分组） ────────────────────────────────────────────
  if (!is.null(groups_to_show)) {
    milo_plot <- milo_obj[, milo_obj[[group_col]] %in% groups_to_show]
  } else {
    milo_plot <- milo_obj
  }

  umap_pl <- scater::plotReducedDim(
    milo_plot,
    dimred     = dimred,
    colour_by  = group_col,
    text_by    = subtype_col,
    text_size  = text_size,
    point_size = point_size
  ) + ggplot2::guides(fill = "none")

  if (!is.null(group_colors)) {
    umap_pl <- umap_pl +
      ggplot2::scale_color_manual(values = group_colors)
  }
  message("  UMAP \u56fe\u5b8c\u6210")

  # ── 4. Nhood graph（用完整 milo_obj） ────────────────────────────────────────
  nh_graph_pl <- miloR::plotNhoodGraphDA(
    milo_obj,
    da_results,
    layout = dimred,
    alpha  = alpha_nhood
  ) +
    ggplot2::scale_fill_gradient2(
      low      = "#3A3EC8",
      mid      = "white",
      high     = "#D82632",
      midpoint = 0,
      na.value = "grey90",
      name     = "logFC"
    ) +
    ggplot2::labs(title = paste0(comparison_name, " | Nhood DA"))

  message("  Nhood graph \u5b8c\u6210")

  # ── 5. 拼接 UMAP + Nhood ────────────────────────────────────────────────────
  p_umap_nhood <- umap_pl + nh_graph_pl +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(title = comparison_name)

  # ── 6. Beeswarm ─────────────────────────────────────────────────────────────
  if (!subtype_col %in% colnames(da_results)) {
    warning("da_results \u4e2d\u6ca1\u6709 '", subtype_col,
            "' \u5217\uff0c\u8bf7\u5148\u8fd0\u884c annotateNhoods()\uff0c\u8df3\u8fc7 beeswarm")
    p_bee <- NULL
  } else {
    p_bee <- miloR::plotDAbeeswarm(
      da_results,
      group.by = subtype_col,
      alpha    = bee_alpha
    ) +
      ggplot2::scale_color_gradient2(
        low = "blue", mid = "white", high = "red", midpoint = 0
      ) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = paste0(comparison_name, " | DA Beeswarm"))
    message("  Beeswarm \u5b8c\u6210")
  }

  # ── 7. 可选保存 ──────────────────────────────────────────────────────────────
  if (save_plots) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    safe_name <- gsub("[^A-Za-z0-9_]", "_", comparison_name)

    ggplot2::ggsave(
      file.path(out_dir, paste0(safe_name, "_qc.pdf")),
      p_qc, width = 10, height = 4
    )
    ggplot2::ggsave(
      file.path(out_dir, paste0(safe_name, "_umap_nhood.pdf")),
      p_umap_nhood, width = 12, height = 5
    )
    if (!is.null(p_bee)) {
      ggplot2::ggsave(
        file.path(out_dir, paste0(safe_name, "_beeswarm.pdf")),
        p_bee, width = 7, height = 8
      )
    }
    message("  \u56fe\u5df2\u4fdd\u5b58\u81f3 ", out_dir)
  }

  # ── 8. 返回所有图对象 ────────────────────────────────────────────────────────
  invisible(list(
    qc          = p_qc,
    umap_nhood  = p_umap_nhood,
    beeswarm    = p_bee
  ))
}
