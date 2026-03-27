# ============================================================
# sc_gsea_quadrant.R
# 用于单细胞 GSEA 四象限分析的可复用函数集
#
# 使用方法：source("sc_gsea_quadrant.R")
# 依赖：fgsea, dplyr, tibble, ggplot2, ggrepel, Seurat
# ============================================================


# ===========================================================
#  上游数据准备函数
# ===========================================================

# ----------------------------------------------------------
# 0a) compute_deg_ranked_lists
#     批量计算 DEG 并转为 fgsea 需要的 ranked vector
# ----------------------------------------------------------

#' @title 批量计算两个方向的 DEG ranked list
#'
#' @description
#' 对每个治疗组自动调用 Seurat::FindMarkers 计算：
#'   - Y 轴: 治疗组 vs 参照组 (如 Tumor)
#'   - X 轴: 应答组 vs 非应答组 (如 CR vs NCR)，仅在该治疗组内
#' 结果直接转换为 fgsea 可用的 named numeric vector (按 avg_log2FC 排序)。
#'
#' @section 方法选择说明 (Method Choice Rationale):
#' 默认使用 MAST 而非 wilcox 或 pseudobulk，原因如下：
#'
#' \strong{为什么不用 wilcox (Seurat 默认)?}
#' wilcox 不支持校正混杂变量 (细胞周期、解离应激、线粒体比例等)，
#' 这些因素会混淆条件间真实差异，导致 ranked list 排名不准确。
#'
#' \strong{为什么不用 pseudobulk (DESeq2)?}
#' Pseudobulk 是处理伪重复 (pseudoreplication) 的理论最优方法
#' (Squair et al. 2021, Nature Communications; Zimmerman et al. 2021),
#' 但对样本量要求高。当每组生物学重复 < 5 (如 CR=3, NCR=3) 时,
#' DESeq2 的 fold change 估计不稳定, 会导致 GSEA 排名噪声过大。
#'
#' \strong{为什么选 MAST?}
#' MAST (Finak et al. 2015, Genome Biology) 支持 latent.vars 校正混杂,
#' 结合了 hurdle model 处理单细胞 dropout, 在细胞水平运算能获得更稳定的
#' avg_log2FC 排名。虽然存在伪重复问题, 但 GSEA 主要依赖排名顺序
#' 而非 p 值精确度, 因此影响有限 (Haber et al. 2017, Nature 采用类似策略)。
#'
#' \strong{注意: 不设 max.cells.per.ident}
#' 找 marker 基因时推荐下采样 (如 500-800 cells) 以避免 p 值膨胀,
#' 但构建 GSEA ranked list 时, 更多细胞 = avg_log2FC 估计更精确 =
#' 排名更可靠, 因此此函数 \strong{故意不设} max.cells.per.ident。
#' 若组间严重不平衡, 可在外部手动 subset 后再调用本函数。
#'
#' @param seurat_obj Seurat 对象
#' @param treatment_col character, 存储治疗方式的 metadata 列名
#' @param treatment_levels character vector, 要分析的治疗组名称
#'   (必须是 treatment_col 中存在的值)
#' @param reference_group character, Y 轴参照组名称 (如 "Tumor")，
#'   也必须存在于 treatment_col 中
#' @param response_col character, 存储应答状态的 metadata 列名
#'   (如 "Pathshort")
#' @param response_pos character, 应答阳性组 (如 "CR")
#' @param response_neg character, 应答阴性组 (如 "NCR")
#' @param filter_pattern character, 用于过滤基因的正则表达式,
#'   默认过滤核糖体、线粒体和 MALAT1
#' @param logfc_threshold numeric, FindMarkers 的 logfc.threshold,
#'   默认 0 (保留所有基因用于 GSEA 排名)
#' @param min_pct numeric, FindMarkers 的 min.pct,
#'   默认 0.01 (GSEA 需要尽量多的基因参与排名，但太低会引入纯噪音)
#' @param test_use character, FindMarkers 的 test.use 方法,
#'   默认 "MAST" (支持校正混杂变量)。
#'   其他选项: "wilcox" (快速无校正), "LR" (慢但可校正)
#' @param latent_vars character vector, 需要校正的混杂变量列名,
#'   如 c("percent_mt", "S.Score", "G2M.Score")。
#'   仅当 test_use = "MAST" 或 "LR" 时有效。默认 NULL (不校正)
#' @param ranking_metric character, 用于排序的指标, 默认 "avg_log2FC"
#' @param verbose logical, 是否显示 FindMarkers 详细输出, 默认 FALSE
#'
#' @return list 包含:
#'   \item{deg_lists_x}{named list, 每个治疗组的 x 方向 ranked vector}
#'   \item{deg_lists_y}{named list, 每个治疗组的 y 方向 ranked vector}
#'   \item{treatments}{named list, 已组装好直接传给 run_all_treatments 的格式}
#'   \item{raw_degs}{list, 原始 FindMarkers 的 data.frame 结果}
#'
compute_deg_ranked_lists <- function(seurat_obj,
                                     treatment_col,
                                     treatment_levels,
                                     reference_group,
                                     response_col,
                                     response_pos = "CR",
                                     response_neg = "NCR",
                                     filter_pattern = "^RPS|^RPL|^MT-|^MALAT1$",
                                     logfc_threshold = 0,
                                     min_pct = 0.01,
                                     test_use = "MAST",
                                     latent_vars = NULL,
                                     max_cells_per_ident = Inf,
                                     ranking_metric = "avg_log2FC",
                                     verbose = FALSE) {
    stopifnot(treatment_col %in% colnames(seurat_obj@meta.data))
    stopifnot(response_col %in% colnames(seurat_obj@meta.data))

    # 校验 latent_vars 是否存在于 metadata 中
    if (!is.null(latent_vars)) {
        missing_vars <- setdiff(latent_vars, colnames(seurat_obj@meta.data))
        if (length(missing_vars) > 0) {
            stop(sprintf(
                "latent_vars 中以下变量不在 metadata 中: %s",
                paste(missing_vars, collapse = ", ")
            ))
        }
        message(sprintf(
            "使用 %s 方法, 校正变量: %s",
            test_use, paste(latent_vars, collapse = ", ")
        ))
    } else {
        message(sprintf("使用 %s 方法 (无 latent vars)", test_use))
    }

    # 内部辅助：DEG data.frame → ranked named vector
    .deg_to_ranked <- function(deg_df, metric = ranking_metric) {
        if (nchar(filter_pattern) > 0) {
            deg_df <- deg_df %>%
                dplyr::filter(!grepl(filter_pattern, rownames(.)))
        }
        deg_df %>%
            tibble::rownames_to_column(var = "symbols") %>%
            dplyr::arrange(dplyr::desc(.data[[metric]])) %>%
            dplyr::select(symbols, dplyr::all_of(metric)) %>%
            tibble::deframe()
    }

    # 构建 FindMarkers 的公共参数
    .fm_args_base <- list(
        logfc.threshold = logfc_threshold,
        min.pct         = min_pct,
        only.pos        = FALSE,
        verbose         = verbose,
        test.use        = test_use
    )
    if (!is.null(latent_vars)) {
        .fm_args_base$latent.vars <- latent_vars
    }
    if (is.finite(max_cells_per_ident)) {
        .fm_args_base$max.cells.per.ident <- max_cells_per_ident
        message(sprintf("每组最多抽样 %d 个细胞", max_cells_per_ident))
    }

    deg_lists_x <- list()
    deg_lists_y <- list()
    raw_degs <- list()

    for (tx in treatment_levels) {
        message(sprintf("\n--- 治疗组: %s ---", tx))

        # ---- Y 轴: 治疗组 vs 参照组 ----
        message(sprintf("  Y 轴: %s vs %s", tx, reference_group))
        Seurat::Idents(seurat_obj) <- seurat_obj@meta.data[[treatment_col]]

        y_args <- c(list(
            object  = seurat_obj,
            ident.1 = tx,
            ident.2 = reference_group
        ), .fm_args_base)

        y_deg <- do.call(Seurat::FindMarkers, y_args)
        raw_degs[[paste0(tx, "_y")]] <- y_deg
        deg_lists_y[[tx]] <- .deg_to_ranked(y_deg)
        message(sprintf("    -> %d genes ranked", length(deg_lists_y[[tx]])))

        # ---- X 轴: 应答 vs 非应答 (在该治疗组内) ----
        message(sprintf("  X 轴: %s vs %s (within %s)", response_pos, response_neg, tx))
        Seurat::Idents(seurat_obj) <- seurat_obj@meta.data[[treatment_col]]

        x_args <- c(list(
            object       = seurat_obj,
            ident.1      = response_pos,
            ident.2      = response_neg,
            group.by     = response_col,
            subset.ident = tx
        ), .fm_args_base)

        x_deg <- tryCatch(
            {
                do.call(Seurat::FindMarkers, x_args)
            },
            error = function(e) {
                warning(sprintf("  X 轴 DEG 计算失败 (%s): %s", tx, e$message))
                return(NULL)
            }
        )

        if (!is.null(x_deg)) {
            raw_degs[[paste0(tx, "_x")]] <- x_deg
            deg_lists_x[[tx]] <- .deg_to_ranked(x_deg)
            message(sprintf("    -> %d genes ranked", length(deg_lists_x[[tx]])))
        }
    }

    # 组装 treatments 格式 (直接传给 run_all_treatments)
    treatments <- list()
    for (tx in names(deg_lists_x)) {
        if (tx %in% names(deg_lists_y)) {
            treatments[[tx]] <- list(x = deg_lists_x[[tx]], y = deg_lists_y[[tx]])
        }
    }

    message(sprintf("\nDEG 计算完成: %d 个治疗组", length(treatments)))

    return(list(
        deg_lists_x = deg_lists_x,
        deg_lists_y = deg_lists_y,
        treatments  = treatments,
        raw_degs    = raw_degs
    ))
}


# ----------------------------------------------------------
# 0b) prepare_cluster_gene_sets
#     从 FindAllMarkers 结果或自定义 marker 提取基因集
# ----------------------------------------------------------

#' @title 从 marker 结果准备 cluster 级基因集
#'
#' @param markers data.frame, FindAllMarkers 的输出 (含 gene, cluster,
#'   avg_log2FC 列), 或者一个已经是 named list 的基因集
#' @param top_n integer, 每个 cluster 取 top N 基因, 默认 25
#' @param ranking_col character, 用于排名的列, 默认 "avg_log2FC"
#' @param filter_pattern character, 过滤基因的正则, 默认过滤核糖体/线粒体
#' @param blacklist_genes character vector, 额外需要排除的基因名
#'   (如即早基因 IEGs / 解离应激基因)。默认 NULL (不额外排除)
#'
#' @return named list, 每个 cluster 一个 character vector (基因名)
#'
prepare_cluster_gene_sets <- function(markers,
                                      top_n = 25,
                                      ranking_col = "avg_log2FC",
                                      filter_pattern = "^RPS|^RPL|^MT-|^MALAT1$",
                                      blacklist_genes = NULL) {
    # 如果已经是 named list，直接返回
    if (is.list(markers) && !is.data.frame(markers)) {
        message(sprintf("输入已是 list 格式，共 %d 个 cluster", length(markers)))
        return(markers)
    }

    # 从 data.frame 提取
    stopifnot(all(c("gene", "cluster") %in% colnames(markers)))
    stopifnot(ranking_col %in% colnames(markers))

    if (nchar(filter_pattern) > 0) {
        n_before <- nrow(markers)
        markers <- markers %>%
            dplyr::filter(!grepl(filter_pattern, gene))
        message(sprintf(
            "过滤 %d 个基因 (pattern: %s)",
            n_before - nrow(markers), filter_pattern
        ))
    }

    if (!is.null(blacklist_genes) && length(blacklist_genes) > 0) {
        n_before <- nrow(markers)
        markers <- markers %>%
            dplyr::filter(!gene %in% blacklist_genes)
        message(sprintf(
            "过滤 %d 个 blacklist 基因 (%s)",
            n_before - nrow(markers),
            paste(blacklist_genes, collapse = ", ")
        ))
    }

    gene_sets <- markers %>%
        dplyr::group_by(cluster) %>%
        dplyr::top_n(n = top_n, wt = .data[[ranking_col]]) %>%
        dplyr::ungroup()

    result <- split(gene_sets$gene, gene_sets$cluster)

    message(sprintf(
        "生成 %d 个基因集, 每个 top %d genes (实际: %s)",
        length(result), top_n,
        paste(range(sapply(result, length)), collapse = "~")
    ))

    return(result)
}


# ----------------------------------------------------------
# 0c) build_gsea_pipeline
#     一键完整流水线: DEG -> ranked list -> GSEA -> 四象限图
# ----------------------------------------------------------

#' @title 一键运行完整 GSEA 四象限流水线
#'
#' @description
#' 封装了从 Seurat 对象出发的整个流程：
#'   1. 计算 DEG ranked lists (compute_deg_ranked_lists)
#'   2. 准备 cluster gene sets (prepare_cluster_gene_sets)
#'   3. 运行 GSEA + 绘图 (run_all_treatments)
#'
#' @section 基因过滤说明 (Gene Filtering):
#' 在 Step 2 准备 cluster 基因集时，支持两层过滤：
#' \itemize{
#'   \item \code{filter_pattern}: 正则表达式过滤，默认去除核糖体蛋白
#'     (RPS/RPL)、线粒体基因 (MT-) 和 MALAT1。
#'   \item \code{blacklist_genes}: 用户自定义的基因黑名单 (character vector)，
#'     用于排除即早基因 (IEGs) 和解离应激基因等不希望参与排名的基因。
#' }
#' 两层过滤均在 \code{top_n} 选取之前执行，确保这些基因不会占用 top marker 名额。
#'
#' 常用 blacklist 示例：
#' \preformatted{
#' blacklist_genes <- c("JUN", "JUNB", "JUND", "FOS", "FOSB", "FOSL1", "FOSL2",
#'                      "EGR1", "NR4A1", "DUSP1", "HSPA1A", "HSPA1B", "HSP90AA1")
#' }
#'
#' @param seurat_obj Seurat 对象
#' @param markers FindAllMarkers 结果 (data.frame) 或已有的 named list 基因集
#' @param treatment_col character, metadata 中治疗方式列名
#' @param treatment_levels character vector, 要分析的治疗组
#' @param reference_group character, 参照组名称 (如 "Tumor")
#' @param response_col character, metadata 中应答列名
#' @param response_pos character, 应答阳性组 (如 "CR")
#' @param response_neg character, 应答阴性组 (如 "NCR")
#' @param top_n integer, 每个 cluster 取 top N marker 基因, 默认 25
#' @param filter_pattern character, 过滤基因正则
#' @param blacklist_genes character vector, 额外需要排除的基因名
#'   (如即早基因 IEGs / 解离应激基因)。默认 NULL (不额外排除)
#' @param test_use character, 差异分析方法, 默认 "MAST"
#' @param latent_vars character vector, 校正的混杂变量, 默认 NULL
#' @param min_pct numeric, 最低表达比例, 默认 0.01
#' @param x_label character, x 轴标签
#' @param y_label_pattern character, y 轴标签模式 (含 {name})
#' @param save_prefix_pattern character, 文件名模式
#' @param padj_cutoff numeric, 显著性阈值
#'
#' @return list 包含:
#'   \item{deg_data}{compute_deg_ranked_lists 的结果}
#'   \item{gene_sets}{cluster 基因集}
#'   \item{gsea_results}{run_all_treatments 的结果}
#'
build_gsea_pipeline <- function(seurat_obj,
                                markers,
                                treatment_col,
                                treatment_levels,
                                reference_group,
                                response_col,
                                response_pos = "CR",
                                response_neg = "NCR",
                                top_n = 25,
                                filter_pattern = "^RPS|^RPL|^MT-|^MALAT1$",
                                blacklist_genes = NULL,
                                test_use = "MAST",
                                latent_vars = NULL,
                                min_pct = 0.01,
                                max_cells_per_ident = Inf,
                                x_label = "NES (CR/NCR)",
                                y_label_pattern = "NES ({name})",
                                save_prefix_pattern = "NES_scatter_{name}",
                                padj_cutoff = 0.05) {
    message("====== Step 1/3: 计算 DEG ranked lists ======\n")
    deg_data <- compute_deg_ranked_lists(
        seurat_obj = seurat_obj,
        treatment_col = treatment_col,
        treatment_levels = treatment_levels,
        reference_group = reference_group,
        response_col = response_col,
        response_pos = response_pos,
        response_neg = response_neg,
        filter_pattern = filter_pattern,
        test_use = test_use,
        latent_vars = latent_vars,
        min_pct = min_pct,
        max_cells_per_ident = max_cells_per_ident
    )

    message("\n====== Step 2/3: 准备 cluster 基因集 ======\n")
    gene_sets <- prepare_cluster_gene_sets(
        markers         = markers,
        top_n           = top_n,
        filter_pattern  = filter_pattern,
        blacklist_genes = blacklist_genes
    )

    message("\n====== Step 3/3: 运行 GSEA + 绘图 ======\n")
    gsea_results <- run_all_treatments(
        cluster_gene_sets   = gene_sets,
        treatments          = deg_data$treatments,
        x_label             = x_label,
        y_label_pattern     = y_label_pattern,
        save_prefix_pattern = save_prefix_pattern,
        padj_cutoff         = padj_cutoff
    )

    message("\n全流程完成！")

    return(list(
        deg_data     = deg_data,
        gene_sets    = gene_sets,
        gsea_results = gsea_results
    ))
}


# ===========================================================
#  核心 GSEA 分析函数
# ===========================================================

# ----------------------------------------------------------
# 1) run_cluster_gsea
#    对两个方向（x / y）分别跑 fgsea，返回整理后
#    的结果和可供绑图的 data.frame
# ----------------------------------------------------------

#' @title 对指定比较运行 cluster-level GSEA
#'
#' @param cluster_gene_sets named list，每个元素是一个 cluster 的
#'   marker gene character vector。names 会被用作 "pathway" 名称。
#'   如果 names 还没有 "Cluster_" 前缀，函数会自动添加。
#' @param stats_x named numeric vector —— x 轴方向的 ranked gene list
#'   （例如 CR vs NCR 的 DEG 统计量）。
#' @param stats_y named numeric vector —— y 轴方向的 ranked gene list
#'   （例如 治疗 vs 癌 的 DEG 统计量）。
#' @param label_x character, x 轴含义标签, 用于输出信息 (默认 "x")
#' @param label_y character, y 轴含义标签, 用于输出信息 (默认 "y")
#' @param nPerm integer, fgsea 置换次数, 默认 10000
#' @param eps numeric, fgsea eps 参数, 默认 0 (精确计算)
#' @param add_cluster_prefix logical, 是否自动给 names 添加 "Cluster_"
#'   前缀, 默认 TRUE
#'
#' @return 一个 list，包含:
#'   \item{tidy_x}{tibble, x 方向 GSEA 结果, 含 types 列}
#'   \item{tidy_y}{tibble, y 方向 GSEA 结果, 含 types 列}
#'   \item{df_plot}{tibble, 合并后用于四象限绑图的 data.frame,
#'     含 nesx, nesy, pvalx, pvaly, padjx, padjy}
#'
run_cluster_gsea <- function(cluster_gene_sets,
                             stats_x,
                             stats_y,
                             label_x = "x",
                             label_y = "y",
                             nPerm = 10000,
                             eps = 0,
                             add_cluster_prefix = TRUE) {
    # -------- 参数检查 --------
    if (!requireNamespace("fgsea", quietly = TRUE)) {
        stop("请先安装 fgsea 包: BiocManager::install('fgsea')")
    }
    if (!is.list(cluster_gene_sets)) {
        stop("cluster_gene_sets 必须是 named list")
    }
    if (is.null(names(cluster_gene_sets))) {
        stop("cluster_gene_sets 必须有 names（cluster 编号/名称）")
    }
    stopifnot(is.numeric(stats_x), is.numeric(stats_y))
    stopifnot(!is.null(names(stats_x)), !is.null(names(stats_y)))

    # -------- 自动添加前缀 --------
    if (add_cluster_prefix) {
        already_prefixed <- grepl("^Cluster_", names(cluster_gene_sets))
        names(cluster_gene_sets)[!already_prefixed] <-
            paste0("Cluster_", names(cluster_gene_sets)[!already_prefixed])
    }

    # -------- 运行 fgsea --------
    message(sprintf(
        "Running fgsea [%s] ... (%d gene sets, %d genes in ranked list)",
        label_x, length(cluster_gene_sets), length(stats_x)
    ))
    res_x <- fgsea::fgsea(
        pathways    = cluster_gene_sets,
        stats       = stats_x,
        nPermSimple = nPerm,
        eps         = eps
    )

    message(sprintf(
        "Running fgsea [%s] ... (%d gene sets, %d genes in ranked list)",
        label_y, length(cluster_gene_sets), length(stats_y)
    ))
    res_y <- fgsea::fgsea(
        pathways    = cluster_gene_sets,
        stats       = stats_y,
        nPermSimple = nPerm,
        eps         = eps
    )

    # -------- 整理结果 --------
    tidy_x <- res_x %>%
        tibble::as_tibble() %>%
        dplyr::mutate(types = ifelse(NES < 0, "down", "up")) %>%
        dplyr::arrange(dplyr::desc(NES))

    tidy_y <- res_y %>%
        tibble::as_tibble() %>%
        dplyr::mutate(types = ifelse(NES < 0, "down", "up")) %>%
        dplyr::arrange(dplyr::desc(NES))

    # -------- 合并用于绑图 --------
    x_for_merge <- tidy_x %>%
        dplyr::arrange(pathway) %>%
        dplyr::select(pathway, NES, pval, padj) %>%
        dplyr::rename(nesx = NES, pvalx = pval, padjx = padj)

    y_for_merge <- tidy_y %>%
        dplyr::arrange(pathway) %>%
        dplyr::select(pathway, NES, pval, padj) %>%
        dplyr::rename(nesy = NES, pvaly = pval, padjy = padj)

    df_plot <- dplyr::left_join(x_for_merge, y_for_merge, by = "pathway")

    message(sprintf(
        "完成！共 %d 个 cluster，其中 padj<0.05 双显著: %d 个",
        nrow(df_plot),
        sum(df_plot$padjx < 0.05 & df_plot$padjy < 0.05, na.rm = TRUE)
    ))

    return(list(
        tidy_x  = tidy_x,
        tidy_y  = tidy_y,
        df_plot = df_plot
    ))
}


# ----------------------------------------------------------
# 2) plot_gsea_quadrant
#    绘制 NES 四象限散点图（已美化版本）
# ----------------------------------------------------------

#' @title 绘制 GSEA NES 四象限散点图
#'
#' @param df_plot data.frame/tibble，至少包含列:
#'   pathway, nesx, nesy, padjx, padjy
#' @param x_label character, x 轴标签
#' @param y_label character, y 轴标签
#' @param padj_cutoff numeric, 双方向显著性阈值, 默认 0.05
#' @param title character, 主标题, 默认 NULL (自动生成)
#' @param subtitle character, 副标题, 默认 NULL (自动生成)
#' @param save_prefix character, 保存文件前缀; NULL 则不保存
#' @param save_width numeric, 图片宽度 (inches), 默认 10
#' @param save_height numeric, 图片高度 (inches), 默认 7
#' @param label_all logical, TRUE = 标注所有点; FALSE = 只标注显著点
#'   默认 TRUE
#' @param point_size_range numeric vector of length 2, 点大小范围,
#'   默认 c(2.5, 8)
#' @param max_overlaps integer, geom_text_repel 允许的最大重叠数, 默认 20
#'
#' @return ggplot2 对象 (同时也会 print 出来)
#'
plot_gsea_quadrant <- function(df_plot,
                               x_label = "NES (x)",
                               y_label = "NES (y)",
                               padj_cutoff = 0.05,
                               title = NULL,
                               subtitle = NULL,
                               save_prefix = NULL,
                               save_width = 10,
                               save_height = 7,
                               label_all = TRUE,
                               point_size_range = c(2.5, 8),
                               max_overlaps = 20) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("请先安装 ggplot2")
    }
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
        stop("请先安装 ggrepel")
    }

    # -------- 象限分类 --------
    df_plot <- df_plot %>%
        dplyr::mutate(
            sig = (padjx < padj_cutoff & padjy < padj_cutoff),
            quadrant = dplyr::case_when(
                !sig ~ "NS",
                nesx > 0 & nesy > 0 ~ "Up / Up",
                nesx > 0 & nesy < 0 ~ "Up / Down",
                nesx < 0 & nesy > 0 ~ "Down / Up",
                nesx < 0 & nesy < 0 ~ "Down / Down",
                TRUE ~ "NS"
            ),
            neg_log_padj = (-log10(padjx) + -log10(padjy)) / 2
        )

    # -------- 配色 --------
    quad_colors <- c(
        "Up / Up"     = "#E74C3C",
        "Up / Down"   = "#3498DB",
        "Down / Up"   = "#F39C12",
        "Down / Down" = "#2ECC71",
        "NS"          = "grey75"
    )

    if (is.null(title)) {
        title <- "GSEA Normalized Enrichment Scores"
    }
    if (is.null(subtitle)) {
        subtitle <- sprintf(
            "Colored points: padj < %g in both comparisons",
            padj_cutoff
        )
    }

    # -------- 标签数据 --------
    if (label_all) {
        label_data <- df_plot
    } else {
        label_data <- dplyr::filter(df_plot, sig)
    }

    # -------- 绑图 --------
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = nesx, y = nesy)) +

        # 参考线
        ggplot2::geom_hline(
            yintercept = 0, linetype = "dashed",
            color = "grey50", linewidth = 0.4
        ) +
        ggplot2::geom_vline(
            xintercept = 0, linetype = "dashed",
            color = "grey50", linewidth = 0.4
        ) +

        # 散点
        ggplot2::geom_point(
            ggplot2::aes(fill = quadrant, size = neg_log_padj),
            shape = 21, color = "white", stroke = 0.6
        ) +
        ggplot2::scale_fill_manual(values = quad_colors, name = "Quadrant") +
        ggplot2::scale_size_continuous(
            name   = "Mean\n-log10(padj)",
            range  = point_size_range,
            breaks = pretty
        ) +

        # 标签
        ggrepel::geom_text_repel(
            data = label_data,
            ggplot2::aes(label = pathway, color = quadrant),
            size = 3.5,
            fontface = "bold",
            max.overlaps = max_overlaps,
            box.padding = 0.6,
            point.padding = 0.4,
            segment.color = "grey55",
            segment.size = 0.3,
            show.legend = FALSE,
            seed = 42
        ) +
        ggplot2::scale_color_manual(values = quad_colors) +

        # 标题
        ggplot2::labs(
            x        = x_label,
            y        = y_label,
            title    = title,
            subtitle = subtitle
        ) +

        # 主题
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
            panel.border = ggplot2::element_rect(
                color = "black", fill = NA,
                linewidth = 0.6
            ),
            plot.title = ggplot2::element_text(
                face = "bold", size = 15,
                hjust = 0
            ),
            plot.subtitle = ggplot2::element_text(
                color = "grey40", size = 11,
                hjust = 0,
                margin = ggplot2::margin(b = 12)
            ),
            axis.title = ggplot2::element_text(face = "bold", size = 12),
            axis.text = ggplot2::element_text(color = "grey30"),
            axis.line = ggplot2::element_line(color = "grey70", linewidth = 0.4),
            panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.3),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "right",
            legend.title = ggplot2::element_text(face = "bold", size = 10),
            legend.text = ggplot2::element_text(size = 9),
            plot.margin = ggplot2::margin(15, 15, 15, 15)
        ) +

        # 坐标轴对称
        ggplot2::coord_cartesian(
            xlim = c(
                -max(abs(df_plot$nesx), na.rm = TRUE) - 0.5,
                max(abs(df_plot$nesx), na.rm = TRUE) + 0.5
            ),
            ylim = c(
                -max(abs(df_plot$nesy), na.rm = TRUE) - 0.5,
                max(abs(df_plot$nesy), na.rm = TRUE) + 0.5
            )
        ) +

        # 图例调整
        ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(size = 4)),
            size = ggplot2::guide_legend(override.aes = list(fill = "grey50"))
        )

    # -------- 保存 --------
    if (!is.null(save_prefix)) {
        ggplot2::ggsave(paste0(save_prefix, ".pdf"), p,
            width = save_width, height = save_height, dpi = 300
        )
        ggplot2::ggsave(paste0(save_prefix, ".png"), p,
            width = save_width, height = save_height, dpi = 300
        )
        message(sprintf("图片已保存: %s.pdf / .png", save_prefix))
    }

    print(p)
    invisible(p)
}


# ----------------------------------------------------------
# 3) extract_leading_edge_overlap
#    批量提取指定 cluster 的 leading edge 交集
# ----------------------------------------------------------

#' @title 批量提取 leading edge 基因交集
#'
#' @param tidy_x tibble, x 方向 fgsea 结果 (含 leadingEdge 列)
#' @param tidy_y tibble, y 方向 fgsea 结果 (含 leadingEdge 列)
#' @param clusters character vector, 要提取的 cluster pathway 名称。
#'   如果为 NULL，则自动选取双方向 padj < padj_cutoff 的 cluster。
#' @param padj_cutoff numeric, 当 clusters = NULL 时用于自动筛选, 默认 0.05
#' @param print_results logical, 是否打印交集结果, 默认 TRUE
#'
#' @return named list，每个元素包含:
#'   \item{leading_x}{x 方向 leading edge genes}
#'   \item{leading_y}{y 方向 leading edge genes}
#'   \item{overlap}{交集基因}
#'   \item{n_overlap}{交集基因数}
#'
extract_leading_edge_overlap <- function(tidy_x,
                                         tidy_y,
                                         clusters = NULL,
                                         padj_cutoff = 0.05,
                                         print_results = TRUE) {
    # 自动选取双显著 cluster
    if (is.null(clusters)) {
        sig_x <- tidy_x %>%
            dplyr::filter(padj < padj_cutoff) %>%
            dplyr::pull(pathway)
        sig_y <- tidy_y %>%
            dplyr::filter(padj < padj_cutoff) %>%
            dplyr::pull(pathway)
        clusters <- intersect(sig_x, sig_y)
        message(sprintf(
            "自动选取双显著 cluster (padj < %g): %d 个",
            padj_cutoff, length(clusters)
        ))
    }

    if (length(clusters) == 0) {
        message("没有符合条件的 cluster")
        return(list())
    }

    results <- list()
    for (cl in clusters) {
        lx <- tidy_x %>%
            dplyr::filter(pathway == cl) %>%
            dplyr::pull(leadingEdge) %>%
            unlist()

        ly <- tidy_y %>%
            dplyr::filter(pathway == cl) %>%
            dplyr::pull(leadingEdge) %>%
            unlist()

        ovlp <- intersect(lx, ly)

        results[[cl]] <- list(
            leading_x = lx,
            leading_y = ly,
            overlap   = ovlp,
            n_overlap = length(ovlp)
        )

        if (print_results) {
            message(sprintf("\n--- %s ---", cl))
            message(sprintf("  Leading edge x: %d genes", length(lx)))
            message(sprintf("  Leading edge y: %d genes", length(ly)))
            message(sprintf(
                "  Overlap: %d genes -> %s",
                length(ovlp),
                paste(ovlp, collapse = ", ")
            ))
        }
    }

    invisible(results)
}


# ----------------------------------------------------------
# 4) run_all_treatments
#    便捷函数：批量运行多个治疗组并生成所有图
# ----------------------------------------------------------

#' @title 批量运行多个治疗组的 GSEA 四象限分析
#'
#' @param cluster_gene_sets named list, 同 run_cluster_gsea
#' @param treatments named list, 每个元素是一个 list(x = ..., y = ...),
#'   其中 x / y 分别是两个方向的 ranked gene list (named numeric vector)。
#'   names(treatments) 就是治疗组名称。
#'   例如: list(nCIT = list(x = deg_x, y = deg_y), IMT = list(...))
#' @param x_label character, 所有图的 x 轴标签, 默认 "NES (CR/NCR)"
#' @param y_label_pattern character, y 轴标签模式, 其中 {name} 会被替换
#'   为治疗组名称。默认 "NES ({name})"
#' @param save_prefix_pattern character, 保存文件名模式, 默认
#'   "NES_scatter_{name}"；NULL 则不保存
#' @param padj_cutoff numeric, 默认 0.05
#' @param ... 传递给 plot_gsea_quadrant 的其他参数
#'
#' @return named list, 每个治疗组一个元素, 含 gsea_result + plot + overlaps
#'
run_all_treatments <- function(cluster_gene_sets,
                               treatments,
                               x_label = "NES (CR/NCR)",
                               y_label_pattern = "NES ({name})",
                               save_prefix_pattern = "NES_scatter_{name}",
                               padj_cutoff = 0.05,
                               ...) {
    stopifnot(is.list(treatments), !is.null(names(treatments)))

    all_results <- list()

    for (tx_name in names(treatments)) {
        message(sprintf("\n========== 治疗组: %s ==========\n", tx_name))

        tx <- treatments[[tx_name]]
        stopifnot(is.list(tx), all(c("x", "y") %in% names(tx)))

        # 运行 GSEA
        gsea_res <- run_cluster_gsea(
            cluster_gene_sets = cluster_gene_sets,
            stats_x           = tx$x,
            stats_y           = tx$y,
            label_x           = paste0(tx_name, " (x: CR/NCR)"),
            label_y           = paste0(tx_name, " (y: vs Cancer)")
        )

        # 生成 y 轴标签
        y_lab <- gsub("\\{name\\}", tx_name, y_label_pattern)

        # 生成保存前缀
        save_pf <- NULL
        if (!is.null(save_prefix_pattern)) {
            save_pf <- gsub("\\{name\\}", tx_name, save_prefix_pattern)
        }

        # 绑图
        p <- plot_gsea_quadrant(
            df_plot     = gsea_res$df_plot,
            x_label     = x_label,
            y_label     = y_lab,
            padj_cutoff = padj_cutoff,
            title       = paste("GSEA Quadrant -", tx_name),
            save_prefix = save_pf,
            ...
        )

        # Leading edge 交集
        overlaps <- extract_leading_edge_overlap(
            tidy_x      = gsea_res$tidy_x,
            tidy_y      = gsea_res$tidy_y,
            padj_cutoff = padj_cutoff
        )

        all_results[[tx_name]] <- list(
            gsea     = gsea_res,
            plot     = p,
            overlaps = overlaps
        )
    }

    message(sprintf("\n全部完成！共处理 %d 个治疗组", length(treatments)))
    invisible(all_results)
}
