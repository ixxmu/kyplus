# ============================================================
# sc_gsea_quadrant_example.R
# 使用示例 —— 展示如何用 sc_gsea_quadrant.R 中的函数
# ============================================================

source("R/sc_gsea_quadrant.R") # 根据你的路径调整

# ============================================================
# 方式一：一键全自动 + MAST 校正（推荐）
# ============================================================
# 前提：你已经有 NKT_cells Seurat 对象和 subTmks_MAST (FindAllMarkers 结果)
# 需要的 metadata 列：Treatshort (治疗方式), Pathshort (CR/NCR/Tumor)

all <- build_gsea_pipeline(
    seurat_obj = NKT_cells,
    markers = subTmks_MAST, # FindAllMarkers 的 data.frame
    treatment_col = "Treatshort", # 存治疗方式的列
    treatment_levels = c("nCIT", "nCRT", "nCRIT"), # 要分析哪些治疗组
    reference_group = "Tumor", # Y 轴参照组
    response_col = "Pathshort", # 存 CR/NCR 的列
    response_pos = "CR",
    response_neg = "NCR",
    top_n = 25, # 每 cluster 取 top 25 marker 基因
    # --- MAST 校正参数 ---
    test_use = "MAST", # 使用 MAST 方法
    latent_vars = c(
        "Dissociation_Stress_Score1", "percent_mt",
        "S.Score", "G2M.Score"
    ), # 校正这些混杂变量
    min_pct = 0.01 # GSEA 需要全基因排名, 阈值设低
)

# 访问结果
all$gsea_results[["nCIT"]]$gsea$df_plot # nCIT 的四象限数据
all$gsea_results[["nCIT"]]$overlaps # nCIT 的 leading edge 交集
all$deg_data$raw_degs[["nCIT_y"]] # nCIT 原始 DEG data.frame


# ============================================================
# 方式二：分步执行（更灵活）
# ============================================================

# --- Step 1: 准备 cluster 基因集 ---
gene_sets <- prepare_cluster_gene_sets(subTmks_MAST, top_n = 25)
# 如果已经有 named list (如 top5_clean_3rd), 直接传入:
# gene_sets <- prepare_cluster_gene_sets(top5_clean_3rd)

# --- Step 2: 批量计算 DEG ranked lists (用 MAST 校正) ---
deg_data <- compute_deg_ranked_lists(
    seurat_obj = NKT_cells,
    treatment_col = "Treatshort",
    treatment_levels = c("nCIT", "nCRT", "nCRIT"),
    reference_group = "Tumor",
    response_col = "Pathshort",
    # MAST 配置
    test_use = "MAST",
    latent_vars = c(
        "Dissociation_Stress_Score1", "percent_mt",
        "S.Score", "G2M.Score"
    ),
    min_pct = 0.01
)

# --- Step 3: 批量 GSEA + 绘图 ---
results <- run_all_treatments(
    cluster_gene_sets = gene_sets,
    treatments        = deg_data$treatments
)

# --- Step 4 (可选): 手动跑单个治疗组 ---
res_nCIT <- run_cluster_gsea(
    cluster_gene_sets = gene_sets,
    stats_x = deg_data$deg_lists_x[["nCIT"]],
    stats_y = deg_data$deg_lists_y[["nCIT"]],
    label_x = "CR/NCR",
    label_y = "nCIT vs Tumor"
)

plot_gsea_quadrant(
    df_plot     = res_nCIT$df_plot,
    x_label     = "NES (CR/NCR)",
    y_label     = "NES (nCIT)",
    save_prefix = "NES_scatter_nCIT"
)

overlaps <- extract_leading_edge_overlap(res_nCIT$tidy_x, res_nCIT$tidy_y)


# ============================================================
# 方式三：兼容已有变量 (不使用 MAST，用默认 wilcox)
# ============================================================

# 如果你已经手动跑过 FindMarkers 并存好了 deg_lists_x, deg_lists_y
treatments <- list(
    nCIT  = list(x = deg_lists_x[["nCIT_x_DEGs"]], y = deg_lists_y[["nCIT_y_DEGs"]]),
    nCRT  = list(x = deg_lists_x[["nCRT_x_DEGs"]], y = deg_lists_y[["nCRT_y_DEGs"]]),
    nCRIT = list(x = deg_lists_x[["nCRIT_x_DEGs"]], y = deg_lists_y[["nCRIT_y_DEGs"]])
)

results <- run_all_treatments(
    cluster_gene_sets = subTmks_tmp, # 或 top5_clean_3rd
    treatments        = treatments
)

# 汇总所有治疗组的双显著 cluster
for (tx in names(results)) {
    sig <- results[[tx]]$gsea$df_plot %>% dplyr::filter(padjx < 0.05, padjy < 0.05)
    cat(sprintf("\n=== %s: %d 个双显著 cluster ===\n", tx, nrow(sig)))
    print(sig)
}
