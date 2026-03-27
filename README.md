# kyplus <img src="https://img.shields.io/badge/version-0.1.2-blue.svg" alt="Version">

> 🚀 Enhanced Project Management and Bioinformatics Analysis Tools for R

一个全功能的R脚手架包，提供项目管理、数据分析、可视化和生物信息学工具。

[![GitHub issues](https://img.shields.io/github/issues/ixxmu/kyplus)](https://github.com/ixxmu/kyplus/issues)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 📚 目录

- [功能概览](#功能概览)
- [安装](#安装)
- [快速开始](#快速开始)
- [核心功能模块](#核心功能模块)
  - [1. 项目管理](#1-项目管理)
  - [2. DCE差异因果效应分析](#2-dce差异因果效应分析)
  - [3. 可视化工具](#3-可视化工具)
  - [4. 并行计算](#4-并行计算)
  - [5. 包管理](#5-包管理)
  - [6. 数据压缩与存储](#6-数据压缩与存储)
  - [7. 通路数据库](#7-通路数据库)
  - [8. AI智能注释](#8-ai智能注释)
  - [9. 数据预处理工具](#9-数据预处理工具)
- [详细文档](#详细文档)
- [示例](#示例)
- [依赖关系](#依赖关系)
- [贡献](#贡献)
- [许可证](#许可证)

---

## 🎯 功能概览

### 脚手架功能
- ✅ **项目管理**：标准化的R项目结构创建和管理
- ✅ **包安装工具**：批量安装和管理R包依赖
- ✅ **并行计算**：mirai并行环境配置和管理
- ✅ **数据存储**：高效的数据压缩和加载工具

### 生物信息学功能
- 🧬 **DCE分析**：完整的差异因果效应（Differential Causal Effects）分析流程
- 🧬 **网络构建**：多种基因调控网络构建方法
- 🧬 **通路富集**：KEGG/GO/Reactome通路富集分析
- 🧬 **KEGG数据**：本地KEGG通路数据库更新和管理

### AI智能工具
- 🤖 **AI细胞注释**：基于ChatGPT的单细胞cluster自动注释
- 🤖 **通用AI接口**：`ai_chat()` 通用API调用层，轻松扩展更多AI功能

### 可视化工具
- 🎨 **配色方案**：Goldfish主题配色，适合出版级图表
- 🎨 **图表导出**：批量导出多种格式的高质量图表
- 🎨 **DCE可视化**：网络图、火山图、热图等专业可视化

---

## 💻 安装

### 从GitHub安装

```r
# 安装devtools（如果还没有）
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}

# 安装kyplus
devtools::install_github("ixxmu/kyplus")
```

### 安装依赖

```r
# 安装Bioconductor依赖（可选，用于DCE分析）
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c(
  "dce", "graph", "minet",
  "clusterProfiler", "enrichplot", "ReactomePA"
))
```

---

## 🚀 快速开始

```r
library(kyplus)

# 1. 创建新项目
create_project_structure(
  path = "~/Projects/my_analysis",
  type = "analysis"
)

# 2. 检查系统性能
check_performance()

# 3. 安装常用R包套件
install_essential_packages()

# 4. 初始化并行环境
init_mirai(n_cores = 4, packages = c("dplyr", "ggplot2"))
```

---

## 📦 核心功能模块

### 1. 项目管理

创建标准化的R项目结构，支持分析项目和R包项目。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `create_project_structure()` | 创建新项目结构 |
| `load_project()` | 加载项目环境 |
| `get_project_config()` | 获取项目配置 |
| `check_performance()` | 检查系统性能 |

#### 使用示例

```r
# 创建分析项目
create_project_structure(
  path = "~/Projects/rnaseq_analysis",
  type = "analysis",
  create_project_file = TRUE
)

# 创建的目录结构：
# rnaseq_analysis/
# ├── R/
# │   ├── functions/
# │   └── pipelines/
# ├── data/
# │   ├── raw/
# │   ├── processed/
# │   └── output/
# ├── docs/
# │   ├── specifications/
# │   └── reports/
# ├── tests/
# │   ├── unit/
# │   └── integration/
# ├── config/
# └── rnaseq_analysis.Rproj

# 加载项目
load_project("~/Projects/rnaseq_analysis")
```

---

### 2. DCE差异因果效应分析

完整的DCE分析工具集，用于识别疾病中的通路失调。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `build_regulatory_network()` | 构建基因调控网络 |
| `split_expression_by_gene()` | 根据基因表达分组样本 |
| `run_dce_analysis()` | 运行完整DCE分析 |
| `batch_dce_analysis()` | 批量分析多个目标基因 |
| `filter_dce_results()` | 过滤和排序DCE结果 |

#### 网络构建方法

- **correlation** - Pearson相关（快速）
- **pcor** - 偏相关（支持岭回归处理共线性）
- **mi** - 互信息（非线性关系）
- **aracne** - ARACNE算法（推荐，去除间接边）
- **random** - 随机网络（测试用）

#### 使用示例

```r
library(dce)
library(kyplus)

# 1. 根据目标基因表达分组样本
expr_data <- your_expression_matrix  # samples × genes
split_data <- split_expression_by_gene(
  expr_matrix = expr_data,
  target_gene = "ISG15",
  quantiles = c(0, 0.4, 0.85, 1),  # Low/Medium/High
  group_labels = c("Low", "Medium", "High")
)

# 2. 运行DCE分析
results <- run_dce_analysis(
  wt_data = split_data$Low,      # 参考组
  mt_data = split_data$High,     # 比较组
  network_method = "aracne",      # 网络构建方法
  threshold = 0.3,                # 边阈值
  max_parents = 3,                # 最大父节点数
  filter_results = TRUE,          # 自动过滤
  pvalue_threshold = 0.05         # 显著性阈值
)

# 3. 查看显著结果
head(results$filtered_results)

# 4. 批量分析多个基因
batch_results <- batch_dce_analysis(
  expr_matrix = expr_data,
  target_genes = c("ISG15", "STAT1", "IRF7"),
  network_method = "aracne",
  parallel = TRUE  # 并行处理
)
```

#### DCE可视化

```r
# 网络图
plot_dce_network(
  results$dce_results,
  highlight_nodes = "ISG15",
  pvalue_threshold = 0.05
)

# 火山图
plot_dce_volcano(
  results$dce_results,
  label_top = 10
)

# 热图
plot_dce_heatmap(
  results$dce_results,
  cluster_rows = TRUE,
  color_scale = "RdBu"
)

# 网络对比
net1 <- build_regulatory_network(split_data$Low, method = "aracne")
net2 <- build_regulatory_network(split_data$High, method = "aracne")
compare_networks(net1, net2, "Low", "High")
```

#### 通路富集分析

```r
# KEGG富集
enrichment <- enrich_dce_results(
  results$dce_results,
  pathway_db = "KEGG",
  organism = "human",
  pvalue_threshold = 0.05
)

# 可视化富集结果
plot_enrichment(enrichment, plot_type = "dot", top_n = 20)
plot_enrichment(enrichment, plot_type = "bar")
plot_enrichment(enrichment, plot_type = "network")

# GO富集
go_enrichment <- enrich_dce_results(
  results$dce_results,
  pathway_db = "GO_BP",  # Biological Process
  organism = "human"
)
```

---

### 3. 可视化工具

专业的数据可视化和图表导出工具。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `goldfish_colors()` | Goldfish配色方案 |
| `scale_color_goldfish()` | ggplot2离散配色 |
| `scale_color_goldfish_c()` | ggplot2连续配色 |
| `scale_color_goldfish_b()` | ggplot2分箱配色 |
| `show_goldfish_palettes()` | 显示所有配色方案 |
| `plot_export()` | 导出图表 |
| `batch_export()` | 批量导出图表 |
| `save_any_plot()` | 通用保存（支持pheatmap/ComplexHeatmap/ggplot） |
| `sc_bindprop()` | 计算细胞类型比例 |
| `sc_bindplot()` | 细胞比例分组对比箱线图 |

#### Goldfish配色主题

内置12种精心设计的配色方案，适合科研出版：

- **default** - 默认配色（8色）
- **warm** - 暖色调（8色）
- **cool** - 冷色调（8色）
- **contrast** - 高对比度（8色）
- **pastel** - 柔和色调（8色）
- **vibrant** - 鲜艳色调（8色）
- **earth** - 大地色系（8色）
- **ocean** - 海洋色系（8色）
- **sunset** - 日落色系（8色）
- **forest** - 森林色系（8色）
- **autumn** - 秋季色系（8色）
- **spring** - 春季色系（8色）

#### 使用示例

```r
library(ggplot2)

# 查看所有配色方案
show_goldfish_palettes()

# 使用Goldfish配色
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_goldfish(palette = "vibrant") +
  theme_minimal()

# 导出图表（多种格式）
plot_export(
  p,
  filename = "iris_plot",
  path = "figures",
  formats = c("pdf", "png", "svg"),
  width = 8,
  height = 6,
  dpi = 300
)

# 批量导出
plot_list <- list(
  plot1 = p1,
  plot2 = p2,
  plot3 = p3
)

batch_export(
  plot_list,
  path = "figures",
  formats = c("pdf", "png"),
  width = 10,
  height = 8
)
```

#### 通用保存 save_any_plot

支持 pheatmap、ComplexHeatmap、ggplot2、base R 图形，自动检测尺寸：

```r
# pheatmap — 自动从对象计算宽高
library(pheatmap)
p <- pheatmap(mat, cellwidth = 20, cellheight = 20, silent = TRUE)
save_any_plot(p, "heatmap_auto.pdf")

# ComplexHeatmap — 同样自动尺寸
library(ComplexHeatmap)
ht <- Heatmap(mat)
save_any_plot(ht, "complex_heatmap.pdf")

# ggplot2 — 指定尺寸
save_any_plot(g, "scatter.png", width_base = 8, height_base = 6)
```

#### 细胞类型比例分析 sc_bindprop / sc_bindplot

计算每个样本中各细胞类型的比例，并生成分组对比箱线图：

```r
# 一步到位：算比例 + 出图
sc_bindplot(seurat_obj,
  cluster_col = "subtype_anno",
  group_col   = "Treatment",
  sample_col  = "orig.ident",
  palette     = "warm"
)

# 或者分步操作
prop_df <- sc_bindprop(seurat_obj,
  cluster_col = "subtype_anno",
  group_col = "Treatment",
  sample_col = "orig.ident"
)
sc_bindplot(prop_df)  # 直接传比例表

# 筛选细胞类型 + 两两比较
sc_bindplot(seurat_obj,
  cluster_col = "subtype_anno",
  group_col = "Treatment",
  cluster_filter = c("CD8_T", "Treg", "NK"),
  comparisons = list(c("Ctrl", "Treatment"))
)
```

---

### 4. 并行计算

基于mirai的现代并行计算框架。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `init_mirai()` | 初始化mirai并行环境 |
| `cleanup_mirai()` | 清理并行环境 |
| `mirai_status()` | 查看并行状态 |

#### 使用示例

```r
# 初始化并行环境
init_mirai(
  n_cores = 8,
  packages = c("dplyr", "ggplot2", "Seurat"),
  verbose = TRUE
)

# 使用mirai进行并行计算
library(mirai)

# 并行任务示例
results <- lapply(1:100, function(i) {
  mirai({
    # 并行执行的代码
    complex_analysis(data[[i]])
  })
}) %>%
  lapply(function(m) m$data)

# 查看状态
mirai_status()

# 清理环境
cleanup_mirai()
```

---

### 5. 包管理

快速安装和管理R包依赖。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `install_essential_packages()` | 安装必备包 |
| `install_package_suite()` | 安装包套件 |
| `setup_cpu_cores()` | 配置多核安装 |

#### 使用示例

```r
# 安装必备包
install_essential_packages(update = TRUE)

# 安装特定套件
install_package_suite(
  suite = "data_science",  # tidyverse生态系统
  method = "auto"
)

# 配置多核并行安装
setup_cpu_cores(n_cores = 4)

# 自定义包套件
my_packages <- c(
  "Seurat", "monocle3", "scater",
  "DESeq2", "edgeR", "limma"
)

install_package_suite(
  suite = "custom",
  packages = my_packages,
  source = "bioc"
)
```

---

### 6. 数据压缩与存储

高效的数据保存和加载工具。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `save_compressed()` | 压缩保存RDS |
| `save_compressed_qs()` | 使用qs格式保存 |
| `load_compressed_qs()` | 加载qs格式数据 |
| `save_end_day()` | 保存工作区（日期标记） |
| `load_start_day()` | 加载工作区 |

#### 使用示例

```r
# 压缩保存大对象
large_data <- your_large_object
save_compressed(large_data, "data/large_data.rds")

# 使用qs格式（更快）
save_compressed_qs(large_data, "data/large_data.qs")
loaded_data <- load_compressed_qs("data/large_data.qs")

# 自动命名保存工作区
save_end_day()  # 保存为 workspace_YYYYMMDD.qs

# 加载最新工作区
load_start_day()
```

---

### 7. 通路数据库

KEGG通路数据库管理工具。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `update_kegg()` | 更新KEGG数据库 |

#### 使用示例

```r
# 更新人类KEGG数据
update_kegg(
  organism = "hsa",
  method = "gseadbr",  # 使用msigdbr
  output_dir = "data/kegg"
)

# 输出GMT格式
update_kegg(
  organism = "hsa",
  method = "webjson",
  output_format = "gmt",
  output_dir = "data/kegg"
)
```

---

### 8. AI智能注释

基于ChatGPT API的智能细胞类型注释工具，支持OpenAI及所有兼容API。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `ai_setup()` | 设置API密钥和端点 |
| `ai_chat()` | 通用AI对话接口（底层API） |
| `sc_annotate_clusters()` | AI自动注释单细胞cluster |

#### 完整使用示例

```r
library(kyplus)
library(Seurat)

# ============================================
# Step 1: 设置API（只需设置一次）
# ============================================
ai_setup(
  api_key = "sk-your-api-key-here",
  base_url = "https://api.openai.com/v1"  # 或你的代理地址
)

# ============================================
# Step 2: 从Seurat对象中提取marker基因
# ============================================
# 假设你已有一个分析好的Seurat对象 seurat_obj
# 已经做过 FindAllMarkers
all_markers <- FindAllMarkers(seurat_obj, only.pos = TRUE)

# 提取每个cluster的top25 marker基因，构建named list
marker_list <- split(all_markers$gene, all_markers$cluster)
marker_list <- lapply(marker_list, head, 25)

# 重命名为 cluster0, cluster1, ... 格式
names(marker_list) <- paste0("cluster", names(marker_list))

# 查看数据结构
str(marker_list, max.level = 1)
# List of 16
#  $ cluster0 : chr [1:25] "CD3D" "CD8A" "CD8B" "GZMK" ...
#  $ cluster1 : chr [1:25] "CD3D" "CD4" "IL7R" "CCR7" ...
#  $ cluster2 : chr [1:25] "FOXP3" "IL2RA" "CTLA4" ...
#  ...

# ============================================
# Step 3: AI自动注释
# ============================================
anno <- sc_annotate_clusters(
  marker_list = marker_list,
  species = "human",           # "human" 或 "mouse"
  tissue = "PBMC",             # 组织类型，帮助提高准确度
  model = "gpt-4o",            # 模型选择
  temperature = 0.1            # 低温度=更确定性的结果
)

# ============================================
# Step 4: 查看注释结果
# ============================================
print(anno)
#   cluster major_lineage                        subtype_anno
# 1       0         CD8_T            Activated_CD8_Tem_GZMK
# 2       1         CD4_T            Naive_Tcm_CD4_IL7R_CCR7
# 3       2          Treg   Activated_Treg_FOXP3_IL2RA_CTLA4
# 4       3        T_cell    Memory_Trm_like_T_ITGA1_SLAMF6
# ...                                      reason
# 1   高表达GZMK和效应记忆T细胞标志物，提示为活化的CD8效应记忆T细胞
# 2   高表达IL7R和CCR7等初始T细胞标志物，提示为CD4初始/中央记忆T细胞
# 3   经典Treg标志物FOXP3、IL2RA、CTLA4高表达，确认为活化调节性T细胞
# ...

# ============================================
# Step 5: 将注释写回Seurat对象
# ============================================
# 方法1：使用 major_lineage（大类）
seurat_obj$major_lineage <- anno$major_lineage[
  match(seurat_obj$seurat_clusters, anno$cluster)
]

# 方法2：使用 subtype_anno（细分亚型）
seurat_obj$subtype_anno <- anno$subtype_anno[
  match(seurat_obj$seurat_clusters, anno$cluster)
]

# ============================================
# Step 6: 可视化注释结果
# ============================================
# 大类注释
DimPlot(seurat_obj, group.by = "major_lineage") +
  scale_color_goldfish(palette = "vibrant") +
  ggtitle("Major Lineage")

# 细分亚型
DimPlot(seurat_obj, group.by = "subtype_anno") +
  scale_color_goldfish(palette = "default") +
  ggtitle("Subtype Annotation")

# 保存注释结果
write.csv(anno, "cell_type_annotation.csv", row.names = FALSE)
```

#### 通用AI对话示例

`ai_chat()` 可以独立使用，在任何场景下调用AI：

```r
# 直接问AI问题
result <- ai_chat(
  system_prompt = "你是一个生信专家。",
  user_prompt = "请解释什么是pseudotime分析？用中文简要回答。"
)
cat(result$content)

# 获取JSON格式的结构化回答
result <- ai_chat(
  system_prompt = "Return only valid JSON.",
  user_prompt = "List the top 5 marker genes for CD8 T cells as a JSON array.",
  parse_json = TRUE
)
str(result$content)
```

---

### 9. 数据预处理工具

表达矩阵预处理常用函数，支持分位数归一化和自动 log2 转换。

#### 功能函数

| 函数 | 说明 |
|------|------|
| `quantile_normalisation()` | 按列分位数归一化，对齐样本分布 |
| `auto_log2_transform()` | 自动检测并执行 log2 转换 |

#### Quantile Normalisation

对表达矩阵按列进行分位数归一化，将各样本的值分布强制对齐为相同分布：

```r
# 基本用法
mat <- matrix(c(5, 2, 3, 4,
                1, 4, 4, 2,
                3, 6, 6, 1), nrow = 4)
norm_mat <- quantile_normalisation(mat)

# 对 data.frame 同样适用
df <- as.data.frame(mat)
norm_df <- quantile_normalisation(df)
```

#### Auto log2 Transform

自动判断表达矩阵是否需要 log2 转换。通过分位数启发式规则检测数据分布，避免重复转换：

```r
# 自动判断并转换
expr_processed <- auto_log2_transform(exprSet)

# 强制执行 log2 转换（跳过检测）
expr_processed <- auto_log2_transform(exprSet, force_log = TRUE)

# 自定义安全阈值（最小值低于该值则判定为已转换）
expr_processed <- auto_log2_transform(exprSet, min_threshold = -5)
```

判断逻辑说明：
- **执行转换**：99%分位 > 100，或值域 > 50 且 25%分位 > 0，或数据呈线性分布特征
- **跳过转换**：最小值 < `min_threshold`（默认 -10），判定为已转换数据
- **`force_log = TRUE`**：忽略判断逻辑，强制执行转换

---

## 📖 详细文档

### Vignettes

1. **DCE分析教程** (`vignettes/dce-analysis.Rmd`)
   - 完整的DCE分析流程
   - 3个工作流程示例
   - 方法对比和最佳实践
   - 故障排除指南

2. **项目管理** (`vignettes/project-management.Rmd`)
   - 项目结构说明
   - 最佳实践
   - 配置选项

3. **并行计算** (`vignettes/parallel_computing_with_mirai.Rmd`)
   - Mirai使用指南
   - 性能优化技巧

4. **包安装** (`vignettes/package-installation.Rmd`)
   - 包管理策略
   - 依赖解决方案

### 在线文档

访问 [https://ixxmu.github.io/kyplus](https://ixxmu.github.io/kyplus) 查看完整文档。

### 帮助文档

```r
# 查看函数帮助
?build_regulatory_network
?run_dce_analysis
?plot_dce_network

# 查看vignette
browseVignettes("kyplus")
```

---

## 💡 示例

### 示例1：完整的RNA-seq分析项目

```r
# 1. 创建项目
create_project_structure("~/Projects/rnaseq_tcga", type = "analysis")

# 2. 初始化并行环境
init_mirai(n_cores = 8, packages = c("DESeq2", "dplyr", "ggplot2"))

# 3. 运行分析（伪代码）
# ... 你的分析代码 ...

# 4. DCE分析
dce_results <- run_dce_analysis(
  wt_data = normal_samples,
  mt_data = tumor_samples,
  network_method = "aracne"
)

# 5. 可视化
p1 <- plot_dce_volcano(dce_results$dce_results)
p2 <- plot_dce_network(dce_results$dce_results, highlight_nodes = "TP53")

# 6. 导出结果
batch_export(
  list(volcano = p1, network = p2),
  path = "data/output/figures",
  formats = c("pdf", "png")
)

# 7. 保存工作区
save_end_day()
```

### 示例2：单细胞数据分析

```r
library(Seurat)
library(kyplus)

# 初始化
init_mirai(
  n_cores = 16,
  packages = c("Seurat", "dplyr", "ggplot2")
)

# 使用Goldfish配色
DimPlot(seurat_obj, group.by = "celltype") +
  scale_color_goldfish(palette = "vibrant")

# 批量导出细胞类型标记基因图
marker_plots <- lapply(unique(seurat_obj$celltype), function(ct) {
  FeaturePlot(seurat_obj, features = markers[[ct]]) +
    scale_color_goldfish_c(palette = "ocean")
})

batch_export(
  marker_plots,
  path = "figures/markers",
  formats = "pdf"
)
```

---

## 📦 依赖关系

### 必需依赖（Imports）

```r
# 核心
yaml, git2r, usethis, Rcpp, parallel, methods, stats, utils

# 数据处理
dplyr, tidyr, stringr, jsonlite, magrittr

# 可视化
ggplot2, tools

# 生物信息
naturalsort, ridge, ppcor, infotheo, minet, graph

# 数据存储
qs, reshape2
```

### 可选依赖（Suggests）

```r
# 测试和文档
testthat (>= 3.0.0), knitr, rmarkdown

# DCE分析
dce, mnem

# 通路富集
clusterProfiler, enrichplot, DOSE, ReactomePA, msigdbr

# 物种数据库
org.Hs.eg.db, org.Mm.eg.db, org.Rn.eg.db

# 增强可视化
ggrepel, patchwork

# 并行计算
mirai, future.apply
```

---

## 🤝 贡献

欢迎贡献代码、报告问题或建议新功能！

### 报告问题

在 [GitHub Issues](https://github.com/ixxmu/kyplus/issues) 提交问题报告。

### 贡献代码

1. Fork本仓库
2. 创建功能分支 (`git checkout -b feature/AmazingFeature`)
3. 提交更改 (`git commit -m 'Add some AmazingFeature'`)
4. 推送到分支 (`git push origin feature/AmazingFeature`)
5. 开启Pull Request

### 开发指南

```r
# 克隆仓库
git clone https://github.com/ixxmu/kyplus.git
cd kyplus

# 安装开发依赖
devtools::install_dev_deps()

# 运行测试
devtools::test()

# 检查包
devtools::check()

# 生成文档
devtools::document()
```

---

## 📄 许可证

本项目采用 MIT 许可证 - 查看 [LICENSE](LICENSE) 文件了解详情。

---

## 📮 联系方式

- **作者**: Your Name
- **Email**: your.email@example.com
- **主页**: https://ixxmu.github.io/kyplus
- **问题反馈**: https://github.com/ixxmu/kyplus/issues

---

## 🙏 致谢

### 引用

如果在研究中使用了kyplus，请引用：

**DCE方法**:
> Pirkl, M., et al. (2022). Identifying cancer pathway dysregulations using differential causal effects. *Bioinformatics*, 38(6), 1550-1559.

**kyplus包**:
> [待添加引用信息]

### 依赖的优秀项目

- [dce](https://bioconductor.org/packages/dce) - Differential Causal Effects
- [mirai](https://github.com/shikokuchuo/mirai) - 现代并行计算框架
- [clusterProfiler](https://guangchuangyu.github.io/software/clusterProfiler/) - 通路富集分析
- [ggplot2](https://ggplot2.tidyverse.org/) - 优雅的数据可视化

---

## 📊 状态

![GitHub last commit](https://img.shields.io/github/last-commit/ixxmu/kyplus)
![GitHub code size](https://img.shields.io/github/languages/code-size/ixxmu/kyplus)
![Lines of code](https://img.shields.io/tokei/lines/github/ixxmu/kyplus)

---

<div align="center">
  <sub>Built with ❤️ by the kyplus team</sub>
</div>
