# kegg_latest function 函数
#' 获取和更新KEGG通路数据
#'@importFrom methods new
#' @importFrom stats as.formula setNames
#' @importFrom utils install.packages installed.packages
#' @importFrom magrittr %>%
#' @description
#' 使用不同方法获取和更新KEGG通路数据。支持三种方法：
#' - "webjson": 从KEGG官网下载的JSON文件解析
#' - "yulab": 使用clusterProfiler的gson_KEGG函数获取
#' - "gseadbr": 使用msigdbr包获取
#'
#' @param method 获取KEGG数据的方法，可选"webjson", "yulab", "gseadbr"或它们的组合
#' @param species 物种代码或名称，对于"webjson"和"yulab"方法使用代码(如"hsa")，对于"gseadbr"方法使用缩写(如"HS")
#' @param json_file "webjson"方法使用的JSON文件路径，当method="webjson"且文件不存在时会提示错误
#' @param save 是否保存结果
#' @param save_path 结果保存路径，默认为当前工作目录
#' @param save_format 保存格式，"rds", "gmt"或"gson"
#' @param prefix 保存文件的前缀，默认为"kegg_数据获取方法"
#' @param add_date 是否在保存的文件名中添加日期
#' @param save_webjson_full 是否保存webjson方法的完整数据，默认为TRUE
#' @param gmt_type 生成GMT文件时使用的基因标识符类型，可选"symbol"、"entrezID"或"both"，默认为"both"
#'
#' @return 返回包含各个方法结果的列表，每个方法的结果为数据框
#'
#' @examples
#' \dontrun{
#' # 使用msigdbr获取KEGG数据
#' kegg_data <- update_kegg()
#'
#' # 使用yulab方法并保存为RDS
#' kegg_data <- update_kegg(method = "yulab", species = "hsa",
#'                        save = TRUE, save_format = "rds")
#'
#' # 从本地JSON文件解析，保留原始数据
#' kegg_data <- update_kegg(method = "webjson",
#'                        json_file = "path/to/hsa00001.json")
#'
#' # 多种方法获取并比较
#' kegg_data <- update_kegg(method = c("yulab", "gseadbr"))
#'
#' # 保存为GMT格式，同时生成symbol和entrezID两个文件
#' kegg_data <- update_kegg(method = "yulab", save = TRUE,
#'                        save_format = "gmt", gmt_type = "both")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest separate
#' @importFrom dplyr mutate select filter
#' @importFrom stringr str_split
#' @export
update_kegg <- function(method = "gseadbr",
                        species = NULL,
                        json_file = NULL,
                        save = FALSE,
                        save_path = getwd(),
                        save_format = "rds",
                        prefix = NULL,
                        add_date = TRUE,
                        save_webjson_full = TRUE,
                        gmt_type = "both") {

  # 参数验证
  method <- match.arg(method, c("webjson", "yulab", "gseadbr"), several.ok = TRUE)
  save_format <- match.arg(save_format, c("rds", "gmt", "gson"))
  gmt_type <- match.arg(gmt_type, c("symbol", "entrezID", "both"))

  # 设置默认物种
  if (is.null(species)) {
    species <- ifelse(any(method %in% c("webjson", "yulab")), "hsa", "HS")
  }

  # 创建获取方法函数列表
  get_methods <- list(
    webjson = function() {
      if (is.null(json_file) || !file.exists(json_file)) {
        stop("使用webjson方法时必须提供有效的json_file参数。\n\n",
             ">>> 请按以下步骤手动下载 hsa00001.json 文件：\n",
             "  1. 打开网页: https://www.kegg.jp/kegg-bin/get_htext?hsa00001.keg\n",
             "  2. 在页面左上角点击 'Download htext' 或 'Download json' 按钮\n",
             "  3. 选择 JSON 格式下载，保存为 hsa00001.json\n",
             "  4. 将文件路径传入 json_file 参数，例如:\n",
             "     update_kegg(method = \"webjson\", json_file = \"path/to/hsa00001.json\")\n")
      }

      # 解析JSON
      tryCatch({
        dat_gson <- jsonlite::fromJSON(json_file, flatten = TRUE)

        # 完整处理数据
        full_data <- tidyr::unnest(dat_gson$children) %>%
          tidyr::unnest() %>%
          tidyr::unnest() %>%
          tidyr::separate(col = name3, sep = ";", into = c("genes", "details")) %>%
          tidyr::separate(col = details, sep = "\t", into = c("detail", NA)) %>%
          tidyr::separate(col = genes, sep = " ", into = c("entrezID", "symbol")) %>%
          tidyr::separate(col = name, sep = " ", into = c("s1ID", "L1"), extra = "merge") %>%
          tidyr::separate(col = name1, sep = " ", into = c("s2ID", "L2"), extra = "merge") %>%
          dplyr::mutate(s3ID = stringr::str_split(name2,
                                                  " ", n = 2, simplify = TRUE)[, 1],
                        .before = entrezID) %>%
          dplyr::mutate(s3ID_raw = stringr::str_split(name2,
                                                      " ", n = 2, simplify = TRUE)[, 2],
                        .before = entrezID) %>%
          dplyr::mutate(L3 = stringr::str_split(s3ID_raw,
                                                " \\[PATH:", n = 2, simplify = TRUE)[, 1],
                        .before = entrezID) %>%
          dplyr::select(-c(name2, s3ID_raw))

        # 保存完整数据（如果需要）
        if (save_webjson_full) {
          date_str <- if (add_date) format(Sys.Date(), "_%Y%m%d") else ""
          file_name <- paste0("kegg_webjson_full", date_str, ".rds")
          full_path <- file.path(save_path, file_name)
          saveRDS(full_data, file = full_path)
          message("完整的webjson KEGG数据已保存至: ", full_path)
        }

        # 返回简化数据
        dplyr::select(full_data, pathway = L3, entrezID, symbol)
      }, error = function(e) {
        stop("解析JSON文件时出错: ", e$message)
      })
    },

    yulab = function() {
      # 检查必要的包
      if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
        stop("需要安装clusterProfiler包: BiocManager::install('clusterProfiler')")
      }

      # 获取KEGG数据
      tryCatch({
        kegg_gson <- clusterProfiler::gson_KEGG(species)

        # 处理数据
        kegg_gsea1 <- kegg_gson@gsid2gene
        kegg_gsea2 <- kegg_gson@gsid2name
        kegg_gsea <- merge(kegg_gsea1, kegg_gsea2)
        colnames(kegg_gsea)[c(3, 2)] <- c("pathway", "entrezID")

        # 创建返回数据框
        result <- dplyr::select(kegg_gsea, pathway, entrezID)

        # 如果需要symbol列且没有，仅在条件允许时尝试获取
        if (species == "hsa") {
          tryCatch({
            # 先检查所需的包
            if (requireNamespace("org.Hs.eg.db", quietly = TRUE) &&
                requireNamespace("AnnotationDbi", quietly = TRUE)) {
              message("正在添加基因符号信息...")

              # 直接添加symbol列
              result$symbol <- NA

              # 获取entrezID对应的基因符号
              entrez_ids <- as.character(unique(result$entrezID))

              # 使用suppressMessages和tryCatch避免警告和错误中断
              suppressMessages({
                symbols_df <- tryCatch({
                  AnnotationDbi::select(org.Hs.eg.db::org.Hs.eg.db,
                                        keys = entrez_ids,
                                        columns = "SYMBOL",
                                        keytype = "ENTREZID")
                }, error = function(e) {
                  message("获取基因符号时出错: ", e$message)
                  return(data.frame(ENTREZID = entrez_ids, SYMBOL = NA))
                })
              })

              # 创建映射字典
              symbol_map <- setNames(as.character(symbols_df$SYMBOL),
                                     as.character(symbols_df$ENTREZID))

              # 填充symbol列
              result$symbol <- symbol_map[as.character(result$entrezID)]
              result$symbol[is.na(result$symbol)] <- result$entrezID[is.na(result$symbol)]
            } else {
              message("无法添加基因符号信息。需要安装org.Hs.eg.db包: BiocManager::install('org.Hs.eg.db')")
              result$symbol <- result$entrezID  # 使用entrezID作为替代
            }
          }, error = function(e) {
            message("添加基因符号时出错，使用entrezID作为替代: ", e$message)
            result$symbol <- result$entrezID
          })
        } else {
          # 非人类物种，使用entrezID作为symbol
          message("非人类物种数据，使用entrezID代替symbol")
          result$symbol <- result$entrezID
        }

        return(result)
      }, error = function(e) {
        stop("使用clusterProfiler获取KEGG数据时出错: ", e$message)
      })
    },

    gseadbr = function() {
      # 检查必要的包
      if (!requireNamespace("msigdbr", quietly = TRUE)) {
        stop("需要安装msigdbr包: install.packages('msigdbr')")
      }

      # 获取KEGG数据
      tryCatch({
        kegg_data <- msigdbr::msigdbr(db_species = species,
                                      collection = "C2",
                                      subcollection = "CP:KEGG_LEGACY")

        # 如果存在ncbi_gene，添加到结果中
        if ("ncbi_gene" %in% colnames(kegg_data)) {
          kegg_data %>%
            dplyr::select(pathway = gs_name,
                          symbol = gene_symbol,
                          entrezID = ncbi_gene)
        } else {
          # 如果没有ncbi_gene列，创建一个空列
          result <- kegg_data %>%
            dplyr::select(pathway = gs_name, symbol = gene_symbol)
          result$entrezID <- NA
          return(result)
        }
      }, error = function(e) {
        stop("使用msigdbr获取KEGG数据时出错: ", e$message)
      })
    }
  )

  # 使用元编程方法获取所有请求的方法结果
  results <- lapply(method, function(m) {
    get_methods[[m]]()
  })
  names(results) <- method

  # 如果只选择了一种方法，直接返回结果
  if (length(method) == 1) {
    result <- results[[1]]
  } else {
    result <- results
  }

  # 保存结果
  if (save) {
    # 使用你的自定义保存函数
    if (!is.null(try(getNamespace("kyplus")$save_compressed, silent = TRUE))) {
      kyplus::save_compressed(result,
                     file = if(is.null(prefix)) paste0("kegg_", paste(method, collapse="_")) else prefix,
                     path = save_path,
                     format = save_format,
                     add_date = add_date)
    } else {
      # 设置默认前缀
      if (is.null(prefix)) {
        if (length(method) == 1) {
          prefix <- paste0("kegg_", method)
        } else {
          prefix <- paste0("kegg_multi_method")
        }
      }

      # 添加日期
      if (add_date) {
        date_str <- format(Sys.Date(), "_%Y%m%d")
        file_name <- paste0(prefix, date_str)
      } else {
        file_name <- prefix
      }

      # 根据格式保存
      if (save_format == "rds") {
        file_path <- file.path(save_path, paste0(file_name, ".rds"))
        saveRDS(result, file = file_path)
        message("KEGG数据已保存至: ", file_path)
      } else if (save_format == "gmt") {
        # 根据gmt_type参数决定保存方式
        write_gmt(result, file_path = save_path, file_name = file_name, type = gmt_type)
      } else if (save_format == "gson") {
        file_path <- file.path(save_path, paste0(file_name, ".gson"))
        if (!requireNamespace("gson", quietly = TRUE)) {
          warning("需要安装gson包来保存gson格式: BiocManager::install('gson')")
        } else {
          gson::write.gson(result, file = file_path)
          message("KEGG数据已保存至: ", file_path)
        }
      }
    }
  }

  return(result)
}

#' 将数据框写入GMT文件
#'
#' 支持灵活的列名映射，可以处理不同来源的基因集数据。
#' 自动识别常见列名（如pathway/term, symbol/gene, entrezID/ncbi_gene等），
#' 也可以通过参数手动指定列名。
#'
#' @param data 基因集数据，可以是数据框或数据框列表
#' @param file_path 保存文件目录，默认为当前工作目录
#' @param file_name 文件名前缀，默认为"geneset"
#' @param type GMT文件中使用的基因类型，可选"symbol"、"entrezID"或"both"
#' @param pathway_col 通路/基因集名称列名，默认自动识别（pathway, term, gs_name）
#' @param symbol_col 基因符号列名，默认自动识别（symbol, gene, gene_symbol）
#' @param entrez_col entrezID列名，默认自动识别（entrezID, entrez_gene, ncbi_gene）
#' @param desc_prefix GMT文件中描述列的前缀，默认为"KEGG_"，设为NULL则使用通路名
#'
#' @return 无返回值（不可见地返回NULL）
#'
#' @examples
#' \dontrun{
#' # 标准列名（pathway, symbol, entrezID）
#' write_gmt(kegg_data, file_path = ".", file_name = "kegg")
#'
#' # webjson解析的数据（term, gene, entrezID）
#' write_gmt(kegg_data_json, file_path = ".", file_name = "kegg_json")
#'
#' # 手动指定列名
#' write_gmt(my_data, file_path = ".", file_name = "custom",
#'           pathway_col = "gs_name", symbol_col = "gene_symbol")
#'
#' # 只生成symbol类型
#' write_gmt(kegg_data, file_path = ".", file_name = "kegg", type = "symbol")
#'
#' # 自定义描述前缀
#' write_gmt(kegg_data, file_path = ".", file_name = "go",
#'           desc_prefix = "GO_")
#' }
#'
#' @export
write_gmt <- function(data,
                      file_path = getwd(),
                      file_name = "geneset",
                      type = "both",
                      pathway_col = NULL,
                      symbol_col = NULL,
                      entrez_col = NULL,
                      desc_prefix = "KEGG_") {
  # 确保类型参数有效
  type <- match.arg(type, c("symbol", "entrezID", "both"))

  # 处理列表类型的数据
  if (is.list(data) && !is.data.frame(data)) {
    for (name in names(data)) {
      df <- data[[name]]
      write_single_gmt(df, file_path, paste0(file_name, "_", name), type,
                        pathway_col, symbol_col, entrez_col, desc_prefix)
    }
  } else if (is.data.frame(data)) {
    write_single_gmt(data, file_path, file_name, type,
                      pathway_col, symbol_col, entrez_col, desc_prefix)
  }

  invisible(NULL)
}

#' 辅助函数，将单个数据框写入GMT文件
#'
#' @param df 数据框
#' @param file_path 保存文件目录
#' @param file_name 文件名前缀
#' @param type GMT文件中使用的基因类型
#' @param pathway_col 通路列名
#' @param symbol_col 基因符号列名
#' @param entrez_col entrezID列名
#' @param desc_prefix 描述前缀
#' @return 无返回值
#' @keywords internal
write_single_gmt <- function(df, file_path, file_name, type,
                              pathway_col = NULL, symbol_col = NULL,
                              entrez_col = NULL, desc_prefix = "KEGG_") {
  cols <- colnames(df)

  # 自动识别通路列
  pw_col <- pathway_col %||%
    intersect(cols, c("pathway", "term", "gs_name"))[1]
  if (is.na(pw_col) || is.null(pw_col)) {
    stop("未找到通路列，请通过pathway_col参数指定。可选列名: ",
         paste(cols, collapse = ", "))
  }

  # 自动识别symbol列
  sym_col <- symbol_col %||%
    intersect(cols, c("symbol", "gene", "gene_symbol"))[1]

  # 自动识别entrezID列
  ent_col <- entrez_col %||%
    intersect(cols, c("entrezID", "entrez_gene", "ncbi_gene"))[1]

  # 根据类型和可用列决定写入哪些文件
  if (type == "both" || type == "symbol") {
    if (!is.na(sym_col) && !is.null(sym_col)) {
      symbol_file <- file.path(file_path, paste0(file_name, "_symbol.gmt"))
      write_gmt_by_type(df, symbol_file, gene_col = sym_col,
                         pathway_col = pw_col, desc_prefix = desc_prefix)
      message("基因符号GMT文件已保存至: ", symbol_file)
    } else {
      warning("数据中未找到symbol列(symbol/gene/gene_symbol)，无法生成基因符号GMT文件。",
              "可通过symbol_col参数指定。")
    }
  }

  if (type == "both" || type == "entrezID") {
    if (!is.na(ent_col) && !is.null(ent_col)) {
      entrez_file <- file.path(file_path, paste0(file_name, "_entrezID.gmt"))
      write_gmt_by_type(df, entrez_file, gene_col = ent_col,
                         pathway_col = pw_col, desc_prefix = desc_prefix)
      message("EntrezID GMT文件已保存至: ", entrez_file)
    } else {
      warning("数据中未找到entrezID列(entrezID/entrez_gene/ncbi_gene)，无法生成EntrezID GMT文件。",
              "可通过entrez_col参数指定。")
    }
  }
}

#' 按指定类型写入GMT文件
#'
#' @param df 数据框
#' @param file_path 完整文件路径
#' @param gene_col 基因列名称
#' @param pathway_col 通路列名称
#' @param desc_prefix 描述前缀
#' @return 无返回值
#' @keywords internal
write_gmt_by_type <- function(df, file_path, gene_col,
                               pathway_col = "pathway",
                               desc_prefix = "KEGG_") {
  if (!gene_col %in% colnames(df)) {
    warning("列 '", gene_col, "' 不存在于数据中。")
    return(invisible(NULL))
  }

  # 按通路分组
  pathways <- split(df[[gene_col]], df[[pathway_col]])

  # 写入GMT文件
  con <- file(file_path, open = "w")
  on.exit(close(con))

  lapply(names(pathways), function(pw) {
    genes <- pathways[[pw]]
    genes <- unique(genes[!is.na(genes) & genes != ""])

    if (length(genes) > 0) {
      desc <- if (!is.null(desc_prefix)) paste0(desc_prefix, pw) else pw
      line <- paste(c(pw, desc, genes), collapse = "\t")
      writeLines(line, con)
    }
  })

  invisible(NULL)
}
