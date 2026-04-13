# 创建包环境
.kyplus_env <- new.env(parent = emptyenv())

#' @keywords internal
create_rproj_file <- function(path) {
  # RStudio project settings in DCF format (RStudio's native format)
  # Do NOT use YAML format - RStudio expects this specific format
  #
  # IMPORTANT: Must avoid UTF-8 BOM (Byte Order Mark) which corrupts
  # DCF parsing and causes RStudio to crash on opening.
  # Use a raw file connection instead of writeLines(text, filename) to
  # guarantee no BOM is prepended.
  proj_content <- paste(
    "Version: 1.0",
    "",
    "RestoreWorkspace: Default",
    "SaveWorkspace: Default",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX",
    "",
    "AutoAppendNewline: Yes",
    "StripTrailingWhitespace: Yes",
    "",  # trailing newline
    sep = "\n"
  )

  proj_file <- file.path(path, paste0(basename(path), ".Rproj"))

  # Write as raw bytes to avoid BOM insertion on Windows
  con <- file(proj_file, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(proj_content), con)

  invisible(proj_file)
}

#' @keywords internal
create_directory_structure <- function(path, type) {
  dirs <- if (type == "analysis") {
    c(
      "R/functions",
      "R/pipelines",
      "data/raw",
      "data/processed",
      "data/output",
      "docs/specifications",
      "docs/reports",
      "tests/unit",
      "tests/integration",
      "config"
    )
  } else {
    c(
      "R",
      "man",
      "data-raw",
      "inst/extdata",
      "vignettes",
      "tests/testthat",
      "config"
    )
  }
  sapply(file.path(path, dirs), dir.create, recursive = TRUE)
}

#' @keywords internal
validate_project_path <- function(path) {
  if (!dir.exists(path)) {
    stop("Project directory does not exist: ", path)
  }
}

#' @keywords internal
load_project_config <- function(path) {
  config_file <- file.path(path, "config/config.yml")
  if (file.exists(config_file)) {
    config <- yaml::read_yaml(config_file)
    # 将配置保存到包环境而非全局环境
    assign("project_config", config, envir = .kyplus_env)
    return(config)
  }
  return(NULL)
}

#' Get the current project configuration
#'
#' Retrieves the project configuration that was previously loaded
#' using load_project_config().
#'
#' @return The project configuration as a list, or NULL if no configuration
#'   has been loaded.
#' @export
get_project_config <- function() {
  if (exists("project_config", envir = .kyplus_env)) {
    return(.kyplus_env$project_config)
  }
  return(NULL)
}

#' @keywords internal
load_project_environment <- function(path) {
  if (file.exists(file.path(path, "renv.lock"))) {
    renv::load(path)
  }
}

#' Check system performance and configuration
#'
#' Displays information about the system configuration,
#' including thread settings, parallel cores, and memory usage.
#'
#' @return Nothing, displays information to the console.
#' @export
check_performance <- function() {
  cat("Current settings:\n")
  cat("\nThread settings:\n")
  cat("OMP_NUM_THREADS:", Sys.getenv("OMP_NUM_THREADS"), "\n")
  cat("MKL_NUM_THREADS:", Sys.getenv("MKL_NUM_THREADS"), "\n")
  cat("OPENBLAS_NUM_THREADS:", Sys.getenv("OPENBLAS_NUM_THREADS"), "\n")
  cat("MAKEFLAGS:", Sys.getenv("MAKEFLAGS"), "\n")
  cat("\nParallel cores:", getOption("mc.cores"), "\n")
  if(file.exists("~/.R/Makevars")) {
    cat("\nMakevars configuration:\n")
    cat(readLines("~/.R/Makevars"), sep = "\n")
  }
  if(Sys.info()[['sysname']] == "Linux") {
    cat("\nMemory info:\n")
    system("free -h")
  }
}

#' Quantile Normalisation
#'
#' 对数据框或矩阵按列进行分位数归一化（Quantile Normalisation）。
#' 每列样本的值分布将被强制对齐为相同的分布。
#'
#' @param df 数值型 data.frame 或 matrix，行为特征（features），列为样本（samples）
#' @return 与输入同维度的 data.frame，保留原始行名，值已完成分位数归一化
#'
#' @examples
#' mat <- matrix(c(5, 2, 3, 4,
#'                 1, 4, 4, 2,
#'                 3, 6, 6, 1), nrow = 4)
#' quantile_normalisation(mat)
#' @export
quantile_normalisation <- function(df) {
  # Step 1: 对每列按值排名，并列时按出现顺序处理
  df_rank <- apply(df, 2, rank, ties.method = "first")

  # Step 2: 对每列独立排序
  df_sorted <- apply(df, 2, sort)

  # Step 3: 计算排序后每行的均值（即各分位点的目标值）
  # 用 rowMeans 替代 apply(..., 1, mean)，更高效
  df_mean <- rowMeans(df_sorted)

  # Step 4: 用排名索引映射回均值，完成归一化
  df_final <- apply(df_rank, 2, function(r) df_mean[r])

  rownames(df_final) <- rownames(df)
  # 返回 data.frame
  return(as.data.frame(df_final))
}

#' 自动化表达矩阵 log2 转换函数
#'
#' @param exprSet 输入的表达矩阵或数据框（仅限数值列）
#' @param min_threshold 负值安全阈值，默认 -10。最小值低于此值则判定为已转换
#' @param force_log 是否强制执行转换（忽略判断逻辑），默认为 FALSE
#'
#' @return 返回处理后的表达矩阵
#' @export
auto_log2_transform <- function(exprSet, min_threshold = -10, force_log = FALSE) {

  # 直接检查每列是否为数值，兼容 matrix 和 data.frame
  if (is.data.frame(exprSet)) {
    if (!all(sapply(exprSet, is.numeric))) {
      stop("错误：输入数据包含非数值列，请先提取数值列。")
    }
  } else if (is.matrix(exprSet)) {
    if (!is.numeric(exprSet)) {
      stop("错误：输入矩阵包含非数值字符。")
    }
  } else {
    stop("错误：输入必须是 matrix 或 data.frame。")
  }

  qx <- as.numeric(quantile(unlist(exprSet), c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm = TRUE))

  LogC_heuristic <- (qx[5] > 100) ||
    (qx[6] - qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)

  Already_Transformed <- qx[1] < min_threshold

  if (force_log || (LogC_heuristic && !Already_Transformed)) {
    exprSet[exprSet < 0] <- 0
    expr_processed <- log2(exprSet + 0.0001)
    if (force_log) {
      message(">>> [Action] 强制模式：已执行 log2(x+1) 转换。")
    } else {
      message(">>> [Action] 检测到原始/线性数据，已执行 log2(x+1) 转换。")
    }
  } else if (Already_Transformed) {
    expr_processed <- exprSet
    message(">>> [Skip] 安全门拦截：最小值 ", round(qx[1], 2),
            " < ", min_threshold, "，判定为已转换数据，跳过转换。")
  } else {
    expr_processed <- exprSet
    message(">>> [Skip] 数据分布特征显示无需进行 log2 转换。")
  }

  return(expr_processed)
}
