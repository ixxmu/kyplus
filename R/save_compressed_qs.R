#' Save R Objects with qs High-Performance Compression (kyplus升级版)
#'
#' Ultra-fast workspace saving using optimized qs::qsave + as.list approach.
#' Designed for end-of-day workspace backup and next-day restoration.
#' Objects are saved as named list and restored with original names.
#'
#' @param ... Objects to save, specified as names or character strings.
#'            If empty, saves entire workspace.
#' @param list Character vector of object names to save
#' @param file Output file path. If NULL, auto-generates timestamped filename
#' @param preset qs compression preset: "fast", "balanced", "high" (default: "balanced")
#' @param nthreads Number of threads for compression (default: parallel::detectCores())
#' @param envir Environment to search for objects (default: .GlobalEnv)
#' @param clean_after Whether to clean workspace after saving (default: FALSE)
#' @param verbose Whether to show progress information (default: TRUE)
#'
#' @return Invisibly returns list with saved file path and object names
#' @export
#'
#' @examples
#' \dontrun{
#' # Save specific objects (ultra-fast qsave method)
#' x <- 1:10
#' y <- letters
#' save_compressed_qs(x, y, file = "my_data.qs")
#'
#' # End-of-day: save everything and clean
#' save_compressed_qs(clean_after = TRUE)
#'
#' # Load back with original names preserved
#' load_compressed_qs("my_data.qs") # x and y restored with qread + list2env
#' }
#'
save_compressed_qs <- function(..., list = character(), file = NULL,
                               preset = c("balanced", "fast", "high"),
                               nthreads = parallel::detectCores() - 1,
                               envir = .GlobalEnv,
                               clean_after = FALSE,
                               verbose = TRUE) {
  # 检查 qs 包
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("qs package is required. Install with: install.packages('qs')")
  }

  preset <- match.arg(preset)

  # 处理变量名
  if (...length()) {
    dots <- match.call(expand.dots = FALSE)$...
    names <- vapply(dots, function(x) {
      if (is.character(x)) {
        return(x)
      }
      if (is.name(x)) {
        return(as.character(x))
      }
      stop("... must contain names or character strings")
    }, "")
    list <- c(list, names)
  }

  # 如果没有指定对象，保存整个工作空间
  if (length(list) == 0) {
    list <- ls(envir = envir)
    if (verbose) cat("💾 保存整个工作空间 (", length(list), " 个对象)\n", sep = "")
  }

  if (length(list) == 0) {
    if (verbose) cat("⚠️  工作空间为空，无需保存\n")
    return(invisible(list(file = NULL, objects = character())))
  }

  # 检查对象是否存在
  missing <- !vapply(list, exists, logical(1), envir = envir)
  if (any(missing)) {
    stop("Objects not found: ", paste(list[missing], collapse = ", "))
  }

  # 生成文件名（如果未指定）
  if (is.null(file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file <- paste0("kyplus_workspace_", timestamp, ".qs")
  }

  if (verbose) {
    cat("🔄 正在保存到:", file, "\n")
    cat("📊 压缩模式:", preset, "| 线程数:", nthreads, "\n")
  }

  # 获取对象并计算大小
  if (verbose) {
    total_size <- sum(sapply(list, function(name) {
      object.size(get(name, envir = envir))
    }))
    cat("💾 数据大小:", format(total_size, units = "MB"), "\n")
  }

  # 保存对象 - 使用高性能的 qsave + as.list 方案
  tryCatch(
    {
      if (length(list) == length(ls(envir = envir))) {
        # 保存整个工作空间（最快方案）
        workspace_list <- as.list(envir)
        qs::qsave(workspace_list, file, preset = preset, nthreads = nthreads)
      } else {
        # 保存特定对象
        objects <- setNames(lapply(list, function(name) get(name, envir = envir)), list)
        qs::qsave(objects, file, preset = preset, nthreads = nthreads)
      }

      if (verbose) {
        file_size <- file.size(file)
        compression_ratio <- if (exists("total_size")) {
          round(as.numeric(total_size) / file_size, 1)
        } else {
          NA
        }
        cat("✅ 保存完成！\n")
        cat("📁 文件大小:", round(file_size / 1024^2, 1), "MB")
        if (!is.na(compression_ratio)) cat(" (压缩比: ", compression_ratio, "x)", sep = "")
        cat("\n")
      }

      # 清理工作空间（如果需要）
      if (clean_after) {
        if (verbose) cat("🧹 正在清理工作空间...\n")

        # 先释放函数内部的副本
        if (exists("workspace_list", inherits = FALSE)) rm(workspace_list)
        if (exists("objects", inherits = FALSE)) rm(objects)
        if (exists("total_size", inherits = FALSE)) rm(total_size)

        rm(list = ls(all.names = TRUE, envir = envir), envir = envir)
        invisible(gc())
        invisible(gc()) # 双重垃圾回收

        if (verbose) {
          cat("✨ 环境已清理！\n")
          cat("📝 恢复命令: load_compressed_qs('", file, "')\n", sep = "")
        }
      }

      invisible(list(file = file, objects = list))
    },
    error = function(e) {
      stop("保存失败: ", e$message)
    }
  )
}

#' Load R Objects from qs Compressed File
#'
#' Load objects saved by save_compressed_qs. Auto-detects latest backup if no file specified.
#'
#' @param file File path to load from. If NULL, loads latest kyplus workspace backup
#' @param envir Environment to load objects into (default: .GlobalEnv)
#' @param verbose Whether to show progress information (default: TRUE)
#' @param nthreads Number of threads for decompression (default: parallel::detectCores() - 1)
#' @param pattern File pattern for auto-detection (default: "kyplus_workspace_.*\\.qs$")
#' @param path Directory to search for backup files (default: ".")
#'
#' @return Invisibly returns names of loaded objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Load specific file
#' load_compressed_qs("my_data.qs")
#'
#' # Auto-load latest backup (next-day workflow)
#' load_compressed_qs()
#'
#' # Load into different environment
#' my_env <- new.env()
#' load_compressed_qs("data.qs", envir = my_env)
#' }
#'
load_compressed_qs <- function(file = NULL, envir = .GlobalEnv, verbose = TRUE,
                               pattern = "kyplus_workspace_.*\\.qs$", path = ".",
                               nthreads = parallel::detectCores() - 1) {
  # 检查 qs 包
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("qs package is required. Install with: install.packages('qs')")
  }

  # 自动查找最新备份
  if (is.null(file)) {
    backup_files <- list.files(path, pattern = pattern, full.names = TRUE)

    if (length(backup_files) == 0) {
      stop("未找到工作空间备份文件。使用 save_compressed_qs() 创建备份。")
    }

    # 选择最新的
    file_info <- file.info(backup_files)
    file <- backup_files[which.max(file_info$mtime)]

    if (verbose) {
      cat("🔍 自动选择最新备份:", basename(file), "\n")
      cat("🕒 创建时间:", format(file_info$mtime[which.max(file_info$mtime)], "%Y-%m-%d %H:%M:%S"), "\n")
    }
  }

  if (!file.exists(file)) {
    stop("文件不存在: ", file)
  }

  if (verbose) {
    cat("📂 正在加载:", basename(file), "\n")
    file_size <- file.size(file)
    cat("📁 文件大小:", round(file_size / 1024^2, 1), "MB\n")
  }

  # 加载数据 - 使用高性能的 qread + list2env 方案
  tryCatch(
    {
      # 使用 qread 读取数据（支持多线程，性能最佳）
      data <- qs::qread(file, nthreads = nthreads)

      if (is.list(data) && !is.null(names(data))) {
        # 多对象情况：显示对象数量，然后恢复
        if (verbose) cat("🔄 正在恢复", length(data), "个对象...\n")

        # 直接用 list2env（最快）
        list2env(data, envir = envir)
        object_names <- names(data)

        if (verbose) {
          total_size <- sum(sapply(object_names, function(x) {
            as.numeric(object.size(get(x, envir = envir)))
          }))
          cat("✅ 恢复完成！\n")
          cat("📊 对象数量:", length(object_names), "\n")
          cat("💾 内存占用:", format(structure(total_size, class = "object_size"), units = "MB"), "\n")
        }
      } else {
        # 单对象情况
        if (verbose) cat("🔄 正在恢复 1 个对象...\n")

        obj_name <- tools::file_path_sans_ext(basename(file))
        obj_name <- gsub("^kyplus_workspace_.*", "restored_workspace", obj_name)

        assign(obj_name, data, envir = envir)
        object_names <- obj_name

        if (verbose) {
          cat("✅ 恢复完成！\n")
          cat("📊 对象数量: 1\n")
          cat("💾 内存占用:", format(object.size(data), units = "MB"), "\n")
        }
      }

      if (verbose) {
        today <- format(Sys.Date(), "%Y年%m月%d日")
        cat("🚀 今天的日期是", today, "，继续你的分析吧！\n")
      }

      invisible(object_names)
    },
    error = function(e) {
      stop("加载失败: ", e$message)
    }
  )
}

#' 下班前保存工作空间 (Convenience wrapper)
#'
#' Convenient function for end-of-day workflow. Saves entire workspace, cleans environment,
#' and restarts R session by default for a fresh start tomorrow.
#'
#' @param file Custom filename (optional)
#' @param preset Compression preset (default: "balanced")
#' @param restart_session Whether to restart R session after saving (default: TRUE)
#' @param verbose Show progress (default: TRUE)
#' @param nthreads Number of threads for compression (default: cores - 1)
#'
#' @export
save_end_day <- function(file = NULL, preset = "balanced", restart_session = TRUE,
                         verbose = TRUE, nthreads = parallel::detectCores() - 1) {
  # 保存工作空间
  result <- save_compressed_qs(
    file = file, preset = preset, clean_after = TRUE,
    verbose = verbose, nthreads = nthreads
  )

  # 重启 R session
  if (restart_session) {
    if (verbose) {
      cat("🔄 准备退出并重启 R session...\n")
      cat("💡 明天使用 load_start_day() 恢复工作环境\n")
      cat("⚡ 使用 q(save = 'no') 快速重启，不保存当前环境\n")
      Sys.sleep(1) # 给用户一秒钟看到信息
    }

    # 使用 q(save = "no") 退出 R，不保存工作空间
    # 这种方式适用于所有 R 环境，不仅限于 RStudio
    q(save = "no")
  }
  invisible(result)
}

#' 上班后恢复工作空间 (Convenience wrapper)
#'
#' Convenient function for start-of-day workflow. Auto-loads latest workspace backup.
#'
#' @param verbose Show progress (default: TRUE)
#' @param nthreads Number of threads for decompression (default: cores - 1)
#'
#' @export
load_start_day <- function(verbose = TRUE, nthreads = parallel::detectCores() - 1) {
  load_compressed_qs(verbose = verbose, nthreads = nthreads)
}
