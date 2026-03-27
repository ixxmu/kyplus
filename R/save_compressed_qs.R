#' Save R Objects with qs2 High-Performance Compression (kyplus升级版)
#'
#' Ultra-fast workspace saving using optimized qs2::qs_save + as.list approach.
#' Designed for end-of-day workspace backup and next-day restoration.
#' Objects are saved as named list and restored with original names.
#'
#' Note: This function now uses the qs2 package (successor to qs).
#' Files are saved in the new .qs2 format by default.
#' Old .qs files can still be loaded via load_compressed_qs() if the legacy qs package is installed.
#'
#' @param ... Objects to save, specified as names or character strings.
#'            If empty, saves entire workspace.
#' @param list Character vector of object names to save
#' @param file Output file path. If NULL, auto-generates timestamped filename (.qs2)
#' @param compress_level ZSTD compression level 1-22 (default: 3, higher = smaller but slower)
#' @param nthreads Number of threads for compression (default: parallel::detectCores() - 1)
#' @param envir Environment to search for objects (default: .GlobalEnv)
#' @param clean_after Whether to clean workspace after saving (default: FALSE)
#' @param verbose Whether to show progress information (default: TRUE)
#'
#' @return Invisibly returns list with saved file path and object names
#' @export
#'
#' @examples
#' \dontrun{
#' # Save specific objects (ultra-fast qs2 method)
#' x <- 1:10
#' y <- letters
#' save_compressed_qs(x, y, file = "my_data.qs2")
#'
#' # End-of-day: save everything and clean
#' save_compressed_qs(clean_after = TRUE)
#'
#' # Load back with original names preserved
#' load_compressed_qs("my_data.qs2") # x and y restored
#' }
#'
save_compressed_qs <- function(..., list = character(), file = NULL,
                               compress_level = 3L,
                               nthreads = parallel::detectCores() - 1,
                               envir = .GlobalEnv,
                               clean_after = FALSE,
                               verbose = TRUE) {
  # Check qs2 package

  if (!requireNamespace("qs2", quietly = TRUE)) {
    stop("qs2 package is required. Install with: install.packages('qs2')")
  }

  # Process variable names
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

  # If no objects specified, save entire workspace
  if (length(list) == 0) {
    list <- ls(envir = envir)
    if (verbose) cat("\U0001f4be \u4fdd\u5b58\u6574\u4e2a\u5de5\u4f5c\u7a7a\u95f4 (", length(list), " \u4e2a\u5bf9\u8c61)\n", sep = "")
  }

  if (length(list) == 0) {
    if (verbose) cat("\u26a0\ufe0f  \u5de5\u4f5c\u7a7a\u95f4\u4e3a\u7a7a\uff0c\u65e0\u9700\u4fdd\u5b58\n")
    return(invisible(list(file = NULL, objects = character())))
  }

  # Check objects exist
  missing <- !vapply(list, exists, logical(1), envir = envir)
  if (any(missing)) {
    stop("Objects not found: ", paste(list[missing], collapse = ", "))
  }

  # Generate filename (if not specified)
  if (is.null(file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file <- paste0("kyplus_workspace_", timestamp, ".qs2")
  }

  # Auto-append .qs2 extension if missing
  if (!grepl("\\.(qs2|qs)$", file)) {
    file <- paste0(file, ".qs2")
  }

  # Warn if user uses old .qs extension
  if (grepl("\\.qs$", file) && !grepl("\\.qs2$", file)) {
    message("\u2139\ufe0f  kyplus \u5df2\u8fc1\u79fb\u5230 qs2 \u683c\u5f0f\u3002\u5efa\u8bae\u4f7f\u7528 .qs2 \u6269\u5c55\u540d\u3002")
    message("   \u5c06\u4fdd\u5b58\u4e3a: ", sub("\\.qs$", ".qs2", file))
    file <- sub("\\.qs$", ".qs2", file)
  }

  if (verbose) {
    cat("\U0001f504 \u6b63\u5728\u4fdd\u5b58\u5230:", file, "\n")
    cat("\U0001f4ca \u538b\u7f29\u7b49\u7ea7:", compress_level, "| \u7ebf\u7a0b\u6570:", nthreads, "\n")
  }

  # Get objects and calculate size
  if (verbose) {
    total_size <- sum(sapply(list, function(name) {
      object.size(get(name, envir = envir))
    }))
    cat("\U0001f4be \u6570\u636e\u5927\u5c0f:", format(total_size, units = "MB"), "\n")
  }

  # Save objects using qs2
  tryCatch(
    {
      if (length(list) == length(ls(envir = envir))) {
        # Save entire workspace
        workspace_list <- as.list(envir)
        qs2::qs_save(workspace_list, file, compress_level = compress_level, nthreads = nthreads)
      } else {
        # Save specific objects
        objects <- setNames(lapply(list, function(name) get(name, envir = envir)), list)
        qs2::qs_save(objects, file, compress_level = compress_level, nthreads = nthreads)
      }

      if (verbose) {
        file_size <- file.size(file)
        compression_ratio <- if (exists("total_size")) {
          round(as.numeric(total_size) / file_size, 1)
        } else {
          NA
        }
        cat("\u2705 \u4fdd\u5b58\u5b8c\u6210\uff01\n")
        cat("\U0001f4c1 \u6587\u4ef6\u5927\u5c0f:", round(file_size / 1024^2, 1), "MB")
        if (!is.na(compression_ratio)) cat(" (\u538b\u7f29\u6bd4: ", compression_ratio, "x)", sep = "")
        cat("\n")
      }

      # Clean workspace if requested
      if (clean_after) {
        if (verbose) cat("\U0001f9f9 \u6b63\u5728\u6e05\u7406\u5de5\u4f5c\u7a7a\u95f4...\n")

        if (exists("workspace_list", inherits = FALSE)) rm(workspace_list)
        if (exists("objects", inherits = FALSE)) rm(objects)
        if (exists("total_size", inherits = FALSE)) rm(total_size)

        rm(list = ls(all.names = TRUE, envir = envir), envir = envir)
        invisible(gc())
        invisible(gc())

        if (verbose) {
          cat("\u2728 \u73af\u5883\u5df2\u6e05\u7406\uff01\n")
          cat("\U0001f4dd \u6062\u590d\u547d\u4ee4: load_compressed_qs('", file, "')\n", sep = "")
        }
      }

      invisible(list(file = file, objects = list))
    },
    error = function(e) {
      stop("\u4fdd\u5b58\u5931\u8d25: ", e$message)
    }
  )
}

#' Load R Objects from qs2/qs Compressed File
#'
#' Load objects saved by save_compressed_qs. Supports both new .qs2 format
#' and legacy .qs format (requires legacy qs package for .qs files).
#' Auto-detects latest backup if no file specified.
#'
#' @param file File path to load from. If NULL, loads latest kyplus workspace backup
#' @param envir Environment to load objects into (default: .GlobalEnv)
#' @param verbose Whether to show progress information (default: TRUE)
#' @param nthreads Number of threads for decompression (default: parallel::detectCores() - 1)
#' @param pattern File pattern for auto-detection (default: searches both .qs2 and .qs files)
#' @param path Directory to search for backup files (default: ".")
#'
#' @return Invisibly returns names of loaded objects
#' @export
#'
#' @examples
#' \dontrun{
#' # Load specific file (qs2 format)
#' load_compressed_qs("my_data.qs2")
#'
#' # Load legacy .qs file (backward compatible)
#' load_compressed_qs("old_data.qs")
#'
#' # Auto-load latest backup (next-day workflow)
#' load_compressed_qs()
#'
#' # Load into different environment
#' my_env <- new.env()
#' load_compressed_qs("data.qs2", envir = my_env)
#' }
#'
load_compressed_qs <- function(file = NULL, envir = .GlobalEnv, verbose = TRUE,
                               pattern = "kyplus_workspace_.*\\.(qs2|qs)$", path = ".",
                               nthreads = parallel::detectCores() - 1) {
  # Auto-detect latest backup
  if (is.null(file)) {
    backup_files <- list.files(path, pattern = pattern, full.names = TRUE)

    if (length(backup_files) == 0) {
      stop("\u672a\u627e\u5230\u5de5\u4f5c\u7a7a\u95f4\u5907\u4efd\u6587\u4ef6\u3002\u4f7f\u7528 save_compressed_qs() \u521b\u5efa\u5907\u4efd\u3002")
    }

    # Prefer .qs2 over .qs if same timestamp
    file_info <- file.info(backup_files)
    file <- backup_files[which.max(file_info$mtime)]

    if (verbose) {
      cat("\U0001f50d \u81ea\u52a8\u9009\u62e9\u6700\u65b0\u5907\u4efd:", basename(file), "\n")
      cat("\U0001f555 \u521b\u5efa\u65f6\u95f4:", format(file_info$mtime[which.max(file_info$mtime)], "%Y-%m-%d %H:%M:%S"), "\n")
    }
  }

  if (!file.exists(file)) {
    stop("\u6587\u4ef6\u4e0d\u5b58\u5728: ", file)
  }

  # Detect file format
  is_legacy_qs <- grepl("\\.qs$", file) && !grepl("\\.qs2$", file)

  if (is_legacy_qs) {
    # Legacy .qs format - need old qs package
    if (!requireNamespace("qs", quietly = TRUE)) {
      stop(
        "\u8be5\u6587\u4ef6\u4f7f\u7528\u65e7\u7248 .qs \u683c\u5f0f\u3002\u9700\u8981\u5b89\u88c5\u65e7\u7248 qs \u5305\u6765\u8bfb\u53d6\u3002\n",
        "  \u5b89\u88c5: remotes::install_github('traversc/qs')\n",
        "  \u5efa\u8bae: \u8bfb\u53d6\u540e\u7528 save_compressed_qs() \u91cd\u65b0\u4fdd\u5b58\u4e3a .qs2 \u683c\u5f0f\u3002"
      )
    }
    if (verbose) {
      message("\u26a0\ufe0f  \u68c0\u6d4b\u5230\u65e7\u7248 .qs \u683c\u5f0f\u6587\u4ef6\uff0c\u6b63\u5728\u4f7f\u7528\u65e7\u7248 qs \u5305\u8bfb\u53d6...")
      message("   \u5efa\u8bae: \u52a0\u8f7d\u540e\u4f7f\u7528 save_compressed_qs() \u91cd\u65b0\u4fdd\u5b58\u4e3a .qs2 \u683c\u5f0f\u4ee5\u83b7\u5f97\u66f4\u597d\u7684\u517c\u5bb9\u6027\u3002")
    }
    read_fn <- function(f, nt) qs::qread(f, nthreads = nt)
  } else {
    # New .qs2 format
    if (!requireNamespace("qs2", quietly = TRUE)) {
      stop("qs2 package is required. Install with: install.packages('qs2')")
    }
    read_fn <- function(f, nt) qs2::qs_read(f, nthreads = nt)
  }

  if (verbose) {
    cat("\U0001f4c2 \u6b63\u5728\u52a0\u8f7d:", basename(file), "\n")
    file_size <- file.size(file)
    cat("\U0001f4c1 \u6587\u4ef6\u5927\u5c0f:", round(file_size / 1024^2, 1), "MB\n")
    if (is_legacy_qs) cat("\U0001f4e6 \u683c\u5f0f: \u65e7\u7248 qs (\u5efa\u8bae\u8fc1\u79fb\u5230 qs2)\n")
    else cat("\U0001f4e6 \u683c\u5f0f: qs2\n")
  }

  # Load data
  tryCatch(
    {
      data <- read_fn(file, nthreads)

      if (is.list(data) && !is.null(names(data))) {
        if (verbose) cat("\U0001f504 \u6b63\u5728\u6062\u590d", length(data), "\u4e2a\u5bf9\u8c61...\n")

        list2env(data, envir = envir)
        object_names <- names(data)

        if (verbose) {
          total_size <- sum(sapply(object_names, function(x) {
            as.numeric(object.size(get(x, envir = envir)))
          }))
          cat("\u2705 \u6062\u590d\u5b8c\u6210\uff01\n")
          cat("\U0001f4ca \u5bf9\u8c61\u6570\u91cf:", length(object_names), "\n")
          cat("\U0001f4be \u5185\u5b58\u5360\u7528:", format(structure(total_size, class = "object_size"), units = "MB"), "\n")
        }
      } else {
        if (verbose) cat("\U0001f504 \u6b63\u5728\u6062\u590d 1 \u4e2a\u5bf9\u8c61...\n")

        obj_name <- tools::file_path_sans_ext(basename(file))
        obj_name <- gsub("^kyplus_workspace_.*", "restored_workspace", obj_name)

        assign(obj_name, data, envir = envir)
        object_names <- obj_name

        if (verbose) {
          cat("\u2705 \u6062\u590d\u5b8c\u6210\uff01\n")
          cat("\U0001f4ca \u5bf9\u8c61\u6570\u91cf: 1\n")
          cat("\U0001f4be \u5185\u5b58\u5360\u7528:", format(object.size(data), units = "MB"), "\n")
        }
      }

      # Recommend migration if loaded from legacy format
      if (is_legacy_qs && verbose) {
        message("\n\U0001f4a1 \u5efa\u8bae\u8fc1\u79fb\u5230 qs2 \u683c\u5f0f:")
        message("   save_compressed_qs(file = '", sub("\\.qs$", ".qs2", basename(file)), "')")
        message("   qs2 \u683c\u5f0f\u66f4\u5feb\u3001\u66f4\u7a33\u5b9a\uff0c\u4e14\u53ef\u8f6c\u6362\u4e3a\u6807\u51c6 RDS \u683c\u5f0f\u3002")
      }

      if (verbose) {
        today <- format(Sys.Date(), "%Y\u5e74%m\u6708%d\u65e5")
        cat("\U0001f680 \u4eca\u5929\u7684\u65e5\u671f\u662f", today, "\uff0c\u7ee7\u7eed\u4f60\u7684\u5206\u6790\u5427\uff01\n")
      }

      invisible(object_names)
    },
    error = function(e) {
      stop("\u52a0\u8f7d\u5931\u8d25: ", e$message)
    }
  )
}

#' Convert Legacy .qs Files to .qs2 Format
#'
#' Batch convert old .qs files to the new .qs2 format.
#' Requires both qs and qs2 packages to be installed.
#'
#' @param files Character vector of .qs file paths. If NULL, finds all .qs files in path.
#' @param path Directory to search for .qs files (default: ".")
#' @param delete_old Whether to delete old .qs files after successful conversion (default: FALSE)
#' @param verbose Show progress (default: TRUE)
#' @param nthreads Number of threads (default: cores - 1)
#'
#' @return Invisibly returns data.frame of conversion results
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert all .qs files in current directory
#' convert_qs_to_qs2()
#'
#' # Convert specific files and delete originals
#' convert_qs_to_qs2(files = c("data1.qs", "data2.qs"), delete_old = TRUE)
#' }
convert_qs_to_qs2 <- function(files = NULL, path = ".", delete_old = FALSE,
                                verbose = TRUE, nthreads = parallel::detectCores() - 1) {
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("qs package is required to read legacy files. Install with: remotes::install_github('traversc/qs')")
  }
  if (!requireNamespace("qs2", quietly = TRUE)) {
    stop("qs2 package is required. Install with: install.packages('qs2')")
  }

  if (is.null(files)) {
    files <- list.files(path, pattern = "\\.qs$", full.names = TRUE)
    # Exclude .qs2 files
    files <- files[!grepl("\\.qs2$", files)]
  }

  if (length(files) == 0) {
    if (verbose) cat("\u2139\ufe0f  \u672a\u627e\u5230 .qs \u6587\u4ef6\u3002\n")
    return(invisible(data.frame()))
  }

  if (verbose) cat("\U0001f504 \u51c6\u5907\u8f6c\u6362", length(files), "\u4e2a .qs \u6587\u4ef6\u5230 .qs2 \u683c\u5f0f...\n")

  results <- data.frame(
    file = character(), new_file = character(),
    status = character(), stringsAsFactors = FALSE
  )

  for (f in files) {
    new_file <- sub("\\.qs$", ".qs2", f)
    tryCatch({
      data <- qs::qread(f, nthreads = nthreads)
      qs2::qs_save(data, new_file, nthreads = nthreads)

      if (verbose) cat("\u2705", basename(f), "->", basename(new_file), "\n")

      if (delete_old) {
        file.remove(f)
        if (verbose) cat("   \U0001f5d1\ufe0f  \u5df2\u5220\u9664\u539f\u6587\u4ef6\n")
      }

      results <- rbind(results, data.frame(
        file = f, new_file = new_file, status = "success",
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      if (verbose) cat("\u274c", basename(f), "- \u8f6c\u6362\u5931\u8d25:", e$message, "\n")
      results <<- rbind(results, data.frame(
        file = f, new_file = NA, status = paste("error:", e$message),
        stringsAsFactors = FALSE
      ))
    })
  }

  if (verbose) {
    n_ok <- sum(results$status == "success")
    cat("\n\U0001f3c1 \u8f6c\u6362\u5b8c\u6210:", n_ok, "/", nrow(results), "\u4e2a\u6587\u4ef6\u6210\u529f\n")
  }

  invisible(results)
}

#' Save Workspace Before End of Day (Convenience wrapper)
#'
#' Convenient function for end-of-day workflow. Saves entire workspace, cleans environment,
#' and restarts R session by default for a fresh start tomorrow.
#'
#' @param file Custom filename (optional)
#' @param compress_level ZSTD compression level (default: 3)
#' @param restart_session Whether to restart R session after saving (default: TRUE)
#' @param verbose Show progress (default: TRUE)
#' @param nthreads Number of threads for compression (default: cores - 1)
#'
#' @export
save_end_day <- function(file = NULL, compress_level = 3L, restart_session = TRUE,
                         verbose = TRUE, nthreads = parallel::detectCores() - 1) {
  # Save workspace
  result <- save_compressed_qs(
    file = file, compress_level = compress_level, clean_after = TRUE,
    verbose = verbose, nthreads = nthreads
  )

  # Restart R session
  if (restart_session) {
    if (verbose) {
      cat("\U0001f504 \u51c6\u5907\u9000\u51fa\u5e76\u91cd\u542f R session...\n")
      cat("\U0001f4a1 \u660e\u5929\u4f7f\u7528 load_start_day() \u6062\u590d\u5de5\u4f5c\u73af\u5883\n")
      cat("\u26a1 \u4f7f\u7528 q(save = 'no') \u5feb\u901f\u91cd\u542f\uff0c\u4e0d\u4fdd\u5b58\u5f53\u524d\u73af\u5883\n")
      Sys.sleep(1)
    }

    q(save = "no")
  }
  invisible(result)
}

#' Load Workspace at Start of Day (Convenience wrapper)
#'
#' Convenient function for start-of-day workflow. Auto-loads latest workspace backup.
#' Supports both .qs2 and legacy .qs files.
#'
#' @param verbose Show progress (default: TRUE)
#' @param nthreads Number of threads for decompression (default: cores - 1)
#'
#' @export
load_start_day <- function(verbose = TRUE, nthreads = parallel::detectCores() - 1) {
  load_compressed_qs(verbose = verbose, nthreads = nthreads)
}
