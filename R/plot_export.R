#' Multi-format Plot Export Function
#'
#' Export ggplot objects to multiple formats (PDF, JPG, and optionally PPT) with different quality settings.
#' This function provides a unified interface for exporting plots with consistent quality settings.
#'
#' @param plot_object A ggplot object to export
#' @param filename Base filename (without extension) for the exported files
#' @param quality Quality level for export. One of "low", "medium", "high". Default is "medium".
#'   \itemize{
#'     \item low: PDF(150DPI), JPG(100DPI, 70% quality) - Quick preview, small files
#'     \item medium: PDF(300DPI), JPG(200DPI, 85% quality) - Standard quality, balanced size
#'     \item high: PDF(600DPI), JPG(300DPI, 95% quality) - Publication quality, large files
#'   }
#' @param export_ppt Logical, whether to export PPT format. Default is FALSE. Requires 'export' package.
#' @param width Plot width in specified units. Default is 11.69 (A4 landscape width).
#' @param height Plot height in specified units. Default is 8.27 (A4 landscape height).
#' @param units Units for width and height. Default is "in" (inches).
#' @param bg Background color for the plot. Default is "white".
#' @param verbose Logical, whether to print export progress. Default is TRUE.
#'
#' @return Invisibly returns a character vector of successfully exported file paths.
#'
#' @examples
#' \dontrun{
#' # Basic usage - exports PDF and JPG with medium quality
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point()
#' plot_export(p, "my_plot")
#'
#' # Different quality levels
#' plot_export(p, "preview_plot", quality = "low") # Quick preview
#' plot_export(p, "publication_plot", quality = "high") # Publication quality
#'
#' # Include PPT export (requires 'export' package)
#' plot_export(p, "presentation_plot", export_ppt = TRUE)
#'
#' # Custom dimensions
#' plot_export(p, "custom_plot", width = 8, height = 6)
#' }
#'
#' @seealso \code{\link{batch_export}}, \code{\link{show_export_help}}
#'
#' @export
plot_export <- function(plot_object,
                        filename,
                        quality = "medium",
                        export_ppt = FALSE,
                        width = 11.69,
                        height = 8.27,
                        units = "in",
                        bg = "white",
                        verbose = TRUE) {
  # Input validation
  if (!inherits(plot_object, "ggplot")) {
    stop("plot_object must be a ggplot object")
  }

  if (!quality %in% c("low", "medium", "high")) {
    stop("quality must be one of: 'low', 'medium', 'high'")
  }

  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required but not installed")
  }

  if (export_ppt && !requireNamespace("export", quietly = TRUE)) {
    warning("export package is required for PPT export but not installed. Skipping PPT export.")
    export_ppt <- FALSE
  }

  # Quality settings
  quality_settings <- switch(quality,
    "low" = list(
      pdf_dpi = 150,
      jpg_dpi = 100,
      jpg_quality = 70
    ),
    "medium" = list(
      pdf_dpi = 300,
      jpg_dpi = 200,
      jpg_quality = 85
    ),
    "high" = list(
      pdf_dpi = 600,
      jpg_dpi = 300,
      jpg_quality = 95
    )
  )

  # Process filename
  base_filename <- tools::file_path_sans_ext(filename)
  exported_files <- character()

  if (verbose) cat("导出质量:", quality, "\n")

  # Export PDF
  pdf_file <- paste0(base_filename, ".pdf")
  tryCatch(
    {
      ggplot2::ggsave(pdf_file,
        plot = plot_object,
        width = width,
        height = height,
        units = units,
        dpi = quality_settings$pdf_dpi,
        bg = bg,
        device = "pdf"
      )
      exported_files <- c(exported_files, pdf_file)
      if (verbose) cat("✓ PDF saved:", pdf_file, "(", quality_settings$pdf_dpi, "DPI )\n")
    },
    error = function(e) {
      if (verbose) cat("✗ PDF export failed:", e$message, "\n")
    }
  )

  # Export JPG
  jpg_file <- paste0(base_filename, ".jpg")
  tryCatch(
    {
      ggplot2::ggsave(jpg_file,
        plot = plot_object,
        width = width,
        height = height,
        units = units,
        dpi = quality_settings$jpg_dpi,
        bg = bg,
        device = "jpeg",
        quality = quality_settings$jpg_quality
      )
      exported_files <- c(exported_files, jpg_file)
      if (verbose) cat("✓ JPG saved:", jpg_file, "(", quality_settings$jpg_dpi, "DPI,", quality_settings$jpg_quality, "% quality )\n")
    },
    error = function(e) {
      if (verbose) cat("✗ JPG export failed:", e$message, "\n")
    }
  )

  # Export PPT (optional)
  if (export_ppt) {
    ppt_file <- paste0(base_filename, ".pptx")

    tryCatch(
      {
        export::graph2ppt(
          x = plot_object,
          file = ppt_file,
          width = width,
          height = height,
          bg = bg
        )
        exported_files <- c(exported_files, ppt_file)
        if (verbose) cat("✓ PPT saved:", ppt_file, "\n")
      },
      error = function(e) {
        if (verbose) cat("✗ PPT export failed:", e$message, "\n")
        if (verbose) {
          if (grepl("memory|allocate", e$message, ignore.case = TRUE)) {
            cat("  💡 建议: 图片太大导致内存不足，尝试:\n")
            cat("     • 使用 quality='low' 参数\n")
            cat("     • 或创建图时设置 raster=TRUE\n")
            cat("     • 或减少基因数量\n")
          } else {
            cat("  提示: 确保PowerPoint已安装并可访问\n")
          }
        }
      }
    )
  }

  # Display results
  if (verbose) {
    cat("\n🎉 导出完成! 共", length(exported_files), "个文件:\n")
    for (file in exported_files) {
      if (file.exists(file)) {
        file_size <- file.size(file)
        size_mb <- round(file_size / (1024^2), 2)
        cat("  📁", file, "(", size_mb, "MB )\n")
      }
    }

    if (!export_ppt) {
      cat("\n💡 提示: 需要PPT格式请添加 export_ppt = TRUE\n")
    }

    cat("\n📝 文件大小提示:\n")
    cat("   • 如果PDF文件过大(>50MB)，建议创建图时使用 raster=TRUE\n")
    cat("   • raster=TRUE: 小文件，快速渲染，适合演示\n")
    cat("   • raster=FALSE: 大文件，矢量图，适合发表\n")
  }

  invisible(exported_files)
}


#' Batch export multiple plots
#'
#' Export multiple ggplot objects with the same settings.
#'
#' @param plot_list List of ggplot objects to export
#' @param filename_list Character vector of filenames (same length as plot_list)
#' @param quality Quality level for all exports
#' @param export_ppt Whether to export PPT format for all plots
#' @param ... Additional arguments passed to \code{plot_export}
#'
#' @return Invisibly returns a list of exported file paths for each plot.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point()
#' p2 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
#'   geom_point()
#'
#' # Batch export with medium quality
#' batch_export(list(p1, p2), c("plot1", "plot2"), quality = "medium")
#'
#' # Batch export with high quality
#' batch_export(list(p1, p2), c("plot1", "plot2"), quality = "high")
#' }
#'
#' @export
batch_export <- function(plot_list, filename_list, quality = "medium", export_ppt = FALSE, ...) {
  if (length(plot_list) != length(filename_list)) {
    stop("plot_list and filename_list must have the same length")
  }

  cat("批量导出", length(plot_list), "个图...\n\n")

  exported_all <- list()
  for (i in seq_along(plot_list)) {
    cat("=== 导出第", i, "个图:", filename_list[i], "===\n")
    exported_all[[i]] <- plot_export(plot_list[[i]], filename_list[i],
      quality = quality, export_ppt = export_ppt, ...
    )
    cat("\n")
  }

  cat("🎊 批量导出完成!\n")
  invisible(exported_all)
}

#' Show plot export help
#'
#' Display usage guide for plot export functions.
#'
#' @export
show_export_help <- function() {
  cat("📋 kyplus::plot_export 使用指南:\n\n")
  cat("🔸 基础用法:\n")
  cat("   plot_export(plot_object, 'filename')                    # 默认中等质量\n")
  cat("   plot_export(plot_object, 'filename', quality = 'low')   # 低质量(小文件)\n")
  cat("   plot_export(plot_object, 'filename', quality = 'high')  # 高质量(大文件)\n\n")

  cat("🔸 包含PPT:\n")
  cat("   plot_export(plot_object, 'filename', export_ppt = TRUE)\n\n")

  cat("🔸 批量导出:\n")
  cat("   batch_export(plot_list, filename_list, quality = 'medium')\n\n")

  cat("🔸 通用保存 (支持 pheatmap / ComplexHeatmap / ggplot / 当前窗口):\n")
  cat("   save_any_plot(plot_obj, 'output.pdf')  # 自动检测尺寸\n")
  cat("   save_any_plot(NULL, 'output.png')       # 保存当前窗口\n\n")

  cat("🔸 质量设置说明:\n")
  cat("   • low    : PDF(150DPI), JPG(100DPI, 70%)  - 快速预览\n")
  cat("   • medium : PDF(300DPI), JPG(200DPI, 85%)  - 标准质量\n")
  cat("   • high   : PDF(600DPI), JPG(300DPI, 95%)  - 发表质量\n\n")

  cat("📝 默认导出 PDF + JPG 双格式\n")
  cat("📝 A4横排尺寸 (11.69 × 8.27 英寸)\n")
  cat("📝 PPT导出需要安装 'export' 包\n")
}


#' Universal Plot Saver with Auto Size Detection
#'
#' @description
#' Saves any plot object (ggplot2, pheatmap, ComplexHeatmap, recordedplot,
#' or base R) to file with automatic size detection. When \code{plot_obj}
#' is \code{NULL}, captures the current graphics device window directly
#' via \code{dev.copy()}, which is useful for plots that cannot be saved
#' through standard \code{print()} or \code{grid.draw()} methods.
#'
#' @param plot_obj Plot object, or \code{NULL} to capture the current
#'   graphics device. Supports:
#'   \itemize{
#'     \item \code{NULL} — captures current device window via dev.copy()
#'     \item \code{recordedplot} — replays a plot recorded by recordPlot()
#'     \item \code{pheatmap} — auto-detects width/height from gtable
#'     \item \code{ComplexHeatmap::Heatmap} — auto-detects after draw()
#'     \item \code{ggplot2::ggplot} — uses width_base/height_base
#'     \item Base R or other — uses width_base/height_base
#'   }
#' @param filename Character. Output file path. Supported extensions:
#'   \code{.pdf}, \code{.png}, \code{.svg}, \code{.tiff}, \code{.jpg}.
#' @param width_base Numeric. Base width in inches (used for ggplot/base R
#'   and as default for device capture). Default is 7.
#' @param height_base Numeric. Base height in inches (used for ggplot/base R
#'   and as default for device capture). Default is 7.
#' @param res Integer. Resolution in DPI for raster formats (png, tiff, jpg).
#'   Default is 300.
#' @param ... Additional arguments passed to the graphics device function.
#'
#' @return Invisibly returns a list with the filename and dimensions used.
#'
#' @details
#' Size detection logic:
#' \itemize{
#'   \item \strong{NULL (device capture)}: Uses \code{dev.copy()} to copy
#'     the current graphics device content to the target file. The source
#'     device remains open. Uses \code{width_base}/\code{height_base}.
#'   \item \strong{recordedplot}: Replays the recorded plot via
#'     \code{replayPlot()} into a new device.
#'   \item \strong{pheatmap}: Extracts dimensions from the internal gtable
#'     using \code{grid::convertWidth/convertHeight}, adds 1 inch margin.
#'   \item \strong{ComplexHeatmap}: Calls \code{draw()} first to compute
#'     layout, then extracts dimensions, adds 1 inch margin.
#'   \item \strong{ggplot2 / other}: Uses \code{width_base} and
#'     \code{height_base} directly.
#' }
#'
#' @examples
#' \dontrun{
#' # Save the current device window (any plot already displayed)
#' plot(1:10)
#' save_any_plot(NULL, "current_window.pdf")
#'
#' # Save a recorded plot
#' plot(1:10, main = "My Plot")
#' rec <- recordPlot()
#' save_any_plot(rec, "recorded.png")
#'
#' # Save pheatmap (auto size)
#' library(pheatmap)
#' p <- pheatmap(matrix(rnorm(100), 10),
#'   cellwidth = 20, cellheight = 20, silent = TRUE
#' )
#' save_any_plot(p, "auto_pheatmap.pdf")
#'
#' # Save ggplot2
#' library(ggplot2)
#' g <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#' save_any_plot(g, "my_ggplot.png", width_base = 8, height_base = 6)
#'
#' # Save ComplexHeatmap
#' library(ComplexHeatmap)
#' ht <- Heatmap(matrix(rnorm(100), 10))
#' save_any_plot(ht, "complex_heatmap.pdf")
#' }
#'
#' @seealso \code{\link{plot_export}}, \code{\link{batch_export}}
#'
#' @export
save_any_plot <- function(plot_obj = NULL, filename, width_base = 7, height_base = 7,
                          res = 300, ...) {
  # Get file extension
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("pdf", "png", "svg", "tiff", "jpg", "jpeg")) {
    stop("Unsupported format: .", ext,
      ". Use: pdf, png, svg, tiff, or jpg.",
      call. = FALSE
    )
  }

  # --- Ensure output directory exists ---
  dir_path <- dirname(filename)
  if (nchar(dir_path) > 0 && dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  w <- width_base
  h <- height_base

  # --- NULL: capture current device via dev.copy() ---
  if (is.null(plot_obj)) {
    if (grDevices::dev.cur() == 1L) {
      stop("No active graphics device to capture. ",
        "Please create a plot first, or pass a plot object.",
        call. = FALSE
      )
    }

    # Auto-detect current device dimensions
    dev_dim <- grDevices::dev.size("in")
    w <- dev_dim[1]
    h <- dev_dim[2]

    tryCatch(
      {
        # Use dev.copy to copy the current device content
        switch(ext,
          "pdf"  = grDevices::dev.copy2pdf(file = filename, width = w, height = h),
          "svg"  = grDevices::dev.copy(grDevices::svg,
            filename = filename, width = w, height = h
          ),
          "png"  = grDevices::dev.copy(grDevices::png,
            filename = filename, width = w, height = h,
            units = "in", res = res
          ),
          "tiff" = grDevices::dev.copy(grDevices::tiff,
            filename = filename, width = w, height = h,
            units = "in", res = res
          ),
          "jpg"  = ,
          "jpeg" = grDevices::dev.copy(grDevices::jpeg,
            filename = filename, width = w, height = h,
            units = "in", res = res
          )
        )
        # dev.copy opens a new device; close it to flush the file
        if (ext != "pdf") {
          grDevices::dev.off()
        }
      },
      error = function(e) {
        stop("Device capture failed: ", e$message, call. = FALSE)
      }
    )

    message(sprintf(
      "Saved (device capture): %s (%.2f x %.2f in)", filename, w, h
    ))
    return(invisible(list(filename = filename, width = w, height = h)))
  }

  # --- recordedplot: replay into a new device ---
  if (inherits(plot_obj, "recordedplot")) {
    switch(ext,
      "pdf" = grDevices::pdf(filename, width = w, height = h, ...),
      "svg" = grDevices::svg(filename, width = w, height = h, ...),
      "png" = grDevices::png(filename,
        width = w, height = h,
        units = "in", res = res, ...
      ),
      "tiff" = grDevices::tiff(filename,
        width = w, height = h,
        units = "in", res = res, ...
      ),
      "jpg" = ,
      "jpeg" = grDevices::jpeg(filename,
        width = w, height = h,
        units = "in", res = res, ...
      )
    )
    tryCatch(
      {
        grDevices::replayPlot(plot_obj)
      },
      error = function(e) {
        grDevices::dev.off()
        stop("Replay recorded plot failed: ", e$message, call. = FALSE)
      }
    )
    grDevices::dev.off()
    message(sprintf(
      "Saved (recordedplot): %s (%.2f x %.2f in)", filename, w, h
    ))
    return(invisible(list(filename = filename, width = w, height = h)))
  }

  # --- Detect plot type and calculate dimensions ---
  if (inherits(plot_obj, "pheatmap")) {
    # ---- pheatmap: extract size from gtable ----
    w <- grid::convertWidth(
      sum(plot_obj$gtable$widths), "inches",
      valueOnly = TRUE
    ) + 1
    h <- grid::convertHeight(
      sum(plot_obj$gtable$heights), "inches",
      valueOnly = TRUE
    ) + 1
    plot_to_draw <- plot_obj$gtable
    draw_method <- "grid"
  } else if (inherits(plot_obj, "Heatmap") ||
    inherits(plot_obj, "HeatmapList")) {
    # ---- ComplexHeatmap: must draw() first to get size ----
    if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      stop("ComplexHeatmap is required.", call. = FALSE)
    }
    plot_obj <- ComplexHeatmap::draw(plot_obj)
    w <- grid::convertWidth(
      ComplexHeatmap::width(plot_obj), "inches",
      valueOnly = TRUE
    ) + 1
    h <- grid::convertHeight(
      ComplexHeatmap::height(plot_obj), "inches",
      valueOnly = TRUE
    ) + 1
    plot_to_draw <- plot_obj
    draw_method <- "grid"
  } else if (inherits(plot_obj, "ggplot")) {
    # ---- ggplot2: use user-specified size ----
    plot_to_draw <- plot_obj
    draw_method <- "print"
  } else if (inherits(plot_obj, c("grob", "gTree", "gtable"))) {
    # ---- grid grob/gtable objects ----
    plot_to_draw <- plot_obj
    draw_method <- "grid"
  } else {
    # ---- base R or other: use user-specified size ----
    plot_to_draw <- plot_obj
    draw_method <- "print"
  }

  # --- Open graphics device ---
  switch(ext,
    "pdf" = grDevices::pdf(filename, width = w, height = h, ...),
    "svg" = grDevices::svg(filename, width = w, height = h, ...),
    "png" = grDevices::png(filename,
      width = w, height = h,
      units = "in", res = res, ...
    ),
    "tiff" = grDevices::tiff(filename,
      width = w, height = h,
      units = "in", res = res, ...
    ),
    "jpg" = ,
    "jpeg" = grDevices::jpeg(filename,
      width = w, height = h,
      units = "in", res = res, ...
    )
  )

  # --- Draw the plot ---
  tryCatch(
    {
      if (draw_method == "grid") {
        grid::grid.draw(plot_to_draw)
      } else {
        print(plot_to_draw)
      }
    },
    error = function(e) {
      grDevices::dev.off()
      stop("Plot drawing failed: ", e$message, call. = FALSE)
    }
  )

  grDevices::dev.off()
  message(sprintf("Saved: %s (%.2f x %.2f in)", filename, w, h))

  invisible(list(filename = filename, width = w, height = h))
}
