#' Save R Objects with Multi-threaded Compression
#'
#' @param ... Objects to save, specified as names or character strings
#' @param list Character vector of object names to save
#' @param file Output file path
#' @param threads Number of compression threads to use (default: number of cores - 1)
#' @param compression_level Compression level from 1-9 (default: 6)
#' @param envir Environment to search for objects (default: parent.frame())
#'
#' @return Invisibly returns the names of saved objects
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1:10
#' y <- letters
#' save_compressed(x, y, file = "data.rds.gz")
#' }
#'

save_compressed <- function(..., list = character(), file,
                            threads = (parallel::detectCores()) - 1,
                            compression_level = 6,
                            envir = parent.frame()) {

  # 检查 pigz 是否已安装
  if (Sys.which("pigz") == "") {
    stop("pigz is not installed. Please install it first.")
  }

  # 处理变量名
  if (...length()) {
    dots <- match.call(expand.dots = FALSE)$...
    names <- vapply(dots, function(x) {
      if (is.character(x)) return(x)
      if (is.name(x)) return(as.character(x))
      stop("... must contain names or character strings")
    }, "")
    list <- c(list, names)
  }

  if (length(list) == 0) {
    stop("No objects specified to save")
  }

  missing <- !vapply(list, exists, logical(1), envir = envir)
  if (any(missing)) {
    stop("Objects not found: ", paste(list[missing], collapse = ", "))
  }

  objects <- lapply(list, function(name) get(name, envir = envir))
  names(objects) <- list

  con <- pipe(paste0("pigz -c", compression_level, " -p", threads, " > ", shQuote(file)), "wb")
  on.exit(close(con))

  saveRDS(objects, file = con)
  invisible(list)
}
