#' Initialize Mirai Parallel Environment
#'
#' @description
#' Sets up a mirai parallel computing environment with specified packages,
#' objects, and functions distributed to all daemon processes.
#'
#' @param n_cores Integer. Number of cores to use. Default is `detectCores() - 1`.
#' @param packages Character vector. Names of packages to load in each daemon.
#' @param objects Named list. Objects to export to daemons.
#' @param functions Named list. Functions to export to daemons.
#' @param code Expression or call. Additional code to execute in each daemon.
#' @param verbose Logical. Whether to print initialization messages. Default is TRUE.
#'
#' @return Invisibly returns the number of cores initialized.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' init_mirai(n_cores = 4, packages = c("dplyr", "ggplot2"))
#'
#' # With objects and functions
#' my_func <- function(x) x^2
#' init_mirai(
#'   n_cores = 6,
#'   packages = "Seurat",
#'   objects = list(data = my_data),
#'   functions = list(my_func = my_func)
#' )
#'
#' # With additional initialization code
#' init_mirai(
#'   packages = "data.table",
#'   code = quote({
#'     options(datatable.verbose = FALSE)
#'     set.seed(42)
#'   })
#' )
#' }
#'
#' @export
init_mirai <- function(n_cores = NULL,
                       packages = NULL,
                       objects = list(),
                       functions = list(),
                       code = NULL,
                       verbose = TRUE) {

  # Check if mirai is available
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop("Package 'mirai' is required but not installed. Please install it with: install.packages('mirai')")
  }

  # Determine number of cores
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
    if (is.na(n_cores) || n_cores < 1) n_cores <- 1
  }

  # Validate inputs
  if (!is.numeric(n_cores) || n_cores < 1) {
    stop("n_cores must be a positive integer")
  }

  if (!is.null(packages) && !is.character(packages)) {
    stop("packages must be a character vector")
  }

  if (!is.list(objects)) {
    stop("objects must be a named list")
  }

  if (!is.list(functions)) {
    stop("functions must be a named list")
  }

  # Start daemons
  mirai::daemons(n = n_cores)

  # Build expression to execute
  expr_parts <- list()

  # Add package loading code
  if (!is.null(packages)) {
    pkg_expr <- substitute({
      for (pkg in pkgs) {
        suppressPackageStartupMessages(
          library(pkg, character.only = TRUE, quietly = TRUE)
        )
      }
    }, list(pkgs = packages))
    expr_parts <- c(expr_parts, list(pkg_expr))
  }

  # Add custom code
  if (!is.null(code)) {
    expr_parts <- c(expr_parts, list(code))
  }

  # Combine all expressions
  if (length(expr_parts) > 0) {
    combined_expr <- if (length(expr_parts) == 1) {
      expr_parts[[1]]
    } else {
      as.call(c(as.name("{"), expr_parts))
    }
  } else {
    combined_expr <- quote({})
  }

  # Merge objects and functions for export to global environment
  all_vars <- c(objects, functions)

  # Check for name conflicts
  if (length(all_vars) > 0 && any(duplicated(names(all_vars)))) {
    warning("Duplicate names found in objects/functions. Later definitions will override earlier ones.")
  }

  # Execute everywhere with correct argument structure
  if (length(all_vars) > 0) {
    # Pass expression as .expr and variables as ...
    do.call(mirai::everywhere, c(list(.expr = combined_expr), all_vars))
  } else if (length(expr_parts) > 0) {
    # Only execute expression if no variables to export
    mirai::everywhere(.expr = combined_expr)
  }

  # Print messages
  if (verbose) {
    message("\u2713 Mirai parallel environment initialized with ", n_cores, " core",
            ifelse(n_cores > 1, "s", ""))

    if (!is.null(packages)) {
      message("\u2713 Loaded packages: ", paste(packages, collapse = ", "))
    }

    if (length(all_vars) > 0) {
      obj_count <- length(objects)
      func_count <- length(functions)
      parts <- character()
      if (obj_count > 0) parts <- c(parts, paste(obj_count, "object(s)"))
      if (func_count > 0) parts <- c(parts, paste(func_count, "function(s)"))
      message("\u2713 Exported ", paste(parts, collapse = " and "))
    }
  }

  invisible(n_cores)
}



#' Clean Up Mirai Parallel Environment
#'
#' @description
#' Shuts down all daemon processes and cleans up the mirai environment.
#'
#' @param verbose Logical. Whether to print cleanup message. Default is TRUE.
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' # After using mirai
#' init_mirai(n_cores = 4)
#' # ... do parallel work ...
#' cleanup_mirai()
#' }
#'
#' @export
cleanup_mirai <- function(verbose = TRUE) {

  if (!requireNamespace("mirai", quietly = TRUE)) {
    warning("Package 'mirai' is not available")
    return(invisible(NULL))
  }

  tryCatch({
    mirai::daemons(0)
    if (verbose) {
      message("\u2713 Mirai environment cleaned up")
    }
  }, error = function(e) {
    warning("Error during cleanup: ", e$message)
  })

  invisible(NULL)
}


#' Get Mirai Status
#'
#' @description
#' Returns the current status of the mirai daemon environment.
#'
#' @return A list containing daemon information, or NULL if no daemons are running.
#'
#' @examples
#' \dontrun{
#' init_mirai(n_cores = 4)
#' status <- mirai_status()
#' print(status)
#' cleanup_mirai()
#' }
#'
#' @export
mirai_status <- function() {

  if (!requireNamespace("mirai", quietly = TRUE)) {
    message("Package 'mirai' is not available")
    return(NULL)
  }

  tryCatch({
    status <- mirai::status()
    return(status)
  }, error = function(e) {
    message("No active mirai daemons")
    return(NULL)
  })
}
