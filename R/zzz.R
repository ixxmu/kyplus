# R/zzz.R

#' @importFrom methods new
#' @importFrom stats as.formula setNames
#' @importFrom utils install.packages installed.packages
#' @importFrom magrittr %>%
#'
#'
#' @useDynLib kyplus, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

.onLoad <- function(libname, pkgname) {
  # 仅记录推荐核心数；避免在加载包时修改用户环境
  n_cores <- max(1L, parallel::detectCores(logical = TRUE) - 1L)

  assign("recommended_cores", n_cores, envir = .kyplus_env)

  # 提供包级默认值，用户可显式覆盖
  if (is.null(getOption("kyplus.n_cores"))) {
    options(kyplus.n_cores = n_cores)
  }

  packageStartupMessage("kyplus loaded.")
  packageStartupMessage(
    "Recommended cores: ", n_cores,
    " (use setup_cpu_cores() / init_mirai() to apply)"
  )
}
