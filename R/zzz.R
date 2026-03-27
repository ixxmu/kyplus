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
  # 自动检测核心数并留出1个核心
  n_cores <- max(1, parallel::detectCores() - 1)

  # OpenMP 线程设置
  Sys.setenv(OMP_NUM_THREADS = n_cores)
  Sys.setenv(OMP_THREAD_LIMIT = n_cores)

  # BLAS/LAPACK 数学库线程
  Sys.setenv(MKL_NUM_THREADS = n_cores)
  Sys.setenv(OPENBLAS_NUM_THREADS = n_cores)

  # 并行编译设置
  Sys.setenv(MAKEFLAGS = paste0("-j", n_cores))

  # 添加编译优化 - 修正这部分
  makevars_path <- path.expand("~/.R/Makevars")  # 在函数内定义路径
  if (!file.exists(makevars_path)) {
    dir.create(dirname(makevars_path), showWarnings = FALSE)
    makevars_content <- sprintf("
MAKEFLAGS = -j%d
CXX11FLAGS = -O3
CXX14FLAGS = -O3
CXX17FLAGS = -O3
", n_cores)
    writeLines(makevars_content, makevars_path)
  }

  # 其他设置保持不变...

  # 内存设置 - 系统总内存的90%
  if(Sys.info()[['sysname']] == "Linux") {
    total_mem <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
    r_mem <- floor(total_mem * 0.9)
    options(future.globals.maxSize = r_mem * 1024)
  }

  # 并行计算核心数
  options(mc.cores = n_cores)

  # 内存管理优化
  options(scipen = 10)
  options(max.print = 100)

  # 显示配置信息
  packageStartupMessage("kyplus optimizations activated:")
  packageStartupMessage("- Parallel cores: ", n_cores)
  packageStartupMessage("- System cores: ", parallel::detectCores())
  packageStartupMessage("- Parallel compilation enabled")
  if(Sys.info()[['sysname']] == "Linux") {
    packageStartupMessage("- Available memory: ", system("free -h", intern=TRUE)[2])
  }
}
