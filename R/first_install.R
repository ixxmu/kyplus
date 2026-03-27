#' Set up optimal CPU cores for package installation
#'
#' @description
#' Configures R to use optimal number of CPU cores for parallel package installation
#' @return Number of cores that will be used for installation
#' @export
setup_cpu_cores <- function() {
  old_cores <- getOption("Ncpus", 1L)
  available_cores <- parallel::detectCores()
  recommended_cores <- max(1, available_cores - 1)
  options(Ncpus = recommended_cores)
  return(recommended_cores)
}

#' Install essential development packages
#'
#' @description
#' Installs and loads fundamental packages required for R development
#' @param quiet Logical. Whether to suppress messages during installation
#' @return Invisible TRUE if successful
#' @export
install_essential_packages <- function(quiet = FALSE) {
  essentials <- c("devtools","pak","BiocManager","remotes")
  install_if_missing(essentials, quiet = quiet)

  # Install wget from GitHub
  if (!quiet) message("Installing wget package from GitHub...")
  tryCatch({
    install.packages("wget", quiet = quiet)
    library(wget)
    library(pak)
  }, error = function(e) {
    warning("Failed to install wget package. Some installations may be slower.")
  })

  invisible(TRUE)
}

#' Install comprehensive set of R packages
#'
#' @description
#' Installs a curated set of packages from CRAN, Bioconductor, and GitHub
#' @param categories Character vector specifying which categories to install:
#' "all", "cran", "bioc", "github", or any combination
#' @param quiet Logical. Whether to suppress messages during installation
#' @export
install_package_suite <- function(categories = "all", quiet = FALSE) {
  # Define package sets
  cran_packages <- c(
    "tidyverse", "data.table", "devtools", "shiny", "remotes",
    "shinydashboard", "blogdown", "survival", "survminer",
    "Seurat", "rlang", "UCSCXenaTools", "maftools", "tidymodels",
    "caret", "ggThemeAssist", "ggplot2", "ggsci", "paletteer",
    "scico", "nord", "reshape"
  )

  bioc_packages <- c(
    "clusterProfiler", "genefilter", "impute", "annotate",
    "org.Hs.eg.db", "Biobase", "limma", "GEOquery", "edgeR"
  )

  github_packages <- c(
    "jmzeng1314/AnnoProbe", "XSLiuLab/GSVAutils",
    "icbi-lab/immunedeconv", "jmzeng1314/humanid",
    "xjsun1221/tinyarray", "tomwenseleers/export",
    "krumsieklab/SurvRank", "immunogenomics/presto"
  )

  # Process installation based on categories
  if ("all" %in% categories || "cran" %in% categories) {
    if (!quiet) message("Installing CRAN packages...")
    install_cran_packages(cran_packages, quiet = quiet)
  }

  if ("all" %in% categories || "bioc" %in% categories) {
    if (!quiet) message("Installing Bioconductor packages...")
    install_bioc_packages(bioc_packages, quiet = quiet)
  }

  if ("all" %in% categories || "github" %in% categories) {
    if (!quiet) message("Installing GitHub packages...")
    install_github_packages(github_packages, quiet = quiet)
  }

  invisible(TRUE)
}

#' Install CRAN packages if not already installed
#' @keywords internal
install_cran_packages <- function(packages, quiet = FALSE) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE, quiet = quiet)
  }
  invisible(sapply(packages, require, character.only = TRUE))
}

#' Install Bioconductor packages if not already installed
#' @keywords internal
install_bioc_packages <- function(packages, quiet = FALSE) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", quiet = quiet)
  }

  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    BiocManager::install(new_packages, ask = FALSE, quiet = quiet)
  }
  invisible(sapply(packages, require, character.only = TRUE))
}

#' Install GitHub packages if not already installed
#' @keywords internal
install_github_packages <- function(packages, quiet = FALSE) {
  new_packages <- packages[!(basename(packages) %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    sapply(new_packages, function(pkg) {
      tryCatch({
        remotes::install_github(pkg, force = TRUE, quiet = quiet)
      }, error = function(e) {
        warning(sprintf("Failed to install %s: %s", pkg, e$message))
      })
    })
  }
  invisible(sapply(basename(packages), require, character.only = TRUE))
}
