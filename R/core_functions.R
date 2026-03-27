#' Create a new R project structure
#'
#' @param path Character. Path where to create the project
#' @param type Character. Project type: "analysis" or "package"
#' @param create_project_file Logical. Whether to create .Rproj file
#' @return Invisible list containing project information
#' @export
create_project_structure <- function(path,
                                     type = c("analysis", "package"),
                                     create_project_file = TRUE) {
  type <- match.arg(type)

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  if (create_project_file) {
    create_rproj_file(path)
  }

  create_directory_structure(path, type)
  create_base_files(path, type)

  invisible(list(
    path = path,
    type = type,
    created_at = Sys.time()
  ))
}

#' @keywords internal
create_base_files <- function(path, type) {
  # Create README file
  readme_content <- create_readme_content(type)
  writeLines(readme_content, file.path(path, "README.md"))

  # Create config file
  config_content <- create_config_content(type, path)
  yaml::write_yaml(config_content, file.path(path, "config/config.yml"))

  # Create .gitignore
  gitignore_content <- create_gitignore_content(type)
  writeLines(gitignore_content, file.path(path, ".gitignore"))
}

#' @keywords internal
create_readme_content <- function(type) {
  if (type == "analysis") {
    return(paste(
      "# Data Analysis Project\n\n",
      "## Project Overview\n",
      "Brief description of your analysis project.\n\n",
      "## Structure\n",
      "- R/functions: Reusable functions\n",
      "- R/pipelines: Data processing pipelines\n",
      "- data: Data files\n",
      "- docs: Documentation\n",
      "- tests: Tests\n\n",
      "## Setup\n",
      "Instructions for setting up the project.\n",
      sep = ""
    ))
  } else {
    return(paste(
      "# R Package Project\n\n",
      "## Overview\n",
      "Brief description of your R package.\n\n",
      "## Installation\n",
      "```r\n",
      "devtools::install_github('username/packagename')\n",
      "```\n\n",
      "## Usage\n",
      "Basic usage examples.\n",
      sep = ""
    ))
  }
}

#' @keywords internal
create_config_content <- function(type, path) {
  if (type == "analysis") {
    list(
      project = list(
        name = basename(path),
        type = "analysis",
        created = as.character(Sys.time())
      ),
      paths = list(
        data = "data",
        output = "data/output",
        reports = "docs/reports"
      ),
      settings = list(
        cache = TRUE,
        parallel = FALSE
      )
    )
  } else {
    list(
      package = list(
        name = basename(path),
        type = "package",
        created = as.character(Sys.time())
      ),
      development = list(
        test = TRUE,
        document = TRUE,
        check = TRUE
      )
    )
  }
}

#' @keywords internal
create_gitignore_content <- function(type) {
  common_ignores <- c(
    ".Rproj.user/",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "*.log"
  )

  if (type == "analysis") {
    c(common_ignores,
      "data/raw/*",
      "data/processed/*",
      "data/output/*",
      "!data/raw/.gitkeep",
      "!data/processed/.gitkeep",
      "!data/output/.gitkeep"
    )
  } else {
    c(common_ignores,
      "inst/doc",
      "doc",
      "Meta",
      "*.tar.gz",
      "*.Rcheck/"
    )
  }
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
