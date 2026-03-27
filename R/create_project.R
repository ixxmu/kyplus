#' Create a New R Project Structure
#'
#' Creates a new R project structure without interrupting the current R session.
#' This function allows you to set up either an analysis project or a package
#' project with standardized directory structure and configuration files.
#'
#' @param path Character. Path where to create the project
#' @param type Character. Project type: "analysis" or "package"
#' @param create_project_file Logical. Whether to create .Rproj file
#' @return Invisible list containing project information
#' @export
#' @examples
#' \dontrun{
#' create_project_structure("~/Projects/new_analysis", "analysis")
#' create_project_structure("~/Projects/new_package", "package")
#' }
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

#' Load Project Environment
#'
#' Loads a project's environment including its configuration and dependencies.
#'
#' @param path Character. Path to the project
#' @return Invisible list containing project information
#' @export
#' @examples
#' \dontrun{
#' load_project("~/Projects/my_analysis")
#' }
load_project <- function(path) {
  validate_project_path(path)

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  setwd(path)
  load_project_config(path)
  load_project_environment(path)

  invisible(list(
    path = path,
    loaded_at = Sys.time()
  ))
}
