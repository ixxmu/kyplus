#' Make Names Unique with Custom Prefix
#'
#' Creates unique names by adding numbered suffixes with custom prefix.
#' For duplicate names, the first occurrence remains unchanged while subsequent
#' occurrences get numbered suffixes.
#'
#' @param names A character vector of names
#' @param sep Separator between name and suffix (default: ".")
#' @param prefix Prefix for the number (default: "M")
#'
#' @return A character vector with unique names
#' @export
#'
#' @examples
#' make_unique_custom(c("MKKS", "MKKS", "MKKS", "ABC"))
#' make_unique_custom(c("MKKS", "MKKS", "ABC"), sep="_", prefix="P")
make_unique_custom <- function(names, sep = ".", prefix = "M") {
  if (!is.character(names)) names <- as.character(names)
  make_unique_cpp(names, sep, prefix)
}
