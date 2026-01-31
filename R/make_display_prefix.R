#' @title Generate a formatted prefix for hierarchical display
#'
#' @param level Integer. Indentation level (>= 0).
#'
#' @param type Character. Either `"title"` or `"info"`, defining which
#'   prefix marker to prepend after the indentation.
#'
#' @return A character string containing the indentation and the associated
#'   prefix marker.
#'
#' @details This helper function builds a display prefix combining indentation
#' and a type-specific marker. It is intended to standardize the visual
#' structure of hierarchical output (e.g., steps, sub-steps, information
#' blocks) in the AgMIP calibration protocol display.
#'
#' @examples
#' \donttest{
#' CroptimizR:::make_display_prefix(1, "title") # "-- "
#' CroptimizR:::make_display_prefix(2, "title") # "---- "
#' CroptimizR:::make_display_prefix(2, "info") # "     "
#' }
#'
#' @keywords internal
#'
make_display_prefix <- function(level = 0, type = c("title", "info")) {
  type <- match.arg(type)

  if (type == "title") {
    # "--" * level + " "
    prefix <- paste0(strrep("--", level), " ")
  } else {
    # (2 * level + 1) spaces
    prefix <- strrep(" ", 2 * level + 1)
  }

  prefix
}
