#' Get example AgMIP protocol Excel file
#'
#' Returns the path to the example AgMIP calibration protocol Excel file
#' shipped with CroptimizR, or copies it to a user-specified location.
#'
#' @param path Optional path where to copy the example file. If NULL (default),
#'   only returns the path to the file inside the package.
#' @param overwrite Logical. Overwrite existing file?
#'
#' @return Path to the example Excel file (either inside the package or to the
#'   copied file).
#' @export
get_agmip_protocol_example <- function(path = NULL, overwrite = FALSE) {
  src <- system.file(
    "extdata",
    "agmip_protocol_example.xlsx",
    package = "CroptimizR"
  )

  if (src == "") {
    stop("Example protocol file not found in CroptimizR installation.")
  }

  # Only return the internal path
  if (is.null(path)) {
    return(src)
  }

  dest <- file.path(path, "agmip_protocol_example.xlsx")

  if (file.exists(dest) && !overwrite) {
    stop("File already exists: ", dest, "\nUse overwrite = TRUE to replace it.")
  }

  ok <- file.copy(src, dest, overwrite = overwrite)
  if (!ok) {
    stop("Failed to copy example protocol to: ", dest)
  }

  message("Example protocol copied to: ", normalizePath(dest))

  normalizePath(dest)
}

#' Get AgMIP protocol Excel template
#'
#' Returns the path to the AgMIP calibration protocol Excel template shipped
#' with CroptimizR, and copies it to a user-specified location.
#'
#' @param path Path where to copy the template file, or NULL to only return
#'   the path to the file inside the package. Defaults to the current working
#'   directory.
#' @param overwrite Logical. Overwrite existing file?
#'
#' @return The path to the template file (either inside the package or to the
#'   copied file).
#' @export
get_agmip_protocol_template <- function(path = ".", overwrite = FALSE) {
  src <- system.file(
    "extdata",
    "agmip_protocol_template.xlsx",
    package = "CroptimizR"
  )

  if (src == "") {
    stop("Template file not found in CroptimizR installation.")
  }

  # Only return the internal path
  if (is.null(path)) {
    return(src)
  }

  dest <- file.path(path, "agmip_protocol_template.xlsx")

  if (file.exists(dest) && !overwrite) {
    stop("File already exists: ", dest, "\nUse overwrite = TRUE to replace it.")
  }

  ok <- file.copy(src, dest, overwrite = overwrite)
  if (!ok) {
    stop("Failed to copy template to: ", dest)
  }

  message("Template copied to: ", normalizePath(dest))

  normalizePath(dest)
}
