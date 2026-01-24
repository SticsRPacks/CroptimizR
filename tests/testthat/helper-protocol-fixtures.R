#' @title Generate a test AgMIP protocol Excel file
#'
#' @param path Path where the Excel file will be saved.
#' @param variables Data.frame for the 'variables' sheet. Default generates 3 variables in 2 groups.
#' @param major Data.frame for the 'major parameters' sheet. Default generates 1 parameter.
#' @param candidate Data.frame for the 'candidate parameters' sheet. Default generates 1 parameter.
#' @param constraints Data.frame for 'parameters to fix or calculate' sheet. Optional.
#' @param extra_sheets Named list of extra sheets to include. Optional.
#'
#' @details
#' Creates an Excel file suitable for testing `load_protocol_agmip()` and
#' `check_protocol_structure()`. Column names are written **exactly** as specified.
#'
#' @return Path to the created Excel file.
#'
#' @keywords internal
make_protocol_xlsx <- function(
    path,
    variables = data.frame(
      variable = c("v1", "v2", "v3"),
      group = c("G1", "G1", "G2"),
      check.names = FALSE
    ),
    major = data.frame(
      parameter = c("p1"),
      group = c("G1"),
      `default value` = 1,
      `lower bound` = 0,
      `upper bound` = 2,
      check.names = FALSE
    ),
    candidate = data.frame(
      parameter = c("p2"),
      group = c("G2"),
      `default value` = 5,
      `lower bound` = 0,
      `upper bound` = 10,
      check.names = FALSE
    ),
    constraints = NULL,
    extra_sheets = list()
) {
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "variables")
  openxlsx::writeData(wb, "variables", variables)

  openxlsx::addWorksheet(wb, "major parameters")
  openxlsx::writeData(wb, "major parameters", major)

  openxlsx::addWorksheet(wb, "candidate parameters")
  openxlsx::writeData(wb, "candidate parameters", candidate)

  if (!is.null(constraints)) {
    openxlsx::addWorksheet(wb, "parameters to fix or calculate")
    openxlsx::writeData(wb, "parameters to fix or calculate", constraints)
  }

  if (length(extra_sheets) > 0) {
    for (nm in names(extra_sheets)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, extra_sheets[[nm]])
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}
