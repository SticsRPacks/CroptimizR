#' @title Load a protocol description file for AgMIP PhaseIV calibration
#'
#' @param protocol_file_path Character string. Path to the Excel file describing the AgMIP PhaseIV calibration protocol.
#'
#' @details
#' Reads an AgMIP PhaseIV protocol file in Excel format and verifies its structure.
#' The function loads the variables, major and candidate parameters, and optional
#' constraints, and returns them in a structured list. The elements of the returned
#' structure can be used directly in the run_protocol_agmip() function to automate
#' the AgMIP calibration protocol.
#'
#' The protocol specification is detailed in Wallach et al., 2024 and Wallach et al., 2025.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{step}{A list of groups, each containing major parameters, candidate parameters, and observed variables.}
#'     \item{param_info}{A list containing parameter bounds (`lb`, `ub`) and default/initial values (`init_values`, `default`).}
#'     \item{forced_param_values}{Named vector of parameter values or formulas to fix, if specified in the protocol file, otherwise NULL.}
#'   }
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr filter
#'
#' @export
#'
load_protocol_agmip <- function(protocol_file_path) {

  # Basic path check
  if (is.null(protocol_file_path) || !is.character(protocol_file_path) || length(protocol_file_path) != 1) {
    stop("`protocol_file_path` must be a single character string giving the path to the protocol description file.")
  }
  if (!file.exists(protocol_file_path)) {
    stop(paste0("Protocol file not found: ", protocol_file_path))
  }
  protocol_file_path <- normalizePath(protocol_file_path, mustWork = TRUE)

  # Check the protocol file structure (sheets and required columns)
  check_protocol_structure(protocol_file_path)

  # Read the necessary sheets
  sheets <- readxl::excel_sheets(protocol_file_path)

  # Variables
  i <- grep("variables", tolower(sheets))
  variables_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
  var_group_map <- setNames(variables_df$group, variables_df$variable)
  groups <- unique(variables_df$group)  # order is defined here

  # Major parameters
  i <- grep("major parameters", tolower(sheets))
  major_params_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
  if (nrow(major_params_df) > 0) {
    if (!is.numeric(major_params_df$`default value`)) stop("Default values of major parameters must be numeric.")
    if (!is.numeric(major_params_df$`lower bound`))   stop("Lower bounds of major parameters must be numeric.")
    if (!is.numeric(major_params_df$`upper bound`))   stop("Upper bounds of major parameters must be numeric.")
  }

  # Candidate parameters
  i <- grep("candidate parameters", tolower(sheets))
  candidate_params_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
  if (nrow(candidate_params_df) > 0) {
    if (!is.numeric(candidate_params_df$`default value`)) stop("Default values of candidate parameters must be numeric.")
    if (!is.numeric(candidate_params_df$`lower bound`))   stop("Lower bounds of candidate parameters must be numeric.")
    if (!is.numeric(candidate_params_df$`upper bound`))   stop("Upper bounds of candidate parameters must be numeric.")
  }

  # Check that groups are known
  if (nrow(major_params_df) > 0) {
    unknown_groups <- setdiff(unique(major_params_df$group), groups)
    if (length(unknown_groups) > 0) {
      stop(paste0(
        "The following group(s) defined in sheet 'major parameters' are not present in sheet 'variables': ",
        paste(unknown_groups, collapse = ", ")
      ))
    }
  }
  if (nrow(candidate_params_df) > 0) {
    unknown_groups <- setdiff(unique(candidate_params_df$group), groups)
    if (length(unknown_groups) > 0) {
      stop(paste0(
        "The following group(s) defined in sheet 'candidate parameters' are not present in sheet 'variables': ",
        paste(unknown_groups, collapse = ", ")
      ))
    }
  }

  # Read constraints if present
  forced_param_values <- NULL
  if (any(grepl("parameters to fix or calculate", tolower(sheets)))) {
    i <- grep("parameters to fix or calculate", tolower(sheets))
    constraints_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
    if (nrow(constraints_df) > 0) {
      forced_param_values <- setNames(constraints_df$`value or formula`, constraints_df$parameter)
    }
  }

  # Bounds & default checks (only if rows exist)
  if (nrow(major_params_df) > 0) {
    idx <- which(major_params_df$`default value` < major_params_df$`lower bound` |
                   major_params_df$`default value` > major_params_df$`upper bound`)
    if (length(idx) > 0) {
      stop(paste0(
        "Default value out of bounds for major parameter(s): ",
        paste(major_params_df$parameter[idx], collapse = ", "),
        ". Please check default value, lower bound and upper bound."
      ))
    }

    idx <- which(major_params_df$`lower bound` >= major_params_df$`upper bound`)
    if (length(idx) > 0) {
      stop(paste0(
        "Invalid bounds for major parameter(s): ",
        paste(major_params_df$parameter[idx], collapse = ", "),
        ". Lower bound is greater than or equal to upper bound."
      ))
    }
  }

  if (nrow(candidate_params_df) > 0) {
    idx <- which(candidate_params_df$`default value` < candidate_params_df$`lower bound` |
                   candidate_params_df$`default value` > candidate_params_df$`upper bound`)
    if (length(idx) > 0) {
      stop(paste0(
        "Default value out of bounds for candidate parameter(s): ",
        paste(candidate_params_df$parameter[idx], collapse = ", "),
        ". Please check default value, lower bound and upper bound."
      ))
    }

    idx <- which(candidate_params_df$`lower bound` >= candidate_params_df$`upper bound`)
    if (length(idx) > 0) {
      stop(paste0(
        "Invalid bounds for candidate parameter(s): ",
        paste(candidate_params_df$parameter[idx], collapse = ", "),
        ". Lower bound is greater than or equal to upper bound."
      ))
    }
  }

  # Build param_info
  param_info <- list(
    lb = setNames(c(major_params_df$`lower bound`, candidate_params_df$`lower bound`),
                  c(major_params_df$parameter, candidate_params_df$parameter)),
    ub = setNames(c(major_params_df$`upper bound`, candidate_params_df$`upper bound`),
                  c(major_params_df$parameter, candidate_params_df$parameter)),
    init_values = setNames(c(major_params_df$`default value`, candidate_params_df$`default value`),
                           c(major_params_df$parameter, candidate_params_df$parameter)),
    default = setNames(c(major_params_df$`default value`, candidate_params_df$`default value`),
                       c(major_params_df$parameter, candidate_params_df$parameter))
  )

  # Build step structure
  step <- lapply(groups, function(group) {

    major <- if (nrow(major_params_df) > 0) major_params_df$parameter[major_params_df$group == group] else NULL
    if (length(major) == 0) major <- NULL

    candidate <- if (nrow(candidate_params_df) > 0) candidate_params_df$parameter[candidate_params_df$group == group] else NULL
    if (length(candidate) == 0) candidate <- NULL

    if (is.null(major) && is.null(candidate)) {
      stop(paste0(
        "Group '", group,
        "' has neither major nor candidate parameters. Each group must have at least one of the two (major OR candidate parameters, not necessarily both)."
      ))
    }

    obs <- names(var_group_map)[var_group_map == group]

    list(
      major_param = major,
      candidate_param = candidate,
      obs_var = obs
    )
  })
  names(step) <- groups

  # Return the result
  list(
    step = step,
    param_info = param_info,
    forced_param_values = forced_param_values
  )
}

#' @title Verify the structure of a protocol description file
#'
#' @param protocol_file_path Character string. Path to the protocol description file.
#'
#' @details
#' Checks that the protocol description file contains all required sheets and that each sheet
#' includes the expected columns. The function is case-insensitive for sheet names.
#' Stops with an informative error message if any sheet or column is missing.
#'
#' @return Invisibly returns TRUE if the structure is valid. Stops execution with an error otherwise.
#'
#' @importFrom readxl excel_sheets read_excel
#'
#' @keywords internal
#'
check_protocol_structure <- function(protocol_file_path) {

  # Verify that the file exists
  if (!file.exists(protocol_file_path)) {
    stop(paste0("Protocol file not found: ", protocol_file_path))
  }

  # Get the available sheets
  sheets <- readxl::excel_sheets(protocol_file_path)
  sheets_lower <- tolower(sheets)

  # Mandatory sheets
  mandatory_sheets <- c(
    "variables",
    "major parameters",
    "candidate parameters"
  )

  # Optional sheets
  optional_sheets <- c(
    "parameters to fix or calculate"
  )

  # Check that all mandatory sheets exist
  missing_sheets <- setdiff(mandatory_sheets, sheets_lower)
  if (length(missing_sheets) > 0) {
    stop(paste0(
      "Sheet(s) missing in protocol file: \"",
      paste(missing_sheets, collapse = "\", \""),
      "\" in ", protocol_file_path,
      "\nPlease add the missing sheet(s)."
    ))
  }

  # Expected columns for each sheet
  expected_cols_ls <- list(
    `variables` = c("variable", "group"),
    `major parameters` = c("parameter", "group", "default value", "lower bound", "upper bound"),
    `candidate parameters` = c("parameter", "group", "default value", "lower bound", "upper bound"),
    `parameters to fix or calculate` = c("parameter", "value or formula")
  )

  # Sheets to check = mandatory ones + optional ones that are present
  sheets_to_check <- c(
    mandatory_sheets,
    intersect(optional_sheets, sheets_lower)
  )

  # Verify columns in each sheet
  invisible(
    lapply(sheets_to_check, function(sheet_name) {

      # Find the actual sheet (case-insensitive)
      idx <- which(sheets_lower == sheet_name)
      if (length(idx) != 1) {
        stop(paste0(
          "Sheet '", sheet_name, "' not found or ambiguous in file ", protocol_file_path
        ))
      }

      # Read the sheet
      df <- readxl::read_excel(protocol_file_path, sheet = sheets[idx])

      # Check the columns
      check_col_names(
        protocol_file_path,
        expected_cols_ls[[sheet_name]],
        names(df),
        sheet_name
      )
    })
  )

  invisible(TRUE)
}

#' @title Verify column names in a protocol sheet
#'
#' @param protocol_file_path Character string. Path to the protocol description file.
#'
#' @param expected_cols Character vector. List of expected column names for the given sheet.
#'
#' @param actual_cols Character vector. Column names as read from the sheet in the protocol file.
#'
#' @param sheet Character string. Name of the sheet being checked.
#'
#' @details
#' Checks that all columns listed in `expected_cols` are present in `actual_cols` for the specified sheet.
#' If any expected columns are missing, the function stops with an informative error message.
#'
#' @return Invisibly returns TRUE if all columns are present. Stops execution with an error otherwise.
#'
#' @keywords internal
#'
check_col_names <- function(protocol_file_path, expected_cols, actual_cols, sheet) {
  # Verify that all expected columns are present in a sheet
  missing_cols <- setdiff(expected_cols, actual_cols)
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Column(s) missing in sheet '", sheet, "': \"",
      paste(missing_cols, collapse = "\", \""),
      "\" in file ", protocol_file_path,
      "\nPlease add the missing column(s)."
    ))
  }
}
