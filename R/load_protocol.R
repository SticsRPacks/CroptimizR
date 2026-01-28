#' @title Load an AgMIP calibration protocol Excel file
#'
#' @param protocol_file_path Character string. Path to the Excel file describing an AgMIP calibration protocol.
#'
#' @details
#' Reads an AgMIP calibration protocol file in Excel format and verifies its structure.
#' The Excel file must follow the AgMIP protocol template structure provided by CroptimizR.
#'
#' The function loads the following sheets: variables, major_parameters, candidate_parameters,
#' and the optional fixed_or_computed_parameters sheet. It returns them as a structured list
#' suitable for use in \code{\link{run_protocol_agmip}}.
#'
#' The protocol specification follows the AgMIP methodology (Wallach et al., 2024; Wallach et al., 2025).
#'
#' @section Excel template and example:
#' CroptimizR provides helper functions to access a ready-to-use Excel template and
#' a fully worked example:
#' \itemize{
#'   \item \code{\link{get_agmip_protocol_template}} to obtain the official Excel template of the AgMIP protocol.
#'     This template must be filled by the user before being used with \code{load_protocol_agmip()}.
#'   \item \code{\link{get_agmip_protocol_example}} to access a complete example used for demonstration and testing.
#' }
#'
#' Both functions can either return the path to the file shipped with the package or copy
#' it to a user-defined location for editing.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{step}{A list of groups, each containing major parameters, candidate parameters, and observed variables.}
#'     \item{param_info}{A list containing parameter bounds (`lb`, `ub`) and default values (`default`).}
#'     \item{forced_param_values}{Named vector of parameter values or formulas to fix, if specified in the protocol file, otherwise NULL.}
#'   }
#'
#' @examples
#' # Load the example protocol shipped with the package
#' protocol_file <- get_agmip_protocol_example()
#' protocol <- load_protocol_agmip(protocol_file)
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr filter
#'
#' @export
load_protocol_agmip <- function(protocol_file_path) {
  # Path checks
  if (is.null(protocol_file_path) || !is.character(protocol_file_path) || length(protocol_file_path) != 1) {
    stop("`protocol_file_path` must be a single character string giving the path to the protocol description file.")
  }
  if (!file.exists(protocol_file_path)) {
    stop(paste0("Protocol file not found: ", protocol_file_path))
  }
  protocol_file_path <- normalizePath(protocol_file_path, mustWork = TRUE)

  # Check sheets and columns
  check_protocol_structure(protocol_file_path)
  sheets <- readxl::excel_sheets(protocol_file_path)

  # Read variables
  i <- grep("variables", tolower(sheets))
  variables_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
  var_group_map <- setNames(variables_df$group, variables_df$variable)
  groups <- unique(variables_df$group)

  # Read major parameters
  i <- grep("major_parameters", tolower(sheets))
  major_params_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
  check_bounds_df(major_params_df, "major")

  # Read candidate parameters
  i <- grep("candidate_parameters", tolower(sheets))
  candidate_params_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
  check_bounds_df(candidate_params_df, "candidate")

  # Check that groups exist in variables
  if (nrow(major_params_df) > 0) {
    unknown_groups <- setdiff(unique(major_params_df$group), groups)
    if (length(unknown_groups) > 0) {
      stop(paste0(
        "The following group(s) defined in sheet 'major_parameters' are not present in sheet 'variables': ",
        paste(unknown_groups, collapse = ", ")
      ))
    }
  }
  if (nrow(candidate_params_df) > 0) {
    unknown_groups <- setdiff(unique(candidate_params_df$group), groups)
    if (length(unknown_groups) > 0) {
      stop(paste0(
        "The following group(s) defined in sheet 'candidate_parameters' are not present in sheet 'variables': ",
        paste(unknown_groups, collapse = ", ")
      ))
    }
  }

  # Read fixed or computed parameters if present
  forced_param_values <- NULL
  if (any(grepl("fixed_or_computed_parameters", tolower(sheets)))) {
    i <- grep("fixed_or_computed_parameters", tolower(sheets))
    constraints_df <- readxl::read_excel(protocol_file_path, sheet = sheets[i])
    if (nrow(constraints_df) > 0) {
      forced_param_values <- setNames(constraints_df$value_or_formula, constraints_df$parameter)
    }
  }

  # Build param_info
  param_info <- list(
    lb = setNames(
      c(major_params_df$lower_bound, candidate_params_df$lower_bound),
      c(major_params_df$parameter, candidate_params_df$parameter)
    ),
    ub = setNames(
      c(major_params_df$upper_bound, candidate_params_df$upper_bound),
      c(major_params_df$parameter, candidate_params_df$parameter)
    ),
    default = setNames(
      c(major_params_df$default_value, candidate_params_df$default_value),
      c(major_params_df$parameter, candidate_params_df$parameter)
    )
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
        "' has neither major nor candidate parameters. Each group must have at least one of the two (major OR candidate parameters)."
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

  # Return
  list(
    step = step,
    param_info = param_info,
    forced_param_values = forced_param_values
  )
}

#' @title Verify the structure of a protocol Excel file
#'
#' @param protocol_file_path Character string. Path to the protocol description file.
#'
#' @details
#' Checks that the protocol Excel file contains all required sheets and that each sheet
#' includes the expected columns. The function is case-insensitive for sheet names.
#' Stops with an informative error if any sheet or column is missing.
#'
#' Mandatory sheets: `variables`, `major_parameters`, `candidate_parameters`
#' Optional sheet: `fixed_or_computed_parameters`
#' Column `description` in major/candidate parameters and `parameter_description` or `constraint_description` in constraints are optional.
#'
#' @return Invisibly returns TRUE if the structure is valid. Stops execution with an error otherwise.
#'
#' @importFrom readxl excel_sheets read_excel
#'
#' @keywords internal
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
    "major_parameters",
    "candidate_parameters"
  )

  # Optional sheets
  optional_sheets <- c(
    "fixed_or_computed_parameters"
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
    `major_parameters` = c("parameter", "group", "default_value", "lower_bound", "upper_bound"),
    `candidate_parameters` = c("parameter", "group", "default_value", "lower_bound", "upper_bound"),
    `fixed_or_computed_parameters` = c("parameter", "value_or_formula")
  )

  # Sheets to check = mandatory + optional if present
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
#' @param expected_cols Character vector. List of expected column names for the given sheet.
#' @param actual_cols Character vector. Column names as read from the sheet in the protocol file.
#' @param sheet Character string. Name of the sheet being checked.
#'
#' @details
#' Checks that all columns listed in `expected_cols` are present in `actual_cols` for the specified sheet.
#' If any expected columns are missing, the function stops with an informative error message.
#'
#' @return Invisibly returns TRUE if all columns are present. Stops execution with an error otherwise.
#'
#' @keywords internal
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

#' @title Check parameter bounds and default values
#'
#' @description
#' Internal function to verify that parameter bounds and default values are consistent.
#' Ensures that the default value is numeric and lies within the lower and upper bounds,
#' and that the lower bound is strictly smaller than the upper bound.
#'
#' @param df A `data.frame` containing the parameters to check. Must include columns:
#'   `parameter`, `default_value`, `lower_bound`, `upper_bound`.
#'
#' @param param_type Character string. A descriptive label of the parameter type, e.g., "major" or "candidate",
#'   used in error messages.
#'
#' @details
#' This function is used internally in `load_protocol_agmip()` to validate
#' both major and candidate parameters in an AgMIP protocol file.
#' It stops execution with an informative error if any inconsistency is detected.
#'
#' @return Invisibly returns `NULL`. Stops with an error if any check fails.
#'
#' @keywords internal
check_bounds_df <- function(df, param_type) {
  # Return immediately if no rows
  if (nrow(df) == 0) {
    return(NULL)
  }

  # Ensure numeric columns
  numeric_cols <- c("default_value", "lower_bound", "upper_bound")
  for (col in numeric_cols) {
    if (!is.numeric(df[[col]])) {
      stop(sprintf("Column '%s' of %s parameters must be numeric.", col, param_type))
    }
  }

  # Check default values are within bounds
  idx <- which(df$default_value < df$lower_bound | df$default_value > df$upper_bound)
  if (length(idx) > 0) {
    stop(sprintf(
      "Default value out of bounds for %s parameter(s): %s. Please check default, lower bound, and upper bound.",
      param_type,
      paste(df$parameter[idx], collapse = ", ")
    ))
  }

  # Check that lower bound is strictly smaller than upper bound
  idx <- which(df$lower_bound >= df$upper_bound)
  if (length(idx) > 0) {
    stop(sprintf(
      "Invalid bounds for %s parameter(s): %s. Lower bound is greater than or equal to upper bound.",
      param_type,
      paste(df$parameter[idx], collapse = ", ")
    ))
  }
}
