#' @title Load the protocol description as given in xls file `protocol_file_path`
#'
#' @inheritParams run_protocol_agmip
#'
#' @param transform_outputs to be described ...
#'
#' @details
#' The AgMIP PhaseIV protocol is thoroughly detailed in Wallach et al., 2024
#' and Wallach et al. 2025.
#'
#' @return A list including:
#   - sitNames_corresp: correspondence between observed and simulated situation names
#   - varNames_corresp: correspondence between simulated and observed variables names
#   - simVar_units: units of simulated variables
#   - param_info: bounds of parameters to estimate
#   - default_param_values: default values of parameters to estimate and parameters to set
#   - param_group: list of major and candidate parameters to estimate
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr filter
#'
#' @export
#'
load_protocol_agmip <- function(protocol_file_path, transform_outputs = NULL) {
  if (is.null(protocol_file_path)) {
    return(NULL)
  }

  check_protocol_structure(protocol_file_path)

  sheets <- readxl::excel_sheets(protocol_file_path)

  # Correspondance between situation Number and name
  sitNames_corresp_df <- readxl::read_excel(protocol_file_path, sheet = grep(tolower("situation names"), sheets))
  sitNames_corresp <- setNames(
    object = sitNames_corresp_df$`Situation Name`,
    nm = sitNames_corresp_df$Number
  )

  # Correspondance between names of observed or required and simulated variables
  variables_df <- readxl::read_excel(protocol_file_path, sheet = grep(tolower("variables"), sheets))
  varNames_corresp_df <- variables_df %>%
    filter(`Name of the simulated variable` != "NA", `Name of the simulated variable` != "na")
  varNames_corresp <- setNames(
    object = varNames_corresp_df$`Name of the simulated variable`,
    nm = varNames_corresp_df$`Name of the observed or required variable`
  )

  # Group of the observed variables
  tmp <- variables_df %>% filter(`Group for calibration` != "NA")
  obsVar_group <- setNames(
    object = tmp$`Group for calibration`,
    nm = tmp$`Name of the observed or required variable`
  )

  # Unit of simulated variables
  simVar_units <- setNames(
    object = varNames_corresp_df$`Unit of the simulated variable`,
    nm = varNames_corresp_df$`Name of the simulated variable`
  )

  # Building param_info
  major_params_df <- readxl::read_excel(protocol_file_path,
    sheet = grep(tolower("major parameters"), sheets)
  )
  if (!is.numeric(major_params_df$`default value`)) {
    stop(paste("Default values of major parameters must be numeric. Please check file", protocol_file_path))
  }
  if (!is.numeric(major_params_df$`lower bound`)) {
    stop(paste("Lower bounds of major parameters must be numeric. Please check file", protocol_file_path))
  }
  if (!is.numeric(major_params_df$`upper bound`)) {
    stop(paste("Upper bounds of major parameters must be numeric. Please check file", protocol_file_path))
  }
  # Check variable groups
  if (!all(major_params_df$group %in% obsVar_group)) {
    stop(paste(
      "Group(s)", paste(major_params_df$group[!(major_params_df$group %in% obsVar_group)],
        collapse = ","
      ),
      "defined in \"major parameters\" sheet of the protocol description file, are not included in the list of groups defined in the \"variables\" sheet.",
      "\nPlease check column \"group\" of \"major parameters\" sheet in file", protocol_file_path
    ))
  }


  candidate_params_df <- readxl::read_excel(protocol_file_path,
    sheet = grep(tolower("candidate parameters"), sheets)
  )
  if (!is.numeric(candidate_params_df$`default value`)) {
    stop(paste("Default values of candidate parameters must be numeric. Please check file", protocol_file_path))
  }
  if (!is.numeric(candidate_params_df$`lower bound`)) {
    stop(paste("Lower bounds of candidate parameters must be numeric. Please check file", protocol_file_path))
  }
  if (!is.numeric(candidate_params_df$`upper bound`)) {
    stop(paste("Upper bounds of candidate parameters must be numeric. Please check file", protocol_file_path))
  }
  if (any(grepl(tolower("parameters to fix or calculate"), sheets))) {
    constraints_df <- readxl::read_excel(protocol_file_path,
      sheet = grep(tolower("parameters to fix or calculate"), sheets)
    )
  } else {
    constraints_df <- NULL
  }
  # Check variable groups
  if (!all(candidate_params_df$group %in% obsVar_group)) {
    stop(paste(
      "Group(s)", paste(candidate_params_df$group[!(candidate_params_df$group %in% obsVar_group)],
        collapse = ","
      ),
      "defined in \"candidate parameters\" sheet of the protocol description file, are not included in the list of groups defined in the \"variables\" sheet.",
      "\nPlease check column \"group\" of \"candidate parameters\" sheet in file", protocol_file_path
    ))
  }


  # Check default values and bounds
  if (any(major_params_df$`default value` < major_params_df$`lower bound` |
    major_params_df$`default value` > major_params_df$`upper bound`)) {
    stop(paste(
      "Default value of parameter(s))",
      paste(major_params_df$`name of the parameter`[major_params_df$`default value` < major_params_df$`lower bound` | major_params_df$`default value` > major_params_df$`upper bound`],
        collapse = ","
      ),
      "out of bounds. Please check default values and bounds of major parameters in file", protocol_file_path
    ))
  }
  if (any(candidate_params_df$`default value` < candidate_params_df$`lower bound` |
    candidate_params_df$`default value` > candidate_params_df$`upper bound`)) {
    stop(paste(
      "Default value of parameter(s))",
      paste(candidate_params_df$`name of the parameter`[candidate_params_df$`default value` < candidate_params_df$`lower bound` | candidate_params_df$`default value` > candidate_params_df$`upper bound`],
        collapse = ","
      ),
      "out of bounds. Please check default values and bounds of candidate parameters in file", protocol_file_path
    ))
  }
  if (any(major_params_df$`lower bound` >= major_params_df$`upper bound`)) {
    stop(paste(
      "Bounds of parameter(s))",
      paste(major_params_df$`name of the parameter`[major_params_df$`lower bound` >= major_params_df$`upper bound`],
        collapse = ","
      ),
      "are not well defined (lower bound >= upper bound. Please check bounds of major parameters in file", protocol_file_path
    ))
  }
  if (any(candidate_params_df$`lower bound` >= candidate_params_df$`upper bound`)) {
    stop(paste(
      "Bounds of parameter(s))",
      paste(candidate_params_df$`name of the parameter`[candidate_params_df$`lower bound` >= candidate_params_df$`upper bound`],
        collapse = ","
      ),
      "are not well defined (lower bound >= upper bound. Please check bounds of candidate parameters in file", protocol_file_path
    ))
  }

  param_info <- list(
    lb = setNames(
      object = c(
        major_params_df$`lower bound`,
        candidate_params_df$`lower bound`
      ),
      nm = c(
        major_params_df$`name of the parameter`,
        candidate_params_df$`name of the parameter`
      )
    ),
    ub = setNames(
      object = c(
        major_params_df$`upper bound`,
        candidate_params_df$`upper bound`
      ),
      nm = c(
        major_params_df$`name of the parameter`,
        candidate_params_df$`name of the parameter`
      )
    ),
    init_values = setNames(
      object = c(
        major_params_df$`default value`,
        candidate_params_df$`default value`
      ),
      nm = c(
        major_params_df$`name of the parameter`,
        candidate_params_df$`name of the parameter`
      )
    ),
    default = setNames(
      object = c(
        major_params_df$`default value`,
        candidate_params_df$`default value`
      ),
      nm = c(
        major_params_df$`name of the parameter`,
        candidate_params_df$`name of the parameter`
      )
    )
  )

  # Parameters per group of observed variables
  param_group <- lapply(unique(major_params_df$group), function(x) {
    res <- list(
      obligatory = filter(major_params_df, group == x)$`name of the parameter`,
      candidates = filter(candidate_params_df, group == x)$`name of the parameter`
    )
    if (length(res$candidates) == 0) {
      res$candidates <- NULL
    }
    return(res)
  })
  names(param_group) <- unique(major_params_df$group)
  param_group <- param_group[intersect(unique(obsVar_group), names(param_group))] # use ordering of the groups as defined in tab Variables

  # Check protocol content
  check_protocol_content(
    protocol_file_path, variables_df, varNames_corresp,
    simVar_units, transform_outputs, param_group,
    obsVar_group, sitNames_corresp
  )

  # Define step structure
  step <- lapply(names(param_group), function(x) {
    res <- list(
      param = param_group[[x]]$obligatory,
      candidate_param = param_group[[x]]$candidates,
      obs_var = varNames_corresp[names(obsVar_group)[obsVar_group == x]]
    )
    if (is.null(res$candidate_param)) {
      res$candidate_param <- character(0)
    }
    return(res)
  })
  names(step) <- names(param_group)

  return(list(step = step, param_info = param_info))
}

#' @title Check the protocol description file structure
#'
#' @inheritParams load_protocol_agmip
#'
#' @details
#'   Check that the protocol description as given in xls file `protocol_file_path`
#'   include the required sheets and columns.
#'
#' @return Stop if protocol structure is not as expected
#'
#' @importFrom readxl excel_sheets read_excel
#'
#' @keywords internal
#'
check_protocol_structure <- function(protocol_file_path) {
  sheets <- readxl::excel_sheets(protocol_file_path)
  expected_sheets <- c(
    "variables", "major parameters", "candidate parameters",
    "parameters to fix or calculate", "situation names"
  )
  if (!all(sapply(expected_sheets, function(x) {
    tolower(x) %in% tolower(expected_sheets)
  }))) {
    stop(paste0(
      "Sheet(s) \"", paste(setdiff(tolower(expected_sheets), tolower(sheets)), collapse = "\", \""),
      "\" not found in ", protocol_file_path, "\nPlease add it (them)."
    ))
  }

  expected_cols_ls <- list(
    `variables` = c("Name of the observed or required variable", "Name of the simulated variable"),
    `major parameters` = c("name of the parameter", "group", "default value", "lower bound", "upper bound"),
    `candidate parameters` = c("name of the parameter", "group", "default value", "lower bound", "upper bound"),
    `parameters to fix or calculate` = c("name of the parameter", "value or formula"),
    `situation names` = c("Number", "Situation Name")
  )
  invisible(
    lapply(names(expected_cols_ls), function(x) {
      df <- readxl::read_excel(protocol_file_path, sheet = grep(tolower(x), sheets))
      check_col_names(protocol_file_path, expected_cols_ls[[x]], names(df), x)
    })
  )
}

#' @title Check the columns names in the protocol description file
#'
#' @inheritParams load_protocol_agmip
#'
#' @param expected_cols List of expected column names for a given sheet
#'
#' @param cols List of column names as read in the protocol description file for a given sheet
#'
#' @param sheet Name of the sheet read in the protocol description file
#'
#' @details
#' Check that the columns listed in `expected_cols` are included in columns listed in `cols`
#' for sheet `sheet`
#'
#' @return Stop if column names are not as expected
#'
#' @keywords internal
#'
check_col_names <- function(protocol_file_path, expected_cols, cols, sheet) {
  if (!all(sapply(expected_cols, function(x) {
    x %in% cols
  }))) {
    stop(paste0(
      "Column(s) \"", paste(setdiff(expected_cols, cols), collapse = "\", \""),
      "\" not found in sheet \"", sheet, "\" of file ",
      protocol_file_path, "\nPlease add it (them)."
    ))
  }
}

#' @title Check the content of a protocol description file
#'
#' @inheritParams load_protocol_agmip
#'
#' @param variables_df to be defined ...
#'
#' @param varNames_corresp to be defined ...
#'
#' @param simVar_units to be defined ...
#'
#' @param transform_outputs to be defined ...
#'
#' @param param_group to be defined ...
#'
#' @param obsVar_group to be defined ...
#'
#' @param sitNames_corresp to be defined ...
#'
#' @return Stop if the content of the protocol description file is not as expected
#'
#' @keywords internal
#'
check_protocol_content <- function(protocol_file_path, variables_df, varNames_corresp,
                                   simVar_units, transform_outputs, param_group,
                                   obsVar_group, sitNames_corresp) {
  # Check situation names were provided
  if (any(is.na(sitNames_corresp))) {
    stop(paste(
      "Missing (one or several) situation name(s). Please check tab sheet \"Situation names\" in file", protocol_file_path,
      "\nA situation name your model_wrapper is able to handle, i.e. is able to run the corresponding situation from its name, must be given for each situation number."
    ))
  }

  # Check that there is at least one "major param" when there are candidates
  invisible(lapply(names(param_group), function(x) {
    if (is.null(param_group[[x]]$obligatory)) {
      stop(paste("\"major parameters\" must be defined for group", x, "\n Please correct file", protocol_file_path))
    }
  }))

  # Check that there is observations for the groups defined in the parameters (major)
  invisible(lapply(names(param_group), function(x) {
    if (!(x %in% unique(obsVar_group[names(varNames_corresp)]))) {
      stop(paste(
        "\"major parameters\" are defined for group", x,
        "but there is either no observed or simulated variables defined for this group in the \"variables\" sheet.\n Please correct file", protocol_file_path
      ))
    }
  }))

  if (!all(sort(names(simVar_units)) == sort(varNames_corresp))) {
    stop(paste0(
      "Incorrect definition of simVar_units or varNames_corresp. ",
      "Please check that they include the same variables.\n",
      paste(setdiff(varNames_corresp, names(simVar_units)), collapse = ","),
      " included in varNames_corresp but not in simVar_units.\n",
      paste(setdiff(names(simVar_units), varNames_corresp), collapse = ","),
      " included in simVar_units but not in varNames_corresp.\n"
    ))
  }

  if (!all(transform_outputs %in% varNames_corresp)) {
    stop(paste0(
      "Incorrect definition of transform_outputs or of the list of simulated variables defined in the \"variables\" sheet of the protocol description xls file ",
      "Please check that all variables included in transform_outputs are also included in this sheet.\n",
      paste(setdiff(transform_outputs, varNames_corresp), collapse = ","),
      " included in transform_outputs but not in the xls sheet.\n"
    ))
  }
}
