#' @title Provide a list of candidate parameters to estimate based on the Forward Selection
#' algorithm proposed in AgMIP calibration phaseIII protocol and a given information criterion
#'
#' @param oblig_param_list Vector of names of parameters that must be estimated
#' (list of obligatory parameters)
#' @param add_param_list Vector of names of additional parameters candidate to the
#' estimation
#' @param crt_list Vector of names of candidate parameters that have been previously
#' used in parameter estimation
#' @param crt_info_crit Value of the information criterion obtained for the estimation performed using the candidate
#' parameters listed in crt_list
#' @param prev_info_crit Vector of information criterion values obtained so far for all lists of candidate
#' parameters except the current one (crt_list)
#'
#' @details This function implements the Forward Selection based on information criterion
#' as described in AgMIP Calibration protocol phase III
#' (Crop model calibration using phenology data)
#'   If current information criterion value is superior to the previous one, we replace the previously
#'   added candidate parameter in crt_list by the next one in add_param_list.
#'   If current information criterion value is inferior to the previous one, we add a new candidate
#'   parameter to crt_list
#'
#'   A first estimation using oblig_param_list as candidate parameters must be performed
#'   before calling this function
#'
#' @return A list that contains:
#' (i) the names of the candidate parameters for next parameter estimation
#' or NULL when all parameters have been tested (next_candidates)
#' (ii) a flag that indicates if the previous parameters tested are selected (selected)
#'
#' @keywords internal
#'
select_param_FwdRegAgMIP <- function(oblig_param_list, add_param_list, crt_list, crt_info_crit, prev_info_crit) {

  res <- list()
  res$next_candidates <- NULL
  if (is.null(add_param_list)) {

    res$selected <- TRUE
    return(res)

  } else if (crt_list[length(crt_list)]==add_param_list[length(add_param_list)]) {

    # we tested all parameters
    if (crt_info_crit<min(prev_info_crit)) {
      res$selected <- TRUE
    } else {
      res$selected <- FALSE
    }
    return(res)

  } else if (length(crt_list)==length(oblig_param_list)) {

    # we only tested so far the obligatory parameters
    res$selected <- TRUE
    res$next_candidates <- c(oblig_param_list,add_param_list[1])

  } else {

    if (crt_info_crit<min(prev_info_crit)) {
      # Add the next candidate to the list
      res$selected <- TRUE
      res$next_candidates <- c(crt_list, add_param_list[which(add_param_list==crt_list[length(crt_list)])+1])

    } else {

      # Replace the last candidate parameter by the next candidate
      res$selected <- FALSE
      res$next_candidates <- c(crt_list[-length(crt_list)],
                               add_param_list[which(add_param_list==crt_list[length(crt_list)])+1])
    }
  }

  return(res)
}
