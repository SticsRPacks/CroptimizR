#' @title Computes AIC for ordinary least squares
#'
#' @inheritParams estim_param
#' @param crit_value Final value of the estimated criterion
#' @param param_nb Number of estimated parameters
#'
#' @return Value of the AIC criterion for ordinary least squares.
#' If called without arguments, returns a named list with element "name"
#' containing the name of the function
#'
#' @export
#'
AIC <- function(obs_list, crit_value, param_nb) {
  if (nargs() == 0) {
    return(list(name = "AIC"))
  }

  # Total number of observations
  n <- sum(sapply(
    obs_list,
    function(x) sum(!is.na(x %>% select(-Date)))
  ))

  return(n * log(crit_value / n) + 2 * param_nb)
}



#' @title Computes AICc for ordinary least squares
#'
#' @inheritParams estim_param
#' @param crit_value Final value of the estimated criterion
#' @param param_nb Number of estimated parameters
#'
#' @return Value of the AICc criterion for ordinary least squares.
#' If called without arguments, returns a named list with element "name"
#' containingthe name of the function and "species" containing
#' "Information criterion"
#'
#' @export
#'
AICc <- function(obs_list, crit_value, param_nb) {
  if (nargs() == 0) {
    return(list(name = "AICc"))
  }

  # Total number of observations
  n <- sum(sapply(
    obs_list,
    function(x) sum(!is.na(x %>% select(-Date)))
  ))

  p <- param_nb

  if ((n - p - 1) == 0) {
    warning("AICc takes Inf value since n-p-1==0,
         where n is the number of observation and p the number of parameters.")
    return(Inf)
  }

  return(n * log(crit_value / n) + 2 * p + 2 * p * (p + 1) / (n - p - 1))
}



#' @title Computes BIC for ordinary least squares
#'
#' @inheritParams estim_param
#' @param crit_value Final value of the estimated criterion
#' @param param_nb Number of estimated parameters
#'
#' @return Value of the BIC criterion for ordinary least squares.
#' If called without arguments, returns a named list with element "name"
#' containing the name of the function and "species" containing
#' "Information criterion"
#'
#' @export
#'
BIC <- function(obs_list, crit_value, param_nb) {
  if (nargs() == 0) {
    return(list(name = "BIC"))
  }

  # Total number of observations
  n <- sum(sapply(
    obs_list,
    function(x) sum(!is.na(x %>% select(-Date)))
  ))

  return(n * log(crit_value / n) + param_nb * log(n))
}
