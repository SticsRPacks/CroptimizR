#' @title Criteria to optimize
#'
#' @description
#' Provide several least squares criteria to estimate parameters by minimizing
#' the difference between observed and simulated values of model output
#' variables.
#'
#' @param sim_list List of simulated variables
#' @param obs_list List of observed variables
#'
#' @return The value of the criterion given the observed and simulated values of
#'  the variables.
#'
#' @details The following criteria are proposed ([see html version]
#' (https://sticsrpacks.github.io/CroptimizR/reference/ls_criteria.html)
#' for a better rendering of equations):
#' \itemize{
#'   \item `crit_ols`: ordinary least squares \cr
#'           The sum of squared residues for each variable: \cr
#'           \deqn{ \sum_{i,j,k} [Y_{ijk}-f_{jk}(X_i;\theta)]^2 }
#'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
#'           situation,
#'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, and \eqn{n_j} the number of measurements of variable \eqn{j}. \cr
#'           In this criterion, one assume that all errors (model and observations errors for all variables, dates and situations) are independent, and that the error variance is constant over time and equal for the different variables \eqn{j}.
#'   \item `crit_log_cwss`: log transformation of concentrated version of weighted sum of squares \cr
#'           The concentrated version of weighted sum of squares is: \cr
#'           \deqn{ \prod_{j} {(\frac{1}{n_j} \sum_{i,k} [Y_{ijk}-f_{jk}(X_i;\theta)]^2 )} ^{n_j/2} }
#'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
#'           situation,
#'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, and \eqn{n_j} the number of measurements of variable \eqn{j}. \cr
#'           `crit_log_cwss` computes the log of this equation. \cr
#'           In this criterion, one assume that all errors (model and observations errors for all variables, dates and situations) are independent, and that the error variance is constant over time but may be different between variables \eqn{j}.
#'           These error variances are automatically estimated.
#'           More details about this criterion are given in Wallach et al. (2011), equation 5.
#'   \item `crit_log_cwss_corr`: log transformation of concentrated version of weighted sum of squares with hypothesis of high correlation between errors for different measurements over time \cr
#'           The original criterion is: \cr
#'           \deqn{ \prod_{j} {(\frac{1}{N_j} \sum_{i} [ \frac{1}{n_{ij}} \sum_{k} (Y_{ijk}-f_{jk}(X_i;\theta))^2 ] )} ^{N_j/2} }
#'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
#'           situation,
#'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, \eqn{N_j} the number of situations including at least one observation of variable \eqn{j}, and \eqn{n_{ij}} the number of observation of variable \eqn{j} on situation \eqn{i}. . \cr
#'           `crit_log_cwss_corr` computes the log of this equation. \cr
#'           In this criterion, one still assume that errors in different
#'           situations or for different variables in the same situation are
#'           independent.
#'           However, errors for different observations over time of the same
#'           variable in the same situation are assumed to be highly correlated.
#'           In this way, each situation contributes a single term to the
#'           overall sum of squared errors regardless of the number of
#'           observations which may be usefull in case one have situations with
#'            very heterogenous number of dates of observations.
#'           More details about this criterion are given in Wallach et al.
#'           (2011), equation 8.
#' }
#' `sim_list` and `obs_list` must have the same structure (i.e. same number of
#' variables, dates, situations, ... use internal function
#' intersect_sim_obs before calling the criterion functions).
#'
#' @name ls_criteria
#'
NULL



#' @export
#' @rdname ls_criteria
crit_ols <- function(sim_list, obs_list) {
  # return criterion type (ls, log-ls, likelihood, log-likelihood)
  # if no argument given

  if (!nargs()) {
    return("ls")
  }
  var_list <- unique(unlist(lapply(obs_list, function(x) colnames(x))))
  var_list <- setdiff(var_list, "Date")

  result <- 0

  for (var in var_list) {
    obs <- unlist(sapply(obs_list, function(x) x[is.element(colnames(x), var)]))
    sim <- unlist(sapply(sim_list, function(x) x[is.element(colnames(x), var)]))
    res <- obs - sim
    res <- res[!is.na(res)]

    sz <- length(res)

    result <- result + res %*% res
  }

  return(as.numeric(result))
}


#' @export
#' @rdname ls_criteria
crit_log_cwss <- function(sim_list, obs_list) {
  # return criterion type (ls, log-ls, likelihood, log-likelihood) #
  # if no argument given

  if (!nargs()) {
    return("log-ls")
  }
  var_list <- unique(unlist(lapply(obs_list, function(x) colnames(x))))
  var_list <- setdiff(var_list, "Date")

  result <- 0

  for (var in var_list) {
    obs <- unlist(sapply(obs_list, function(x) x[is.element(colnames(x), var)]))
    sim <- unlist(sapply(sim_list, function(x) x[is.element(colnames(x), var)]))
    res <- obs - sim
    res <- res[!is.na(res)]

    sz <- length(res)

    result <- result + (sz / 2) * (log(1 / sz) + log(res %*% res))
  }

  return(as.numeric(result))
}

#' @export
#' @rdname ls_criteria
crit_log_cwss_corr <- function(sim_list, obs_list) {
  # return criterion type (ls, log-ls, likelihood, log-likelihood)
  # if no argument given
  if (!nargs()) {
    return("log-ls")
  }

  var_list <- unique(unlist(lapply(obs_list, function(x) colnames(x))))
  var_list <- setdiff(var_list, "Date")
  result <- 0

  for (var in var_list) {
    result1 <- 0
    for (i in seq_along(obs_list)) {
      obs <- obs_list[[i]][[var]]
      if (length(obs) != 0) {
        sim <- sim_list[[i]][[var]]
        res <- obs - sim
        res <- res[!is.na(res)]
        # compte les USM
        sz <- length(res)
        result1 <- result1 + (1 / sz) * (res %*% res)
      }
    }
    Nj <- sum(sapply(obs_list, function(x) is.element(var, colnames(x))))
    result <- result + (Nj / 2) * (log(result1) - log(Nj))
  }

  return(as.numeric(result))
}
