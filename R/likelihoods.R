#' @title Likelihoods
#'
#' @description
#' Provide several likelihoods to estimate parameters using bayesian methods.
#'
#' @param sim_list List of simulated variables
#' @param obs_list List of observed variables
#'
#' @return The value of the likelihood given the observed and simulated values
#'  of the variables.
#'
#' @details The following log-likelihoods are proposed (
#' [see html version](https://sticsrpacks.github.io/CroptimizR/reference/Likelihoods.html)
#' for a better rendering of equations):
#' \itemize{
#'   \item `likelihood_log_ciidn`: log transformation of concentrated version of iid normal likelihood \cr
#'           The concentrated version of iid normal likelihood is:
#'           \deqn{ \prod_{j} ({\sum_{i,k} [Y_{ijk}-f_{jk}(X_i;\theta)]^2 )}^{-(n_j/2+2)} }
#'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
#'           situation,
#'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, and \eqn{n_j} the number of measurements of variable \eqn{j}. \cr
#'           `likelihood_log_ciidn` computes the log of this equation. \cr
#'           Here, one assume that all errors (model and observations errors for all variables, dates and situations) are independent, and that the error variance is constant over time but may be different between variables \eqn{j}.
#'           These error variances are automatically estimated.
#'   \item `likelihood_log_ciidn_corr`: log transformation of concentrated version of iid normal likelihood but with hypothesis of high correlation between errors for different measurements over time\cr
#'           The concentrated version of iid normal likelihood is:
#'           \deqn{ \prod_{j} {( \sum_{i} [ \frac{1}{n_{ij}} \sum_{k} (Y_{ijk}-f_{jk}(X_i;\theta))^2 ] )} ^{-(N_j/2+2)} }
#'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
#'           situation,
#'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, \eqn{N_j} the number of situations including at least one observation of variable \eqn{j}, and \eqn{n_{ij}} the number of observation of variable \eqn{j} on situation \eqn{i}. \cr
#'           `likelihood_log_ciidn_corr` computes the log of this equation. \cr
#'           Here, one still assume that errors in different situations or for
#'            different variables in the same situation are independent.
#'           However, errors for different observations over time of the same
#'           variable in the same situation are assumed to be highly correlated.
#'           In this way, each situation contributes a single term to the
#'           overall sum of squared errors regardless of the number of
#'           observations which may be useful in case one have situations with
#'           very heterogeneous number of dates of observations.
#' }
#' `sim_list` and `obs_list` must have the same structure
#' (i.e. same number of variables, dates, situations, ... use internal function
#' intersect_sim_obs before calling the criterion functions).
#'
#' @name Likelihoods
#'
NULL


#' @export
#' @rdname Likelihoods
likelihood_log_iidn <- function(sim_list,obs_list, weight) {
  #'
  #' @description
  #' log-likelihood for heteroscedastic and independant normal errors, errors are defined in a user function
  #'
  #' @param sim_list List of simulated variables
  #' @param obs_list List of observed variables
  #' @param weight function that takes in input a vector of observed values and the name of the correpsonding variable and that returns the corresponding error standard deviations
  #'
  #' @return The value of the likelihood given the observed and simulated values of the variables.
  #'
  #' @details
  #'           \deqn{ \sum_{j} {\left[ \sum_{i} \left(-\frac{n_{ij}.log(2pi)}{2} -  \sum_{k} \left[log(\sigma_{ijt}) + \frac{1}{2} \left(\frac{Y_{ijk}-f_{jk}(X_i;\theta)}{\sigma_{ijt}}\right)^2 \right] \right) \right] }}
  #'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
  #'           situation,
  #'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, and \eqn{n_{ij}} the number of measurements of variable \eqn{j} for situation \eqn{i}. \cr
  #'
  #' `sim_list` and `obs_list` must have the same structure (i.e. same number of variables, dates, situations, ... use internal function
  #' intersect_sim_obs before calling the criterion functions).

  if (!nargs()) {
    return("log-likelihood")
  }

  var_list=unique(unlist(lapply(obs_list,function (x) colnames(x))))
  var_list=setdiff(var_list,"Date")

  result=0

  for (var in var_list) {
    obs <- unlist(sapply(obs_list, function(x) x[is.element(colnames(x), var)]))
    sim <- unlist(sapply(sim_list, function(x) x[is.element(colnames(x), var)]))
    res <- obs - sim
    id_not_na <- !is.na(res)
    res <- res[id_not_na]
    sigma <- weight(obs[id_not_na], var)

    sz <- length(res)

    result=result - 0.5*sz*log(2*pi)-sum(log(sigma))-0.5*((res/sigma)%*%(res/sigma))

  }

  return(as.numeric(result))
}


#' @export
#' @rdname Likelihoods
likelihood_log_ciidn <- function(sim_list, obs_list, ...) {
  # return criterion type (ls, log-ls, likelihood, log-likelihood)
  # if no argument given

  if (!nargs()) {
    return("log-likelihood")
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

    result <- result - (sz / 2 + 2) * log(res %*% res)
  }

  return(as.numeric(result))
}

#' @export
#' @rdname Likelihoods
likelihood_log_ciidn_corr <- function(sim_list, obs_list, ...) {
  # return criterion type (ls, log-ls, likelihood, log-likelihood)
  # if no argument given
  if (!nargs()) {
    return("log-likelihood")
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
        sz <- length(res)
        result1 <- result1 + (1 / sz) * (res %*% res)
      }
    }
    Nj <- sum(sapply(obs_list, function(x) is.element(var, colnames(x))))
    result <- result - ((Nj / 2) + 2) * log(result1)
  }

  return(as.numeric(result))
}
