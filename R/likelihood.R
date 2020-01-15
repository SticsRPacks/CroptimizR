#' @title Likelihood
#'
#' @description
#' Provide several likelihood to estimate parameters using bayesian methods.
#'
#' @param sim_list List of simulated variables
#' @param obs_list List of observed variables
#'
#' @return The value of the likelihood given the observed and simulated values of the variables.
#'
#' @details The following likelihood are proposed ([see html version](https://sticsrpacks.github.io/CroptimizR/reference/likelihood.html) for a better rendering of equations):
#' \itemize{
#'   \item `likelihood_ciidn`: concentrated version of iid normal likelihood
#'           \deqn{ \prod_{j} {\sum_{i,k} [Y_{ijk}-f_{jk}(X_i;\theta)]^2 )}^{-(n_j/2+2)} }
#'           where \eqn{ Y_{ijk} } is the observed value for the \eqn{k^{th}} time point of the \eqn{j^{th}} variable in the \eqn{i^{th}}
#'           situation,
#'           \eqn{ f_{jk}(X_i;\theta) } the corresponding model prediction, and \eqn{n_j} the number of measurements of variable \eqn{j}. \cr
#'           More details about this criterion are given in Wallach et al. (2011), equation 5.
#'   \item `likelihood_log_ciidn`: log transformation of concentrated version of iid normal likelihood \cr
#' }
#' `sim_list` and `obs_list` must have the same structure (i.e. same number of variables, dates, situations, ... use internal function
#' intersect_sim_obs before calling the criterion functions).
#'
#' @name Likelihood
#'
NULL


#' @export
#' @rdname Likelihood
likelihood_ciidn <- function(sim_list,obs_list) {

  var_list=unique(unlist(lapply(obs_list,function (x) colnames(x))))
  var_list=setdiff(var_list,"Date")

  result=1

  for (var in var_list) {
    obs=unlist(sapply(obs_list,function (x) x[is.element(colnames(x),var)]))
    sim=unlist(sapply(sim_list,function (x) x[is.element(colnames(x),var)]))
    res=obs-sim
    res=res[!is.na(res)]

    sz=length(res)

    result=result * (res%*%res)^(-(sz/2+2))
  }

  return(as.numeric(result))
}

#' @export
#' @rdname Likelihood
likelihood_log_ciidn <- function(sim_list,obs_list) {

  var_list=unique(unlist(lapply(obs_list,function (x) colnames(x))))
  var_list=setdiff(var_list,"Date")

  result=0

  for (var in var_list) {
    obs=unlist(sapply(obs_list,function (x) x[is.element(colnames(x),var)]))
    sim=unlist(sapply(sim_list,function (x) x[is.element(colnames(x),var)]))
    res=obs-sim
    res=res[!is.na(res)]

    sz=length(res)

    result=result - (sz/2+2)*log(res%*%res+1e-300)
  }

  return(as.numeric(result))
}

#' @export
#' @rdname Likelihood
likelihood_ciidn_corr <- function(sim_list,obs_list) {

  var_list=unique(unlist(lapply(obs_list,function (x) colnames(x))))
  var_list=setdiff(var_list,"Date")

  result=1

  for (var in var_list) {
    result1 <- 0
    for (i in 1:length(obs_list)) {
      obs=obs_list[[i]][[var]]
      if (length(obs)!=0) {
        sim=sim_list[[i]][[var]]
        res=obs-sim
        res=res[!is.na(res)]
        sz=length(res)
        result1=result1 + (1/sz)*(res%*%res)
      }
    }
    Nj <-sum(sapply(obs_list,function (x) is.element(var,colnames(x))))
    result<-result*(result1^-((Nj/2)+2))
  }

  return(as.numeric(result))
}
