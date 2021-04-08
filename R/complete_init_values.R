#' @title Complete given initial values by sampling values from bounds taking into account inequality constraints between parameters
#'
#' @inheritParams estim_param
#'
#' @param init_values A data.frame containing some initial
#' values for the parameters (or NULL)
#'
#' @param nb_values Total number of initial values required for the parameters
#'
#' @param lb Lower bounds of the parameters
#'
#' @param ub Upper bounds of the parameters
#'
#' @param ranseed Set random seed so that each execution give the same results.
#' If you want randomization, set it to NULL
#'
#' @return A data.frame containing initial values for all parameters and repetitions
#' of the minimization process that includes the initial values given in `init_values`
#' plus some randomly chosen ones in bounds `lb` and `ub`
#'
#' @keywords internal
#'
#' @examples
#'
#' # No user initial values provided nor constraints
#' lb = c(dlaimax = 0.0005, durvieF = 50)
#' ub = c(dlaimax = 0.0025, durvieF = 400)
#' CroptimizR:::complete_init_values(init_values=NULL, nb_values=5, lb=lb, ub=ub)
#'
#' # Some user initial values are provided but no constraints
#' lb = c(dlaimax = 0.0005, durvieF = 50)
#' ub = c(dlaimax = 0.0025, durvieF = 400)
#' init_values = data.frame(dlaimax=c(0.001, NA), durvieF=c(75, 100))
#' CroptimizR:::complete_init_values(init_values=NULL, nb_values=5, lb=lb, ub=ub)
#'
complete_init_values <- function(init_values, nb_values, lb, ub, ranseed=NULL,
                                 satisfy_par_const=NULL) {

  if (!is.null(lb) && !is.null(ub)) {
    param_names <- names(lb)
    sampled_values <- as.data.frame(sample_params(list(lb=lb, ub=ub),nb_values,ranseed))
    if (!is.null(satisfy_par_const)) {
      idx <- which(sapply(1:nrow(sampled_values),function(x) {satisfy_par_const(sampled_values[x,])}))
      sampled_values <- sampled_values[idx,]
      count <- 1
      while (nrow(sampled_values)<nb_values && count<1000) {
        seed <- sample(1000,1,replace=FALSE)
        sampled_tmp <- as.data.frame(CroptimizR:::sample_params(list(lb=lb, ub=ub),nb_values,
                                                                seed=seed))
        idx <- which(sapply(1:nrow(sampled_tmp),function(x) {satisfy_par_const(sampled_tmp[x,])}))
        sampled_values <- dplyr::bind_rows(sampled_values,sampled_tmp[idx,])
        count <- count+1
      }
      if (count>=1000) stop(paste("Error, number of sampling of initial values reached maximum allowed value: ",count,
                            "samplings have been performed but this did not allow to gather",nb_values,"satisfying the prescribed constraints on the parameters."))
      sampled_values <- sampled_values[1:nb_values,]
    }
    for (param in param_names) {
      idx <- which(!is.na(init_values[,param]))
      if (length(idx)>0) {
        sampled_values[idx[1:min(nb_values,length(idx))],param] <- init_values[idx[1:min(nb_values,length(idx))],param]
      }
    }
  } else {
    if (nrow(init_values)<nb_values || any(is.na(init_values))) {
      stop("Init_values must contain initial values for all parameters and repetitions if ub and lb are not provided.")
    }
    sampled_values <- init_values
  }

  return(sampled_values)
}
