#' @title A wrapper for nloptr package
#'
#' @inheritParams optim_switch
#'
#' @return prints, graphs and a list containing:
#' `final_values`, the vector of estimated values for optimized parameters
#' for the repetition that lead to the lowest value of the criterion
#' `init_values`, the vector of initial values for optimized parameters
#' `est_values`, the vector of estimated values for optimized parameters
#' for all repetitions
#' `ind_min_crit`, the index of the repetition that lead to the lowest value
#' of the criterion
#' `nlo`, the data structure returned by nloptr
#'
#' @keywords internal
#'

wrap_nloptr <- function(optim_options,param_info,crit_options) {

  if (is.null((nb_rep=optim_options$nb_rep))) { nb_rep<-5 }
  if (is.null((xtol_rel=optim_options$xtol_rel))) { xtol_rel<-1e-4 }
  if (is.null((ftol_rel=optim_options$ftol_rel))) { ftol_rel<-1e-10 }
  if (is.null((maxeval=optim_options$maxeval))) { maxeval<-500 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed<-NULL }
  if (is.null((path_results=optim_options$path_results))) { path_results<-getwd() }
  if (!is.null((optim_options$algorithm))) if (toupper(optim_options$algorithm)!="NLOPT_LN_NELDERMEAD") {
     warning("Only NLOPT_LN_NELDERMEAD algorithm is interfaced in CroptimizR for package nloptr.")
    }
  algorithm<-"NLOPT_LN_NELDERMEAD"

  # return requested information if only optim_options is given in argument
  if (nargs()==1 & methods::hasArg(optim_options)) {
    return(list(package="nloptr", family="Frequentist",
                method="NLOPT_LN_NELDERMEAD", init_values_nb=nb_rep))
  }

  crit_options$tot_max_eval <- nb_rep*maxeval
  param_names <- get_params_names(param_info)
  nb_params=length(param_names)
  bounds=get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  # Run nloptr for each repetition
  nlo <- vector("list",nb_rep)
  nlo<-lapply(nlo,function(x) {x<-list(objective=NA,solution=rep(NA,nb_params))})

  start_time <- Sys.time()
  for (irep in 1:nb_rep){

    crit_options$irep <- irep
    try(nlo[[irep]] <- nloptr::nloptr(x0 = as.numeric(init_values[irep,]), eval_f = main_crit,
                          lb = bounds$lb, ub = bounds$ub,
                          opts = list("algorithm"=algorithm,
                                      "xtol_rel"=xtol_rel, "maxeval"=maxeval,
                                      "ranseed"=ranseed),
                          crit_options=crit_options))

    elapsed <- Sys.time() - start_time
    progress <- 1.0 * irep / nb_rep
    remaining <- elapsed / progress - elapsed
    cat(sprintf('Working: %.2f%%. Estimated remaining time: %.2f %s\n', progress * 100, remaining, units(remaining)))
  }
  if (all(is.na(sapply(nlo,function(x) x$objective)))) {
    stop(paste("All",nb_rep,
               "repetitions of the parameter estimation lead to an error.",
               "\n   * Please look at warning messages."))
  } else if (any(is.na(sapply(nlo,function(x) x$objective)))) {
    warning(paste("Some repetitions of the parameter estimation aborted.",
               "\n   * Please look at other warning messages for more details."))
  }

  # Get the estimated values
  est_values=t(rbind(sapply(nlo,`[[`,"solution")))
  colnames(est_values)=param_names

  # Which repetion has the smallest criterion
  ind_min_crit=which.min(sapply(nlo, function(x) x$objective))

  # Store all criterion values
  crit <- sapply(nlo, function(x) x$objective)

  final_values <- est_values[ind_min_crit,]
  names(final_values) <- param_names
  res <- list(final_values = final_values,
              init_values = init_values,
              est_values = est_values,
              min_crit_value = nlo[[ind_min_crit]]$objective,
              ind_min_crit = ind_min_crit,
              crit_values=crit,
              nlo = nlo)

  return(res)

}
