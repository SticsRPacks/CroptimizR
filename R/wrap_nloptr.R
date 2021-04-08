#' @title A wrapper for nloptr package
#'
#' @inheritParams optim_switch
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
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

wrap_nloptr <- function(param_names,optim_options,param_info,crit_options) {
  if (is.null((nb_rep=optim_options$nb_rep))) { nb_rep=1 }
  if (is.null((xtol_rel=optim_options$xtol_rel))) { xtol_rel=1e-4 }
  if (is.null((ftol_rel=optim_options$ftol_rel))) { ftol_rel=1e-10 }
  if (is.null((maxeval=optim_options$maxeval))) { maxeval=500 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  nb_params=length(param_names)
  bounds=get_params_bounds(param_info)
  user_init_values=get_params_init_values(param_info)

  # Sample initial values and include user provided ones
  init_values <- complete_init_values(user_init_values, nb_rep, lb = bounds$lb,
                                      ub = bounds$ub, ranseed,
                                      satisfy_par_const=crit_options$satisfy_par_const)

  # Run nloptr for each repetition
  nlo <- vector("list",nb_rep)
  nlo<-lapply(nlo,function(x) {x<-list(objective=NA,solution=rep(NA,nb_params))})

  start_time <- Sys.time()
  for (irep in 1:nb_rep){

    crit_options$irep <- irep
    try(nlo[[irep]] <- nloptr::nloptr(x0 = as.numeric(init_values[irep,]), eval_f = main_crit,
                          lb = bounds$lb, ub = bounds$ub,
                          opts = list("algorithm"="NLOPT_LN_NELDERMEAD",
                                      "xtol_rel"=xtol_rel, "maxeval"=maxeval,
                                      "ranseed"=ranseed),
                          crit_options=crit_options))

    elapsed <- Sys.time() - start_time
    progress <- 1.0 * irep / nb_rep
    remaining <- elapsed / progress - elapsed
    print(sprintf('Working: %.2f%%. ETA: %.2f', progress * 100, remaining))
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

  # Graph and print the results
  tmp<-rbind(bounds$lb,bounds$ub,est_values, init_values)
  tmp[apply(tmp,2,is.infinite)]<-NA
  minvalue<-apply(tmp,2,min,na.rm=TRUE); maxvalue<-apply(tmp,2,max,na.rm=TRUE)
  minvalue<-minvalue-0.05*(maxvalue-minvalue); maxvalue<-maxvalue+0.05*(maxvalue-minvalue)
  crit <- sapply(nlo, function(x) x$objective)

  tryCatch(
    {
      grDevices::pdf(file = file.path(path_results,"EstimatedVSinit.pdf") , width = 9, height = 9)
    },
    error=function(cond) {
      filename=paste0("EstimatedVSinit_new.pdf")
      warning("Error trying to create ",path_results,"/EstimatedVSinit.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",filename)
      message(cond)
      grDevices::pdf(file = file.path(path_results,filename) , width = 9, height = 9)
    })

  tryCatch(
    {
      p <- plot_estimVSinit(init_values, est_values, crit, bounds$lb, bounds$ub)
      print(p)
      grDevices::dev.off()
    },
    error=function(cond) {

      warning("Error trying to create EstimatedVSinit bubble graph file. \n
              Maybe linked with the values of the criterion to plot (size of the bubbles):",
              paste0(crit,collapse = ","),"\n Trying without the bubbles ...")
      message(cond)

      p <- plot_estimVSinit(init_values, est_values, crit, bounds$lb, bounds$ub, bubble=FALSE)
      print(p)
      grDevices::dev.off()
    })


  # Display of parameters for the repetition which has the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ind_min_crit,ipar]))
  }
  print(paste("Minimum value of the criterion:", nlo[[ind_min_crit]]$objective))
  print(paste("Complementary graphs and results can be found in ", path_results))

  final_values <- est_values[ind_min_crit,]
  names(final_values) <- param_names
  res <- list(final_values = final_values,
              init_values = init_values,
              est_values = est_values,
              min_crit_value = nlo[[ind_min_crit]]$objective,
              ind_min_crit = ind_min_crit,
              crit_values=crit,
              nlo = nlo)

  # Save the results of nloptr
  save(res, file = file.path(path_results,"optim_results.Rdata"))

  return(res)

}
