#' @title A wrapper for optim function
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
#' `optim`, the data structure returned by nloptr
#'
#' @keywords internal
#'

wrap_optim <- function(param_names,optim_options,param_info,crit_options) {

  if (is.null((nb_rep=optim_options$nb_rep))) { nb_rep=1 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null((hessian=optim_options$hessian))) { hessian=FALSE }
  if (is.null((method=optim_options$method))) { method="Nelder-Mead" }
  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  nb_params=length(param_names)
  set.seed(ranseed)

  bounds=get_params_bounds(param_info)
  user_init_values=get_params_init_values(param_info)

  # Sample initial values and include user provided ones
  init_values=sample_params(param_info,nb_rep,ranseed)
  for (param in param_names) {
    idx=which(!is.na(user_init_values[,param]))
    init_values[idx,param]=user_init_values[idx,param]
  }

  # Run nloptr for each repetition
  optim <- list()
  start_time <- Sys.time()
  for (irep in 1:nb_rep){

    optim[[irep]] <- stats::optim(par=as.numeric(init_values[irep,]), fn = main_crit,
                                  method=method,
#                          lower=bounds$lb, upper=bounds$ub,
                          control = optim_options,
                          hessian=hessian,
                          crit_options=crit_options)

    elapsed <- Sys.time() - start_time
    progress <- 1.0 * irep / nb_rep
    remaining <- elapsed / progress - elapsed
    print(sprintf('Working: %.2f%%. ETA: %.2f', progress * 100, remaining))

  }

  # Get the estimated values
  est_values=t(sapply(optim,function(x) x$par))

  # Which repetion has the smallest criterion
  ind_min_crit=which.min(sapply(optim, function(x) x$value))

  # Graph and print the results
  tryCatch(
    {
      grDevices::pdf(file = file.path(path_results,"EstimatedVSinit.pdf") , width = 9, height = 9)
      for (ipar in 1:nb_params) {
        graphics::plot(init_values[,ipar], est_values[,ipar],
                       main = "Estimated vs Initial values of the parameters for different repetitions",
                       graphics::text(init_values[,ipar], est_values[,ipar], pos=1,col="black"),
                       xlim = c(bounds$lb[ipar],bounds$ub[ipar]),
                       ylim = c(bounds$lb[ipar],bounds$ub[ipar]),
                       xlab = paste("Initial value for", param_names[ipar]),
                       ylab = paste("Estimated value for", param_names[ipar]))
        graphics::text(init_values[ind_min_crit,ipar], est_values[ind_min_crit,ipar],
                       labels = ind_min_crit, pos=1,col="red")
      }
      grDevices::dev.off()
    },
    error=function(cond) {
      warning("Error trying to create ",path_results,"/EstimatedVSinit.pdf file. It is maybe opened in a pdf viewer and locked. It will not be created.")
      message(cond)
      flush.console()
      })


  # Save the results of nloptr
  save(optim, file = file.path(path_results,"optim_results.Rdata"))

  # Display of parameters for the repetition which has the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ind_min_crit,ipar]))
  }
  print(paste("Minimum value of the criterion:", optim[[ind_min_crit]]$value))
  print(paste("Complementary graphs and results can be found in ", path_results))

  final_values <- est_values[ind_min_crit,]
  names(final_values) <- param_names
  res <- list(final_values = final_values,
              init_values = init_values,
              est_values = est_values,
              ind_min_crit = ind_min_crit,
              optim = optim)
  return(res)

}
