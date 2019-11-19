optim_switch <- function(param_names,obs_list,crit_function,model_function,model_options=NULL,optim_method="simplex",optim_options=NULL,prior_information) {
  #' @title Call the required parameter estimation method
  #'
  #' @param param_names Name(s) of parameters to estimate (a parameter name must
  #' be replicated if several groups of situations for this parameter)
  #' @param obs_list List of observed values
  #' @param crit_function Function implementing the criterion to optimize
  #' @param model_function Crop Model wrapper function
  #' @param model_options List of options for the Crop Model wrapper (optional)
  #' @param optim_method Name of the parameter estimation method (optional,
  #' simplex is the only one interfaced for the moment)
  #' @param optim_options List of options of the parameter estimation method:
  #' \code{nb_rep}, the number of repetitions (optional, default=1)
  #' \code{xtol_rel}, the tolerance for the stopping criterion on relative
  #' differences on parameters values between 2 iterations (optional, default=1e-5)
  #' \code{maxeval}, the maximum number of criterion evaluation (optional, default=500)
  #' \code{path_results}, the path where to store the results (optional, default=getwd())
  #' @param prior_information Prior information on the parameters to estimate.
  #' For the moment only uniform distribution are allowed.
  #' Either
  #' a list containing:
  #'    - (named) vectors of upper and lower bounds (\code{ub} and \code{lb}),
  #'    - \code{init_values}, A (column named) vector or data.frame containing initial
  #' values to test for the parameters (optional, if not provided (or if less values
  #' than number of repetitions of the minimization are provided), the (or part
  #' of the) initial values will be randomly generated using LHS sampling within
  #' parameter bounds.
  #'
  #' or
  #' a named list containing for each parameter the list of situations per group
  #' (\code{sit_list}), the vector of upper and lower bounds (one value per group)
  #' (\code{ub} and \code{lb}) and \code{init_values} (one column per group)
  #'
  #' @return prints, graphs and a list containing the results of the parameter estimation.
  #' which content depends on the method used.
  #'   e.g. for Nelder meade simplex in nloptr, this list contains
  #' \code{final_values}, the vector of estimated values for optimized parameters
  #' for the repetition that lead to the lowest value of the criterion
  #' \code{init_values}, the vector of initial values for optimized parameters
  #' \code{est_values}, the vector of estimated values for optimized parameters
  #' for all repetitions
  #' \code{ind_min_crit}, the index of the repetition that lead to the lowest value
  #' of the criterion
  #' \code{nlo}, the data structure returned by nloptr
  #'
  #' @export
  #'
  #' @examples

  # TO DO LIST
  # - externalize nloptr
  #      o see if we create one function for nloptr or one per methods included
  #        in nloptr (depends on their genericity)
  # - add parameter normalization step

  # Normalize parameters
  # TO DO


  # CALL optim method

  if (is.null((nb_rep=optim_options$nb_rep))) { nb_rep=1 }
  if (is.null((xtol_rel=optim_options$xtol_rel))) { xtol_rel=1e-5 }
  if (is.null((maxeval=optim_options$maxeval))) { maxeval=500 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null(optim_options$init_values))
    { init_values=NULL }
  else
    { init_values=matrix(optim_options$init_values,
                       ncol=length(names(optim_options$init_values))) }
  ##
  ## TODO : init_values should be defined in prior_information? (possibly defined per sit group ...)
  ##

  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  nb_params=length(param_names)

  crit_options_loc=list()
  crit_options_loc$param_names=param_names
  crit_options_loc$obs_list=obs_list
  crit_options_loc$crit_function=crit_function
  crit_options_loc$model_function=model_function
  crit_options_loc$model_options=model_options
  crit_options_loc$prior_information=prior_information

  bounds=get_params_bounds(prior_information)

  # Sample initial values
  sample_sz=nb_rep-NROW(init_values)
  if (sample_sz>0) {
    complem_init_values=sample_params(prior_information,sample_sz,ranseed)
    init_values=rbind(init_values,complem_init_values)
  }

  # Run nloptr for each repetition
  nlo <- list()
  start_time <- Sys.time()
  for (irep in 1:nb_rep){

    nlo[[irep]] <- nloptr(x0 = init_values[irep,], eval_f = main_crit,
                          lb = bounds$lb, ub = bounds$ub,
                          opts = list("algorithm"="NLOPT_LN_NELDERMEAD",
                                      "xtol_rel"=xtol_rel, "maxeval"=maxeval,
                                      "ranseed"=ranseed),
                          crit_options=crit_options_loc)

    elapsed <- Sys.time() - start_time
    progress <- 1.0 * irep / nb_rep
    remaining <- elapsed / progress - elapsed
    print(sprintf('Working: %.2f%%. ETA: %.2f', progress * 100, remaining))
  }

  # Get the estimated values
  est_values=t(sapply(nlo,function(x) x$solution))

  # Which repetion has the smallest criterion
  ind_min_crit=which.min(sapply(nlo, function(x) x$objective))

  # Graph and print the results
  pdf(file = file.path(path_results,"EstimatedVSinit.pdf") , width = 9, height = 9)
  for (ipar in 1:nb_params) {
    plot(init_values[,ipar], est_values[,ipar],
         main = "Estimated vs Initial values of the parameters for different repetitions",
         text(init_values[,ipar], est_values[,ipar], pos=1,col="black"),
         xlim = c(bounds$lb[ipar],bounds$ub[ipar]),
         ylim = c(bounds$lb[ipar],bounds$ub[ipar]),
         xlab = paste("Initial value for", param_names[ipar]),
         ylab = paste("Estimated value for", param_names[ipar]))
    text(init_values[ind_min_crit,ipar], est_values[ind_min_crit,ipar],
         labels = ind_min_crit, pos=1,col="red")
  }
  dev.off()

  # pdf(file = file.path(path_results,"ConvergencePlots.pdf") , width = 9, height = 9)
  # for (ipar in 1:(nb_params+1)) {
  #   plot(init_values[,ipar], est_values[,ipar],
  #        main = "Estimated vs Initial values of the parameters for different repetitions",
  #        text(init_values[,ipar], est_values[,ipar], pos=1,col="black"),
  #        xlim = c(bounds$lb[ipar],bounds$ub[ipar]),
  #        ylim = c(bounds$lb[ipar],bounds$ub[ipar]),
  #        xlab = paste("Initial value for", param_names[ipar]),
  #        ylab = paste("Estimated value for", param_names[ipar]))
  #   text(init_values[ind_min_crit,ipar], est_values[ind_min_crit,ipar],
  #        labels = ind_min_crit, pos=1,col="red")
  #}
  # dev.off()


  # Save the results of nloptr
  save(nlo, file = file.path(path_results,"optim_results.Rdata"))

  # Display of parameters for the repetition which has the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ind_min_crit,ipar]))
  }
  print(paste("Minimum value of the criterion :", nlo[[ind_min_crit]]$objective))

 # res=est_values[ind_min_crit,]
 # names(res)=param_names

  final_values <- est_values[ind_min_crit,]
  names(final_values) <- param_names
  res <- list(final_values = final_values,
              init_values = init_values,
              est_values = est_values,
              ind_min_crit = ind_min_crit,
              nlo = nlo)
  return(res)

}
