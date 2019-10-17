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
  #' Either a list containing (named) vectors of upper and lower
  #' bounds (\code{ub} and \code{lb}), or a named list containing for each
  #' parameter the list of situations per group (\code{sit_list})
  #' and the vector of upper and lower bounds (one value per group) (\code{ub} and \code{lb})
  #'
  #' @return The vector of values for optimized parameters + prints and graphs,
  #' depending on the parameter estimation method used
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
  init_values=sample_params(prior_information,nb_rep,ranseed)

  # Run nloptr for each repetition
  nlo <- list()
  for (irep in 1:nb_rep){

    nlo[[irep]] <- nloptr(x0 = init_values[irep,], eval_f = main_crit,
                          lb = bounds$lb, ub = bounds$ub,
                          opts = list("algorithm"="NLOPT_LN_NELDERMEAD",
                                      "xtol_rel"=xtol_rel, "maxeval"=maxeval,
                                      "ranseed"=ranseed),
                          crit_options=crit_options_loc)

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

  # Save the results of nloptr
  save(nlo, file = file.path(path_results,"optim_results.Rdata"))

  # Display of parameters for the repetion who have the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ind_min_crit,ipar]))
  }
  print(paste("Minimum value of the criterion :", nlo[[ind_min_crit]]$objective))

  res=est_values[ind_min_crit,]
  names(res)=param_names
  return(res)

}
