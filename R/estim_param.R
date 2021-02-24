#' @title main function for parameter estimation
#'
#' @param obs_list List of observed values to use for parameter estimation
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (Date or POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date.
#' @param crit_function Function implementing the criterion to optimize
#' (optional, see default value in the function signature). See
#' [here](https://sticsrpacks.github.io/CroptimizR/reference/ls_criteria.html)
#' for more details about the list of proposed criterion.
#' @param model_function Crop Model wrapper function to use.
#' @param model_options List of options for the Crop Model wrapper (see help of
#' the Crop Model wrapper function used).
#' @param optim_method Name of the parameter estimation method to use (optional,
#' see default value in the function signature). For the moment, can be "simplex"
#' or "dreamzs". See [here](https://sticsrpacks.github.io/CroptimizR/articles/Available_parameter_estimation_algorithms.html)
#' for a brief description and references on the available methods.
#' @param optim_options List of options of the parameter estimation method.
#' `path_results` The path where to store the results (optional, default=getwd())
#' Click on the links to see the specific options for the [simplex](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html)
#' and [DreamZS](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_DREAM.html) methods.
#' @param param_info Information on the parameters to estimate.
#' Either
#' a list containing:
#'    - (named) vectors of upper and lower bounds (`ub` and `lb`) (-Inf and Inf can be used),
#'    - `init_values`, A data.frame containing initial
#' values to test for the parameters (optional, if not provided, or if less values
#' than number of repetitions of the minimization are provided), the, or part
#' of the, initial values will be randomly generated using LHS sampling within
#' parameter bounds).
#'
#' or
#' a named list containing for each parameter the list of situations per group
#' (`sit_list`), the vector of upper and lower bounds (one value per group)
#' (`ub` and `lb`) and the list of initial values per group
#' `init_values` (data.frame, one column per group, optional).
#' (see [here](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html)
#' for an example)
#' @param forced_param_values Named vector, must contain the values for the model parameters
#' to force (optional, NULL by default). These values will be transfered to the
#' model wrapper through its param_values argument so that the given parameters
#' always take the same values for each model simulation. Should not include values
#' for estimated parameters (i.e. parameters defined in `param_info` argument).
#' @param transform_obs User function for transforming observations before each criterion
#' evaluation (optional), see details section for more information
#' @param transform_sim User function for transforming simulations before each criterion
#' evaluation  (optional), see details section for more information
#' @param satisfy_par_const User function for including constraints on estimated
#' parameters (optional), see details section for more information
#' @param var_names (optional) List of variables for which the wrapper must return results.
#' By default the wrapper is asked to simulate only the observed variables. However,
#' it may be useful to simulate also other variables, typically when transform_sim
#' and/or transform_obs functions are used. Note however that it is active only if
#' the model_function used handles this argument.
#'
#' @details
#'   The optional argument `transform_obs` must be a function with 4 arguments:
#'      o model_results: the list of simulated results returned by the mode_wrapper used
#'      o obs_list: the list of observations as given to estim_param function
#'      o param_values: a named vector containing the current parameters values proposed
#'      by the estimation algorithm
#'      o model_options: the list of model options as given to estim_param function
#'   It must return a list of observations (same format as `obs_list` argument) that
#'   will be used to compute the criterion to optimize.
#'
#'   The optional argument `transform_sim` must be a function with 4 arguments:
#'      o model_results: the list of simulated results returned by the mode_wrapper used
#'      o obs_list: the list of observations as given to estim_param function
#'      o param_values: a named vector containing the current parameters values proposed
#'      by the estimation algorithm
#'      o model_options: the list of model options as given to estim_param function
#'   It must return a list of simulated results (same format as this returned by the model wrapper used)
#'   that will be used to compute the criterion to optimize.
#'
#'   The optional argument `satisfy_par_const` must be a function with 2 arguments:
#'      o param_values: a named vector containing the current parameters values proposed
#'      by the estimation algorithm
#'      o model_options: the list of model options as given to estim_param function
#'   It must return a logical indicating if the parameters values satisfies the constraints
#'   (freely defined by the user in the function body) or not.
#'
#' @return prints, graphs and a list containing the results of the parameter estimation,
#' which content depends on the method used, all that saved in the defined in
#' `optim_options.path_results`
#'
#' @seealso For more detail and examples, see the different vignettes in
#' [CroptimizR website](https://sticsrpacks.github.io/CroptimizR/)
#'
#' @export
#'

estim_param <- function(obs_list, crit_function=crit_log_cwss, model_function,
                        model_options=NULL, optim_method="nloptr.simplex",
                        optim_options, param_info, forced_param_values=NULL,
                        transform_obs=NULL, transform_sim=NULL, satisfy_par_const=NULL,
                        var_names=NULL) {

  # Measured elapse time
  tictoc::tic.clearlog()
  tictoc::tic("Total time for parameter estimation")

  # Check inputs

  ## obs_list
  if (!CroptimizR:::is.obs(obs_list)) {
    stop("Incorrect format for argument obs_list.")
  }
  ## crit_function
  if (!is.function(crit_function)) {
    stop("Incorrect format for argument crit_function. Should be a function.")
  }
  ## model_function
  if (!is.function(model_function)) {
    stop("Incorrect format for argument model_function. Should be a function.")
  }
  ## optim_method
  if (!is.character(optim_method)) {
    stop("Incorrect format for argument optim_method. Should be of type character and contains the name of the parameter estimation method to use.")
  }
  ## param_info
  if (!is.list(param_info)) {
    stop("Incorrect format for argument param_info. Should be a list.")
  } else if ( !all(is.element(c("lb","ub"),names(param_info))) &&
              !all(sapply(param_info, function(x) all(is.element(c("lb","ub"),names(x))))) ) {
    stop("Incorrect format for argument param_info. Should contains lb and ub vectors.")
  }
  if (any(sapply(param_info, function(x) is.element("sit_list",names(x))))) {
    if (!all(sapply(param_info, function(x) is.element("sit_list",names(x))))) {
      stop("sit_list is defined for at least one parameter in argument param_info but not for all.")
    }
#    if (!all(unlist(sapply(param_info, function(x) setequal(unlist(x$sit_list),names(obs_list)),
#                           simplify = FALSE)))) {
#      stop("List of situations in argument param_info$***$sit_list are not identical to observed ones (names(obs_list)) for at least one parameter.")
#    }
  }
  param_names=get_params_names(param_info)

  ## forced_param_values
  if (!is.null(forced_param_values)) {
    if (!is.vector(forced_param_values)) {
      stop("Incorrect format for argument forced_param_values, should be a vector.")
    }
    if (any(names(forced_param_values) %in% param_names)) {
      stop("The following parameters are defined both in forced_param_values and param_info
           arguments of estim_param function while they should not (a parameter cannot
           be both forced and estimated):",paste(intersect(names(forced_param_values),param_names),
                                                 collapse=","))
    }
  }


  # Run the estimation
  crit_options=list(param_names=param_names, obs_list=obs_list,
                    crit_function=crit_function, model_function=model_function,
                    model_options=model_options, param_info=param_info,
                    transform_obs=transform_obs, transform_sim=transform_sim,
                    satisfy_par_const=satisfy_par_const,
                    path_results=optim_options$path_results,
                    var_names=var_names,
                    forced_param_values=forced_param_values)

  result=optim_switch(param_names,optim_method,optim_options,param_info,crit_options)


  # Measure elapse time
  tictoc::toc(log=TRUE)
  result$total_time=unlist(tictoc::tic.log(format = TRUE))
  tictoc::tic.clearlog()

  return(result)

}
