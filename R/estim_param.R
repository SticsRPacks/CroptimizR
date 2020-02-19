#' @title main function for parameter estimation
#'
#' @param obs_list List of observed values to use for parameter estimation
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (Date or POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date.
#' @param crit_function Function implementing the criterion to optimize
#' (optional, see default value in the function signature). See
#' [here](https://sticsrpacks.github.io/CroptimizR/reference/ls_criterion.html)
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
#' or
#' a named list containing for each parameter the list of situations per group
#' (`sit_list`), the vector of upper and lower bounds (one value per group)
#' (`ub` and `lb`) and the list of initial values per group
#' `init_values` (data.frame, one column per group, optional).
#' (see [here](https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_Specific_and_Varietal.html) for an example)
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
#' @examples
#' \dontrun{
#' # See the different vignettes in [CroptimizR website](https://sticsrpacks.github.io/CroptimizR/)
#' for examples.
#' }



estim_param <- function(obs_list,crit_function=crit_log_cwss,model_function,model_options=NULL,
                        optim_method="nloptr.simplex",optim_options=NULL,param_info) {

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


  # Run the estimation

  param_names=get_params_names(param_info)
  return(optim_switch(param_names,obs_list,crit_function,model_function,model_options,
                      optim_method,optim_options,param_info))

  }
