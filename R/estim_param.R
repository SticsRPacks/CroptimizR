#' @title main function for parameter estimation
#'
#' @param obs_list List of observed values to use for parameter estimation
#' A `named list` (names = situations names) of data.frame containing
#' one column named Date with the dates (POSIXct format) of the different observations
#' and one column per observed variables with either the measured values or NA, if
#' the variable is not observed at the given date.
#' @param crit_function Function implementing the criterion to optimize
#' (optional, default=crit_cwss). See
#' [here](https://sticsrpacks.github.io/CroptimizR/reference/ls_criterion.html)
#' for more details about the list of proposed criterion.
#' @param model_function Crop Model wrapper function to use
#' @param model_options List of options for the Crop Model wrapper (optional,
#' see help of the Crop Model wrapper function used)
#' @param optim_method Name of the parameter estimation method to use (optional,
#' default="nloptr.simplex", the only one available for the moment)
#' @param optim_options List of options of the parameter estimation method:
#' `nb_rep`, the number of repetitions (optional, default=1)
#' `xtol_rel`, the tolerance for the stopping criterion on relative
#' differences on parameters values between 2 iterations (optional, default=1e-5)
#' `maxeval`, the maximum number of criterion evaluation (optional, default=500)
#' `path_results`, the path where to store the results (optional, default=getwd())
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
#'
#' # Example one one situation and one variable with the Stics model
#' # Version 9.* of Stics is needed to run this example
#'
#' # Load the needed packages
#'  if(!require("SticsRPacks")){
#'    devtools::install_github("SticsRPacks/SticsRPacks")
#'    library("SticsRPacks")
#'  }
#'
#' # Download the data for the example (Stics input files in txt format, one folder per USM)
#' data_dir= normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
#' data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
#' download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
#' unzip(data_dir_zip, exdir = data_dir)
#' unlink(data_dir_zip)
#' data_dir= file.path(normalizePath(list.dirs(data_dir)[2], winslash = "/"),"study_case_1","V9.0")
#' # NB: all examples are now in data_dir
#'
#' # Define the path to the locally installed version of JavaStics
#' # THIS SHOULD BE ADAPTED TO YOUR CASE !!!
#' javastics_path=file.path(getwd(),"JavaSTICS-1.41-stics-9.0")
#' stics_path=file.path(javastics_path,"bin/stics_modulo.exe")
#'
#' # Read and select the observations for the parameter estimation
#' sit_name="bo96iN+"
#' obs_list=get_obs(file.path(data_dir,"XmlFiles"),
#'                           obs_filenames = paste0(sit_name,".obs"))
#' var_name="lai_n"
#' obs_list[[sit_name]]=obs_list[[sit_name]][,c("Date",var_name)]
#'
#' # Set parameter information on the parameters to estimate
#' param_info=list(lb=c(dlaimax=0.0005, durvieF=50),
#'                        ub=c(dlaimax=0.0025, durvieF=400),
#'                        init_values=data.frame(dlaimax=c(0.0015), durvieF=c(225)))
#'
#' # Set options for the parameter estimation method
#' optim_options=list()
#' optim_options$nb_rep <- 2 # How many times we run the minimization with different parameters
#' optim_options$xtol_rel <- 1e-05 # Tolerance criterion between two iterations
#' optim_options$maxeval <- 5 # Maximum number of iterations executed by the function
#' optim_options$path_results <- data_dir # path where to store results graphs
#'
#'# Set the model options (see '? stics_wrapper_options' for details)
#' model_options=stics_wrapper_options(stics_path,data_dir,
#'                                     parallel=FALSE)
#'
#' # Run the optimization
#' optim_results=estim_param(obs_list=obs_list,
#'                          model_function=stics_wrapper,
#'                          model_options=model_options,
#'                          optim_options=optim_options,
#'                          param_info=param_info)
#'
#' # See results (graph and Rdata) stored in optim_options$path_results
#'
#' # See the different vignettes in [CroptimizR website](https://sticsrpacks.github.io/CroptimizR/)
#' for more details and examples.
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
