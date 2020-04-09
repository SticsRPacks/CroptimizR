
# Download the example USMs:
data_dir= file.path(SticsRFiles::download_data(),"study_case_1","V9.0")
# NB: all examples are now in data_dir

# DEFINE THE PATH TO YOUR LOCALLY INSTALLED VERSION OF JAVASTICS
javastics_path=file.path(path_to_JavaStics,"JavaSTICS-1.41-stics-9.0")
stics_path=file.path(javastics_path,"bin/stics_modulo.exe")

#'
#' ## Generate Stics input files from JavaStics input files
#'
#' The Stics wrapper function used in CroptimizR works on text formatted input files (new_travail.usm,
#' climat.txt, ...) stored per USMs in different directories (which names must be the USM names).
#' `stics_inputs_path` is here the path of the directory that will contain these USMs folders.
#'
#' If you start from xml formatted input files (JavaStics format: usms.xml, sols.xml, ...)
#' the following lines allow generating txt files from xml files.
#' In this case, xml files are stored in `file.path(data_dir,"XmlFiles")` and the Stics input files
#' (test format) will be stored in `stics_inputs_path=file.path(data_dir,"TxtFiles")`
#'
## ----gen_dirs, results='hide', message=FALSE, warning=FALSE-------
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)

gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = file.path(data_dir,"XmlFiles"),
  target_path = stics_inputs_path, display = TRUE)


#'
#'
#' ## Run the model before optimization for a prior evaluation
#'
#' Here model parameters values are read in the model input files.
#'
## ----results='hide', message=FALSE, warning=FALSE-----------------

# Set the model options (see '? stics_wrapper_options' for details)
model_options=stics_wrapper_options(stics_path,stics_inputs_path,
                                    parallel=FALSE)

# Run the model on all situations found in stics_inputs_path
sim_before_optim=stics_wrapper(model_options=model_options)


#'
#' ## Read and select the observations for the parameter estimation
#'
#' For Stics, observation files must for the moment have exactly the same names as the corresponding USMs and be stored in a unique folder to be read by the get_obs function. This will be improved in next versions.
#'
#' In this example, we only keep observations for situation (i.e. USM for Stics) `sit_name` and variable `var_name`.
#'
#' **`obs_list` defines the list of situations, variables and dates that will be used to estimate the parameters. Use the function `filter_obs` (see `? filter_obs`) for removing situations, variables and/or dates from an observation list.**
#'
#' In variables and parameters names, "(\*)" must be replaced by "_\*" to be handled by R (e.g. lai(n) is denoted here lai_n).
#'
## ----message=FALSE, warning=FALSE---------------------------------

sit_name="bo96iN+"  # can be a vector of situation names if you want to consider several, e.g. c("bo96iN+","bou00t1")
var_name="lai_n"    # can be a vector of variable names if you want to consider several, e.g. c("lai_n","masec_n")
obs_list=get_obs(file.path(data_dir,"XmlFiles"),
                          obs_filenames = paste0(sit_name,".obs"))
obs_list=filter_obs(obs_list, var_names=var_name,include=TRUE)


#'
#' ## Set information on the parameters to estimate
#'
#' **`param_info` must contain information about the parameters that will be estimated in the parameter estimation process from the situations, variables and dates defined in `obs_list`**.
#'
#' It must include the definition of their upper and lower bounds (-Inf and Inf can be used). This will determine the list of estimated parameters.
#'
#' All the numerical parameters which values can be provided to the model through its R wrapper can be estimated using the provided parameter estimation methods (although it may not work well for integer parameters).
#'
#' Initial values for the minimization can also be provided in `param_info` (see `? estim_param`).
#'
## ----message=FALSE, warning=FALSE---------------------------------
# 2 parameters here: dlaimax and durvieF, of bounds [0.0005,0.0025] and [50,400].
param_info=list(lb=c(dlaimax=0.0005, durvieF=50),
                       ub=c(dlaimax=0.0025, durvieF=400))

#'
#' ## Set options for the parameter estimation method
#'
#' `optim_options` must contain the options of the parameter estimation method.
#' Here we defined a few important options for the simplex method of the nloptr package (default method in estim_param).
#' To see the full set of options available for the simplex method, type `? nl.opts`
#'
#' The number of repetitions is advised to be set to at least 5, while 10 is a reasonable maximum value.
#' `maxeval` should be used to stop the minimization only if results have to be produced within a given duration, otherwise set it to a high value so that the minimization stops when the criterion based on `xtol_rel` is satisfied.
#'
## ----message=FALSE, warning=FALSE---------------------------------
optim_options=list()
optim_options$nb_rep <- 7 # Number of repetitions of the minimization
                          # (each time starting with different initial
                          # values for the estimated parameters)
optim_options$maxeval <- 500 # Maximum number of evaluations of the
                             # minimized criteria
optim_options$xtol_rel <- 1e-03 # Tolerance criterion between two iterations
                                # (threshold for the relative difference of
                                # parameter values between the 2 previous
                                # iterations)
optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
optim_options$ranseed <- 1234 # set random seed so that each execution give the same results
                              # If you want randomization, don't set it.
