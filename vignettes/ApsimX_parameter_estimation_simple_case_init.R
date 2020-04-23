
# DEFINE THE PATH TO THE LOCALLY INSTALLED VERSION OF APSIM (should be something like C:/path/to/apsimx/bin/Models.exe on windows, and /usr/local/bin/Models on linux)
apsimx_path <- path_to_Apsim

#' 
#' ## Set the list of situations and variables to consider in this example
#' 
#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------

sit_name="GattonRowSpacingRowSpace25cm"  # among "GattonRowSpacingRowSpace25cm", "GattonRowSpacingRowSpace50cm","GattonRowSpacingRowSpaceN0"

var_name = c("Wheat.Leaf.LAI") # or "Wheat.AboveGround.Wt"


#' 
#' 
#' ## Run the model before optimization for a prior evaluation
#' 
#' In this case, the argument `param_values` of the wrapper is not set: the values of the model input parameters are all read in the model input files.
#' 
## ----results='hide', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------


# Set the model options (see '? apsimx_wrapper_options' for details)
files_path <- system.file(file.path("extdata","apsimx_files"),package = "ApsimOnR")
apsimx_file <- file.path(files_path, "template.apsimx")

# Setting met files path
met_files_path <- files_path

# Setting observed data files path
obs_files_path <- files_path

# Setting sqlite db tables names 
predicted_table_name <- "DailyReport"
observed_table_name <- "Observed"

model_options=apsimx_wrapper_options(apsimx_path = apsimx_path,
                                     apsimx_file =  apsimx_file,
                                     variable_names = var_name,
                                     predicted_table_name = predicted_table_name,
                                     met_files_path = met_files_path,
                                     observed_table_name = observed_table_name,
                                     obs_files_path = obs_files_path) 

# Run the model (on all situations found in the apsimx_file)
sim_before_optim=apsimx_wrapper(model_options=model_options)



#' 
#' ## Read and select the corresponding observations
#' 
#' We only keep observations for situation `sit_name` and variable `var_name` (`obs_list` defines the list of situations and variables that will be used in the parameter estimation process).
#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------
# At the moment, observed data are read from the db file after the first simulation ran before optimization.
#But they may be loaded using the original xlsx data file (from the files_path)  

obs_list <- read_apsimx_output(sim_before_optim$db_file_name,
                               model_options$observed_table_name,
                               model_options$variable_names,
                               names(sim_before_optim$sim_list))
obs_list=filter_obs(obs_list, sit_names=sit_name,include=TRUE)


#' 
#' ## Set information on the parameters to estimate
#' 
#' **`param_info` must contain information about the parameters that will be estimated in the parameter estimation process from the situations, variables and dates defined in `obs_list`**.
#' 
#' It must include the definition of their upper and lower bounds (-Inf and Inf can be used). This will determine the list of estimated parameters.
#'
#' Initial values for the minimization can also be provided in `param_info` (see `? estim_param`).
#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------
# 2 parameters here: ExtinctionCoeff and RUE, of bounds [0.4,0.6] and [1.4,1.6]
param_info <- 
  list(lb=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.4,
            .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.4),
       ub=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.6,
            .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.6))


#' 
#' ## Set options for the parameter estimation method
#' 
#' `optim_options` should contain the options of the parameter estimation method.
#' Here we defined a few options for the simplex method of the `nloptr` package (default method in estim_param).
#' The full set of options for the simplex method can be found in the [vignette of nloptr package](https://cran.r-project.org/web/packages/nloptr/vignettes/nloptr.pdf).
#' 
#' The number of repetitions `nb_rep` is advised to be set at least to 5, while 10 is a reasonable maximum value.
#' `maxeval` should be used to stop the minimization only if results have to be produced within a given duration, otherwise set it to a high value so that the minimization stops when the criterion based on the relative tolerance `xtol_rel` is satisfied. 
#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------

optim_options=list() 
optim_options$nb_rep <- 7 # Number of repetitions of the minimization 
                          # (each time starting with different initial
                          # values for the estimated parameters) 
optim_options$maxeval <-  500 # Maximum number of evaluations of the 
                             # minimized criteria 
optim_options$xtol_rel <- 1e-03 # Tolerance criterion between two iterations
                                # (threshold for the relative difference of 
                                # parameter values between the 2 previous 
                                # iterations)

optim_options$path_results <- getwd() # path where to store the results (graph and Rdata)

optim_options$ranseed <- 1234 # set random seed so that each execution give the same results
                              # If you want randomization, don't set it.


