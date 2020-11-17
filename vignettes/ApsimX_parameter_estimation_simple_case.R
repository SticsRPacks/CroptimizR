params <-
list(eval_rmd = FALSE)

## ----setup, eval=TRUE, include=FALSE------------------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

## ----setup_initializations, message=FALSE, results=FALSE, warning=FALSE-------
#  
#  # Install and load the needed libraries
#  if(!require("CroptimizR")){
#    devtools::install_github("SticsRPacks/CroptimizR@*release")
#    library("CroptimizR")
#  }
#  if(!require("ApsimOnR")){
#    devtools::install_github("hol430/ApsimOnR")
#    library("ApsimOnR")
#  }
#  if(!require("dplyr")){
#    install.packages("dplyr",repos="http://cran.irsn.fr")
#    library("dplyr")
#  }
#  
#  # DEFINE THE PATH TO THE LOCALLY INSTALLED VERSION OF APSIM (should be something like C:/path/to/apsimx/bin/Models.exe on windows, and /usr/local/bin/Models on linux)
#  apsimx_path <- path_to_Apsim

## ----message=FALSE, warning=FALSE---------------------------------------------
#  
#  sit_name="GattonRowSpacingRowSpace25cm"  # among "GattonRowSpacingRowSpace25cm", "GattonRowSpacingRowSpace50cm","GattonRowSpacingRowSpaceN0"
#  
#  var_name = c("Wheat.Leaf.LAI") # or "Wheat.AboveGround.Wt"

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#  
#  
#  # Set the model options (see '? apsimx_wrapper_options' for details)
#  files_path <- system.file(file.path("extdata","apsimx_files"),package = "ApsimOnR")
#  apsimx_file <- file.path(files_path, "template.apsimx")
#  
#  # Setting met files path
#  met_files_path <- files_path
#  
#  # Setting observed data files path
#  obs_files_path <- files_path
#  
#  # Setting sqlite db tables names
#  predicted_table_name <- "DailyReport"
#  observed_table_name <- "Observed"
#  
#  model_options=apsimx_wrapper_options(apsimx_path = apsimx_path,
#                                       apsimx_file =  apsimx_file,
#                                       variable_names = var_name,
#                                       predicted_table_name = predicted_table_name,
#                                       met_files_path = met_files_path,
#                                       observed_table_name = observed_table_name,
#                                       obs_files_path = obs_files_path)
#  
#  # Run the model (on all situations found in the apsimx_file)
#  sim_before_optim=apsimx_wrapper(model_options=model_options)

## ----message=FALSE, warning=FALSE---------------------------------------------
#  # At the moment, observed data are read from the db file after the first simulation ran before optimization.
#  #But they may be loaded using the original xlsx data file (from the files_path)
#  
#  obs_list <- read_apsimx_output(sim_before_optim$db_file_name,
#                                 model_options$observed_table_name,
#                                 model_options$variable_names,
#                                 names(sim_before_optim$sim_list))
#  obs_list=filter_obs(obs_list, sit_names=sit_name,include=TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
#  # 2 parameters here: ExtinctionCoeff and RUE, of bounds [0.4,0.6] and [1.4,1.6]
#  param_info <-
#    list(lb=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.4,
#              .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.4),
#         ub=c(.Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue=0.6,
#              .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue=1.6))

## ----message=FALSE, warning=FALSE---------------------------------------------
#  
#  optim_options=list()
#  optim_options$nb_rep <- 7 # Number of repetitions of the minimization
#                            # (each time starting with different initial
#                            # values for the estimated parameters)
#  optim_options$maxeval <-  500 # Maximum number of evaluations of the
#                               # minimized criteria
#  optim_options$xtol_rel <- 1e-03 # Tolerance criterion between two iterations
#                                  # (threshold for the relative difference of
#                                  # parameter values between the 2 previous
#                                  # iterations)
#  
#  optim_options$path_results <- getwd() # path where to store the results (graph and Rdata)
#  
#  optim_options$ranseed <- 1234 # set random seed so that each execution give the same results
#                                # If you want randomization, don't set it.

## ----message=FALSE, warning=FALSE---------------------------------------------
#  
#  optim_results=estim_param(obs_list=obs_list,
#                              model_function=apsimx_wrapper,
#                              model_options=model_options,
#                              optim_options=optim_options,
#                              param_info=param_info)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ## [1] "Estimated value for .Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue :  0.432140042063685"
#  ## [1] "Estimated value for .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue :  1.6"
#  ## [1] "Minimum value of the criterion: -9.66022388221585"

## ----eval=TRUE, echo=FALSE, out.width = '50%'---------------------------------

knitr::include_graphics("ResultsSimpleCaseApsim/estimInit_ExtinctionCoeff.png")

knitr::include_graphics("ResultsSimpleCaseApsim/estimInit_RUE.png")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  ## load(file.path(optim_options$path_results,"optim_results.Rdata"))
#  ## nlo[[2]]

## ----echo=FALSE, eval=TRUE----------------------------------------------------
load(file.path("ResultsSimpleCaseApsim","optim_results.Rdata"))
print(nlo[[2]])

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#  
#  sim_after_optim=apsimx_wrapper(param_values=optim_results$final_values,
#                                 model_options=model_options)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#  png(file.path(optim_options$path_results,"sim_obs_plots.png"),
#      width = 15, height = 10, units = "cm", res=1000)
#  par(mfrow = c(1,2))
#  
#  # Simulated and observed LAI before optimization
#  Ymax=max(max(obs_list[[sit_name]][,var_name], na.rm=TRUE),
#           max(sim_before_optim$sim_list[[sit_name]][,var_name], na.rm=TRUE))
#  plot(sim_before_optim$sim_list[[sit_name]][,c("Date",var_name)],type="l",
#       main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
#  points(obs_list[[sit_name]]$Date,obs_list[[sit_name]][[var_name]],col="red")
#  plot(sim_after_optim$sim_list[[sit_name]][,c("Date",var_name)],type="l",
#       main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
#  points(obs_list[[sit_name]]$Date,obs_list[[sit_name]][[var_name]],col="red")
#  
#  dev.off()

## ----eval=TRUE, echo=FALSE, message=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated and observed target variable before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSimpleCaseApsim/sim_obs_plots.png")

