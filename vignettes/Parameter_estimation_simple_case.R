params <-
list(eval_auto_vignette = TRUE, eval_manual_vignette = FALSE, 
    eval_auto_test = FALSE, path_to_JavaStics = "D:/Home/sbuis/Documents/GitHub/CroptimizR/vignettes/JavaSTICS-1.41-stics-9.0", 
    result_path = "D:/Home/sbuis/Documents/GitHub/CroptimizR/vignettes")

## ----setup, eval=TRUE, include=FALSE------------------------------------------
# Global options
path_to_JavaStics <- params$path_to_JavaStics

## ----setup_install_per_package, eval=params$eval_auto_test, include=FALSE-----
#  # Install and load the needed libraries
#  # This one is adapted for manual or test cases (one can first install the version of the libraries we want to test)
#  library(SticsOnR)
#  library(SticsRFiles)
#  library(CroptimizR)
#  

## ----setup_install, eval=FALSE, message=FALSE, results=FALSE, warning=FALSE----
#  # Install and load the needed libraries
#  if(!require("SticsRPacks")){
#    devtools::install_github("SticsRPacks/SticsRPacks")
#    library("SticsRPacks")
#  }

## ----setup_initializations, eval=params$eval_auto_test, message=FALSE, results=FALSE, warning=FALSE----
#  
#  # DEFINE THE PATH TO YOUR LOCALLY INSTALLED VERSION OF JAVASTICS
#  javastics_path=path_to_JavaStics
#  
#  # Download the example USMs and define the path to the JavaStics workspace (JavaStics XML input files):
#  data_dir= file.path(SticsRFiles::download_data(),"study_case_1","V9.0")
#  javastics_workspace_path=file.path(data_dir,"XmlFiles")

## ----gen_dirs, eval=params$eval_auto_test, results='hide', message=FALSE, warning=FALSE----
#  stics_inputs_path=file.path(data_dir,"TxtFiles")
#  dir.create(stics_inputs_path)
#  
#  gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
#    target_path = stics_inputs_path, verbose = TRUE)

## ----eval=params$eval_auto_test, results='hide', message=FALSE, warning=FALSE----
#  
#  # Set the model options (see '? stics_wrapper_options' for details)
#  model_options= stics_wrapper_options(javastics_path, data_dir = stics_inputs_path, parallel=FALSE)
#  
#  # Run the model on all situations found in stics_inputs_path
#  sim_before_optim=stics_wrapper(model_options=model_options)

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  
#  sit_name="bo96iN+"  # can be a vector of situation names if you want to consider several, e.g. c("bo96iN+","bou00t1")
#  var_name="lai_n"    # can be a vector of variable names if you want to consider several, e.g. c("lai_n","masec_n")
#  obs_list= get_obs(javastics_workspace_path, usm_name = sit_name)
#  obs_list= filter_obs(obs_list, var_names= var_name, include=TRUE)

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  # 2 parameters here: dlaimax and durvieF, of bounds [0.0005,0.0025] and [50,400].
#  param_info=list(lb=c(dlaimax=0.0005, durvieF=50),
#                         ub=c(dlaimax=0.0025, durvieF=400))

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  optim_options=list()
#  optim_options$nb_rep <- 7 # Number of repetitions of the minimization
#                            # (each time starting with different initial
#                            # values for the estimated parameters)
#  optim_options$maxeval <- 500 # Maximum number of evaluations of the
#                               # minimized criteria
#  optim_options$xtol_rel <- 1e-03 # Tolerance criterion between two iterations
#                                  # (threshold for the relative difference of
#                                  # parameter values between the 2 previous
#                                  # iterations)
#  optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
#  optim_options$ranseed <- 1234 # set random seed so that each execution give the same results
#                                # If you want randomization, don't set it.

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  optim_results=estim_param(obs_list=obs_list,
#                              model_function=stics_wrapper,
#                              model_options=model_options,
#                              optim_options=optim_options,
#                              param_info=param_info)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  ## [1] "Estimated value for dlaimax :  0.00169614928696274"
#  ## [1] "Estimated value for durvieF :  53.9691276907021"
#  ## [1] "Minimum value of the criterion: 4.72322279544591"

## ----eval=params$eval_auto_vignette, echo=FALSE, out.width = '45%'------------

knitr::include_graphics("ResultsSimpleCase/estimInit_dlaimax.PNG")

knitr::include_graphics("ResultsSimpleCase/estimInit_durvieF.PNG")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  ## load(file.path(optim_options$path_results,"optim_results.Rdata"))
#  ## res$nlo[[2]]

## ----echo=FALSE, eval=params$eval_auto_vignette, warning=FALSE----------------
load(file.path("ResultsSimpleCase","optim_results.Rdata"))
print(res$nlo[[2]])

## ----eval=params$eval_manual_vignette, message=FALSE, warning=FALSE-----------
#  sim_after_optim=stics_wrapper(param_values=optim_results$final_values,
#                                model_options=model_options)

## ----eval=params$eval_manual_vignette, warning=FALSE, message=FALSE-----------
#  png(file.path(optim_options$path_results,"sim_obs_plots.png"),
#      width = 15, height = 10, units = "cm", res=1000)
#  par(mfrow = c(1,2))
#  
#  # Simulated and observed LAI before optimization
#  Ymax=max(max(obs_list[[sit_name]][,var_name], na.rm=TRUE),
#           max(sim_before_optim$sim_list[[sit_name]][,var_name], na.rm=TRUE))
#  plot(sim_before_optim$sim_list[[sit_name]][,c("Date",var_name)],type="l",
#       main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
#  points(obs_list[[sit_name]],col="green")
#  
#  # Simulated and observed LAI after optimization
#  plot(sim_after_optim$sim_list[[sit_name]][,c("Date",var_name)],type="l",
#       main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
#  points(obs_list[[sit_name]],col="green")
#  
#  dev.off()

## ----eval=params$eval_auto_vignette, echo=FALSE, message=FALSE, warning=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated and observed target variable before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSimpleCase/sim_obs_plots.png")

## ----move_results, eval=params$eval_manual_vignette, include=FALSE------------
#  # Move the files produced since the temp. folder is removed after Rmd execution
#  file.copy(file.path(optim_options$path_results,"EstimatedVSinit.pdf"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"optim_results.Rdata"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"sim_obs_plots.png"),params$result_path)

