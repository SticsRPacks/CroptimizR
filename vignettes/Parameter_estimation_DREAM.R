params <-
list(eval_auto_vignette = TRUE, eval_manual_vignette = FALSE, 
    eval_auto_test = FALSE, path_to_JavaStics = "D:/Home/sbuis/Documents/GitHub/CroptimizR/vignettes/JavaSTICS-1.41-stics-9.0", 
    result_path = "D:/Home/sbuis/Documents/GitHub/CroptimizR/vignettes")

## ----setup, eval=TRUE, include=FALSE------------------------------------------
# Global options
path_to_JavaStics <- params$path_to_JavaStics

## ----setup_install_per_package, eval=params$eval_auto_test,  echo=FALSE-------
#  # Install and load the needed libraries
#  # This one is adapted for manual or test cases (one can first install the version of the libraries we want to test)
#  library(SticsOnR)
#  library(SticsRFiles)
#  library(CroptimizR)

## ----setup_initializations, eval=params$eval_auto_test, message=FALSE, results=FALSE, warning=FALSE,  echo=FALSE----
#  
#  # DEFINE THE PATH TO YOUR LOCALLY INSTALLED VERSION OF JAVASTICS
#  javastics_path= path_to_JavaStics
#  
#  # Download the example USMs and define the path to the JavaStics workspace (JavaStics XML input files):
#  data_dir= file.path(SticsRFiles::download_data(),"study_case_1","V9.0")
#  javastics_workspace_path=file.path(data_dir,"XmlFiles")
#  stics_inputs_path=file.path(data_dir,"TxtFiles")
#  
#  # Generate Stics input files from JavaStics input files
#  dir.create(stics_inputs_path)
#  gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
#    target_path = stics_inputs_path, display = TRUE)

## ----eval=params$eval_auto_test, echo=FALSE, message=FALSE, warning=FALSE-----
#  # Read observation files
#  obs_list=get_obs(javastics_workspace_path)
#  obs_list=filter_obs(obs_list, var_names=c("lai_n"),include=TRUE)

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  param_info=list()
#  param_info$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
#                                          "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
#                          lb=0.0005,ub=0.0025)
#  param_info$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#                                        c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
#                          lb=c(50,100),ub=c(400,450))

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  model_options= stics_wrapper_options(javastics_path, data_dir = stics_inputs_path, parallel=TRUE)

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  optim_options=list()
#  optim_options$iterations <- 10000 # Total number of iterations
#                                    # (=> optim_options$iterations/optim_options$startValue
#                                    # iterations per chain)
#  optim_options$startValue <- 3 # Number of markov chains
#  optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
#  optim_options$ranseed <- 1234 # seed for random numbers

## ----eval=params$eval_auto_test, results='hide', message=FALSE, warning=FALSE----
#  optim_results=estim_param(obs_list=obs_list,
#                            crit_function=likelihood_log_ciidn,
#                            model_function=stics_wrapper,
#                            model_options=model_options,
#                            optim_options=optim_options,
#                            optim_method="BayesianTools.dreamzs",
#                            param_info=param_info)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  ## ## # # # # # # # # # # # # # # # # # # # # # # # # #
#  ## ## ## MCMC chain summary ##
#  ## ## # # # # # # # # # # # # # # # # # # # # # # # # #
#  ## ##
#  ## ## # MCMC sampler:  DREAMzs
#  ## ## # Nr. Chains:  3
#  ## ## # Iterations per chain:  2669
#  ## ## # Rejection rate:  0.878
#  ## ## # Effective sample size:  423
#  ## ## # Runtime:  54194.02  sec.
#  ## ##
#  ## ## # Parameters
#  ## ##            psf     MAP    2.5%  median   97.5%
#  ## ## dlaimax  1.045   0.001   0.001   0.001   0.001
#  ## ## durvieF1 1.000 289.864 213.438 311.226 398.462
#  ## ## durvieF2 1.009 208.487 147.158 298.312 443.323
#  ## ##
#  ## ## ## DIC:  442.256
#  ## ## ## Convergence
#  ## ##  Gelman Rubin multivariate psrf:
#  ## ##
#  ## ## ## Correlations
#  ## ##          dlaimax durvieF1 durvieF2
#  ## ## dlaimax    1.000   -0.074   -0.059
#  ## ## durvieF1  -0.074    1.000   -0.029
#  ## ## durvieF2  -0.059   -0.029    1.000

## ----eval=params$eval_auto_vignette, echo=FALSE, out.width = '45%'------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_durvieF1.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_durvieF2.PNG")

## ----eval=params$eval_auto_vignette, echo=FALSE, out.width = '45%'------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_durvieF1.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_durvieF2.PNG")

## ----eval=params$eval_auto_vignette, echo=FALSE, out.width = '75%'------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/correlation_plot.PNG")

## ----eval=params$eval_auto_test, results='hide', message=FALSE, warning=FALSE----
#  optim_options$PreviousResults=optim_results$out
#  optim_options$iterations <- 1000 # Total number of new iterations
#  optim_results=estim_param(obs_list=obs_list,
#                            crit_function=likelihood_log_ciidn,
#                            model_function=stics_wrapper,
#                            model_options=model_options,
#                            optim_options=optim_options,
#                            optim_method="BayesianTools.dreamzs",
#                            param_info=param_info)

## ----move_results, eval=params$eval_manual_vignette, include=FALSE------------
#  # Move the files produced since the temp. folder is removed after Rmd execution
#  file.copy(file.path(optim_options$path_results,"correlationPlots.pdf"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"gelmanDiagPlots.pdf"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"iterAndDensityPlots.pdf"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"marginalPlots.pdf"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"optim_results.Rdata"),params$result_path)

