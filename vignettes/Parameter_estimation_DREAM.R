## ----init, include=FALSE------------------------------------------------------
params <-
list(eval_rmd = FALSE)

## ----setup, eval=TRUE, include=FALSE------------------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

## ----setup_initializations,  echo=FALSE, message=FALSE, results=FALSE, warning=FALSE----
#  knitr::opts_chunk$set(echo = TRUE)
#  # Install and load the needed libraries
#  devtools::install_github("SticsRPacks/SticsRPacks")
#  library("SticsRPacks")
#  
#  # Download the example USMs:
#  data_dir= normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
#  data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
#  download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
#  unzip(data_dir_zip, exdir = data_dir)
#  unlink(data_dir_zip)
#  data_dir= file.path(normalizePath(list.dirs(data_dir)[2], winslash = "/"),"study_case_1","V9.0")
#  # NB: all examples are now in data_dir
#  # Define the path to the local version of JavaStics
#  javastics_path=file.path(path_to_JavaStics,"JavaSTICS-1.41-stics-9.0")
#  stics_path=file.path(javastics_path,"bin/stics_modulo.exe")

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
#  # Read observation files
#  obs_list=get_obs(file.path(data_dir,"XmlFiles"))
#  obs_list=filter_obs(obs_list, var_names=c("lai_n"),include=TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
#  optim_options=list()
#  optim_options$iterations <- 10000 # Total number of iterations
#                                    # (=> optim_options$iterations/optim_options$startValue
#                                    # iterations per chain)
#  optim_options$startValue <- 3 # Number of markov chains
#  optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
#  optim_options$ranseed <- 1234 # seed for random numbers

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
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

## ----eval=TRUE, echo=FALSE, out.width = '45%'---------------------------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_durvieF1.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_durvieF2.PNG")

## ----eval=TRUE, echo=FALSE, out.width = '45%'---------------------------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_durvieF1.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_durvieF2.PNG")

## ----eval=TRUE, echo=FALSE, out.width = '75%'---------------------------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/correlation_plot.PNG")

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#  optim_options$PreviousResults=optim_results$out
#  optim_options$iterations <- 1000 # Total number of new iterations
#  optim_results=estim_param(obs_list=obs_list,
#                            crit_function=likelihood_log_ciidn,
#                            model_function=stics_wrapper,
#                            model_options=model_options,
#                            optim_options=optim_options,
#                            optim_method="BayesianTools.dreamzs",
#                            param_info=param_info)

