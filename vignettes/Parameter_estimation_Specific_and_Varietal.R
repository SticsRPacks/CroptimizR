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
#  javastics_path=path_to_JavaStics
#  
#  # Download the example USMs and define the path to the JavaStics workspace (JavaStics XML input files):
#  data_dir= file.path(SticsRFiles::download_data(),"study_case_1","V9.0")
#  javastics_workspace_path=file.path(data_dir,"XmlFiles")
#  stics_inputs_path=file.path(data_dir,"TxtFiles")
#  
#  # Generate Stics input files from JavaStics input files
#  dir.create(stics_inputs_path)
#  gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
#    target_path = stics_inputs_path, verbose = TRUE)

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
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
#  
#  model_options= stics_wrapper_options(javastics_path, data_dir = stics_inputs_path, parallel=TRUE)

## ----eval=params$eval_auto_test, message=FALSE, warning=FALSE-----------------
#  
#  optim_options=list()
#  optim_options$nb_rep <- 7 # Number of repetitions of the minimization
#                            # (each time starting with different initial
#                            # values for the estimated parameters)
#  optim_options$maxeval <- 1000 # Maximum number of evaluations of the
#                              # minimized criteria
#  optim_options$xtol_rel <- 1e-04 # Tolerance criterion between two iterations
#                                  # (threshold for the relative difference of
#                                  # parameter values between the 2 previous
#                                  # iterations)
#  optim_options$path_results <- data_dir # path where to store the results (graph and Rdata)
#  optim_options$ranseed <- 1234 # random seed

## ----eval=params$eval_auto_test, results='hide', message=FALSE, warning=FALSE----
#  optim_results=estim_param(obs_list=obs_list,
#                              model_function=stics_wrapper,
#                              model_options=model_options,
#                              optim_options=optim_options,
#                              param_info=param_info)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  ## [1] "Estimated value for dlaimax :  0.00112078439658861"
#  ## [1] "Estimated value for durvieF1 :  400"
#  ## [1] "Estimated value for durvieF2 :  213.495265823847"
#  ## [1] "Minimum value of the criterion: -1.48441926984469"

## ----eval=params$eval_auto_vignette, echo=FALSE, out.width = '45%'------------
knitr::include_graphics("ResultsSpecificVarietal/estimInit_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietal/estimInit_durvieF_var1.PNG")
knitr::include_graphics("ResultsSpecificVarietal/estimInit_durvieF_var2.PNG")

## ----eval=params$eval_manual_vignette, message=FALSE, warning=FALSE-----------
#  # Install a few packages needed for the following
#  if(!require("gridExtra")){
#    install.packages("gridExtra",repos="http://cran.irsn.fr")
#    library("gridExtra")
#  }
#  if(!require("hydroGOF")){
#    install.packages("hydroGOF",repos="http://cran.irsn.fr")
#    library("hydroGOF")
#  }
#  if(!require("grid")){
#    install.packages("grid",repos="http://cran.irsn.fr")
#    library("grid")
#  }
#  if(!require("dplyr")){
#    install.packages("dplyr",repos="http://cran.irsn.fr")
#    library("dplyr")
#  }
#  if(!require("ggplot2")){
#    install.packages("ggplot2",repos="http://cran.irsn.fr")
#    library("ggplot2")
#  }
#  # Run the model without and with forcing the optimized values of the parameters
#  sim_before_optim=stics_wrapper(model_options=model_options)
#  sim_after_optim=stics_wrapper(param_values=optim_results$final_values,
#                                model_options=model_options)
#  
#  # transform into data.frame and intersect for using ggplot2
#  sim_before_df = bind_rows(sim_before_optim$sim_list,.id = 'Situation')
#  sim_after_df = bind_rows(sim_after_optim$sim_list,.id = 'Situation')
#  obs_df = bind_rows(obs_list,.id = 'Situation')
#  sim_before_df = rename(sim_before_df,LAI_sim=lai_n)
#  sim_after_df = rename(sim_after_df,LAI_sim=lai_n)
#  obs_df = rename(obs_df,LAI_obs=lai_n)
#  sim_before_obs_df = merge.data.frame(sim_before_df,obs_df,by = c('Situation','Date'), all.x = TRUE)
#  sim_after_obs_df = merge.data.frame(sim_after_df,obs_df,by = c('Situation','Date'), all.x = TRUE)
#  # Compute RMSE
#  rmse_before = rmse(sim_before_obs_df$LAI_obs,sim_before_obs_df$LAI_sim)
#  rmse_after = rmse(sim_after_obs_df$LAI_obs,sim_after_obs_df$LAI_sim)
#  # Plot the graphs
#  max_LAI_sim=max(c(sim_before_obs_df$LAI_sim,sim_after_obs_df$LAI_sim,na.rm=TRUE),na.rm=TRUE)
#  max_LAI_obs=max(sim_before_obs_df$LAI_obs,na.rm=TRUE)
#  p1=ggplot(sim_before_obs_df, aes(x = LAI_obs, y = LAI_sim)) +
#    geom_point(shape=21, size = 3, color = 'blue',
#               fill="white",alpha = 0.8,stroke = 1) +
#    theme(text = element_text(size=16)) +
#    theme(aspect.ratio=1) +
#    labs(x = "Observed LAI", y = "Simulated LAI") +
#    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "blue") +
#    xlim(0,max_LAI_obs) +  ylim(0,max_LAI_sim) +
#    ggtitle(paste("Before optimization \n RMSE=",round(rmse_before,2))) +
#    theme(plot.title = element_text(hjust = 0.5))
#  p2=ggplot(sim_after_obs_df, aes(x = LAI_obs, y = LAI_sim)) +
#    geom_point(shape=21, size = 3, color = 'blue',
#               fill="white",alpha = 0.8,stroke = 1) +
#    theme(text = element_text(size=16)) +
#    theme(aspect.ratio=1) +
#    labs(x = "Observed LAI", y = "Simulated LAI") +
#    geom_abline(intercept = 0, slope = 1, size = 0.5, color = "blue") +
#    xlim(0,max_LAI_obs) +  ylim(0,max_LAI_sim) +
#    ggtitle(paste("After optimization \n RMSE=",round(rmse_after,2))) +
#    theme(plot.title = element_text(hjust = 0.5))
#  p=grid.arrange(grobs=list(p1,p2), nrow=1, ncol=2)
#  # Save the graph
#  ggsave(file.path(optim_options$path_results,
#                   paste0("sim_obs",".png")), plot=p)

## ----eval=params$eval_auto_vignette, echo=FALSE, message=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated vs observed LAI before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSpecificVarietal/sim_obs.png")

## ----move_results, eval=params$eval_manual_vignette, include=FALSE------------
#  # Move the files produced since the temp. folder is removed after Rmd execution
#  file.copy(file.path(optim_options$path_results,"EstimatedVSinit.pdf"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"optim_results.Rdata"),params$result_path)
#  file.copy(file.path(optim_options$path_results,"sim_obs.png"),params$result_path)

