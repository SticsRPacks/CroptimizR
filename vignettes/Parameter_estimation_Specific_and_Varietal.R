params <-
list(eval_rmd = FALSE)

## ----setup, eval=TRUE, include=FALSE------------------------------------------
# Global options
knitr::opts_chunk$set(eval = params$eval_rmd)

## ----setup_initializations,  echo=FALSE, message=FALSE, results=FALSE, warning=FALSE----
#  knitr::opts_chunk$set(echo = TRUE)
#
#  # Install and load the needed libraries
#  if(!require("SticsRFiles")){
#    devtools::install_github("SticsRPacks/SticsRFiles@*release")
#    library("SticsRFiles")
#  }
#  if(!require("SticsOnR")){
#    devtools::install_github("SticsRPacks/SticsOnR@*release")
#    library("SticsOnR")
#  }
#  if(!require("CroptimizR")){
#    devtools::install_github("SticsRPacks/CroptimizR@*release")
#    library("CroptimizR")
#  }
#
#  # Download the example USMs:
#  data_dir= normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
#  data_dir_zip= normalizePath(file.path(data_dir,"master.zip"), winslash = "/", mustWork = FALSE)
#  download.file("https://github.com/SticsRPacks/data/archive/master.zip", data_dir_zip)
#  unzip(data_dir_zip, exdir = data_dir)
#  unlink(data_dir_zip)
#  data_dir= file.path(normalizePath(list.dirs(data_dir)[2], winslash = "/"),"study_case_1","V9.0")
#  # NB: all examples are now in data_dir
#
#  # Define the path to the local version of JavaStics
#  javastics_path=file.path(getwd(),"JavaSTICS-1.41-stics-9.0")
#  stics_path=file.path(javastics_path,"bin/stics_modulo.exe")

## ----message=FALSE, warning=FALSE---------------------------------------------
#  # Read observation files
#  obs_list=get_obs(file.path(data_dir,"XmlFiles"))
#  obs_list=lapply(obs_list,"[",c("Date","lai_n"))

## ----message=FALSE, warning=FALSE---------------------------------------------
#  prior_information=list()
#  prior_information$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),lb=0.0005,ub=0.0025)
#  prior_information$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"), c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),lb=c(50,100),ub=c(400,450))

## ----message=FALSE, warning=FALSE---------------------------------------------
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
#  optim_options$path_results <- getwd() # path where to store the results (graph and Rdata)
#  optim_options$ranseed <- 1234 # random seed

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
#  # Set the model options (see '? stics_wrapper_options' for details)
#  model_options=stics_wrapper_options(stics_path,data_dir, parallel=TRUE)
#
#  # Run the optimization
#  optim_results=estim_param(obs_list=obs_list,
#                              model_function=stics_wrapper,
#                              model_options=model_options,
#                              optim_options=optim_options,
#                              prior_information=prior_information)
#

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  ## [1] "Estimated value for dlaimax :  0.001111705"
#  ## [1] "Estimated value for durvieF1 :  359.7516"
#  ## [1] "Estimated value for durvieF2 :  384.4479"
#  ## [1] "Minimum value of the criterion : 7.98187472584448"

## ----eval=TRUE, echo=FALSE, out.width = '50%'---------------------------------

knitr::include_graphics("ResultsSpecificVarietal/estimInit_dlaimax.PNG")

knitr::include_graphics("ResultsSpecificVarietal/estimInit_durvieF_var1.PNG")

knitr::include_graphics("ResultsSpecificVarietal/estimInit_durvieF_var2.PNG")


## ----message=FALSE, warning=FALSE---------------------------------------------
#  # Run the model without and with forcing the optimized values of the parameters
#  sim_before_optim=stics_wrapper(model_options=model_options)
#
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
#
#  # Compute RMSE
#  rmse_before = rmse(sim_before_obs_df$LAI_obs,sim_before_obs_df$LAI_sim)
#  rmse_after = rmse(sim_after_obs_df$LAI_obs,sim_after_obs_df$LAI_sim)
#
#  # Plot the graphs
#  max_LAI_sim=max(c(sim_before_obs_df$LAI_sim,sim_after_obs_df$LAI_sim,na.rm=TRUE),na.rm=TRUE)
#  max_LAI_obs=max(sim_before_obs_df$LAI_obs,na.rm=TRUE)
#
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
#
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
#
#  p=grid.arrange(grobs=list(p1,p2), nrow=1, ncol=2)
#
#  # Save the graph
#  ggsave(file.path(optim_options$path_results,
#                   paste0("sim_obs",".png")), plot=p)
#
#

## ----eval=TRUE, echo=FALSE, message=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated vs observed LAI before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSpecificVarietal/sim_obs.png")

