
#' 
#' The results printed in output on the R console are the following:
## ----eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## [1] "Estimated value for dlaimax :  0.00112078439658861"
## [1] "Estimated value for durvieF1 :  400"
## [1] "Estimated value for durvieF2 :  213.495265823847"
## [1] "Minimum value of the criterion: -1.48441926984469"
#' 
#' Complementary graphs and data are stored in the `optim_options$path_results` folder. Among them, the EstimatedVSinit.pdf file contains the following figures: 
#' 
## ----eval=TRUE, echo=FALSE, out.width = '45%'------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("ResultsSpecificVarietal/estimInit_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietal/estimInit_durvieF_var1.PNG")
knitr::include_graphics("ResultsSpecificVarietal/estimInit_durvieF_var2.PNG")

#' 
#' Figure 1: plots of estimated vs initial values of parameters dlaimax and durvieF (estimated for both varieties).
#' 
#' 
#' ## Compare simulations and observations before and after optimization
#' 
#' A couple of plots to check if the calibration reduced the difference between simulations and observations.
#' 
## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Install a few packages needed for the following
if(!require("gridExtra")){
  install.packages("gridExtra",repos="http://cran.irsn.fr")
  library("gridExtra")
}
if(!require("hydroGOF")){
  install.packages("hydroGOF",repos="http://cran.irsn.fr")
  library("hydroGOF")
}
if(!require("grid")){
  install.packages("grid",repos="http://cran.irsn.fr")
  library("grid")
}
if(!require("dplyr")){
  install.packages("dplyr",repos="http://cran.irsn.fr")
  library("dplyr")
}
if(!require("ggplot2")){
  install.packages("ggplot2",repos="http://cran.irsn.fr")
  library("ggplot2")
}
# Run the model without and with forcing the optimized values of the parameters
sim_before_optim=stics_wrapper(model_options=model_options)
sim_after_optim=stics_wrapper(param_values=optim_results$final_values,
                              model_options=model_options)
  
# transform into data.frame and intersect for using ggplot2
sim_before_df = bind_rows(sim_before_optim$sim_list[[1]],.id = 'Situation')
sim_after_df = bind_rows(sim_after_optim$sim_list[[1]],.id = 'Situation')
obs_df = bind_rows(obs_list,.id = 'Situation')
sim_before_df = rename(sim_before_df,LAI_sim=lai_n)
sim_after_df = rename(sim_after_df,LAI_sim=lai_n)
obs_df = rename(obs_df,LAI_obs=lai_n)
sim_before_obs_df = merge.data.frame(sim_before_df,obs_df,by = c('Situation','Date'), all.x = TRUE)
sim_after_obs_df = merge.data.frame(sim_after_df,obs_df,by = c('Situation','Date'), all.x = TRUE)
# Compute RMSE
rmse_before = rmse(sim_before_obs_df$LAI_obs,sim_before_obs_df$LAI_sim)
rmse_after = rmse(sim_after_obs_df$LAI_obs,sim_after_obs_df$LAI_sim)
# Plot the graphs
max_LAI_sim=max(c(sim_before_obs_df$LAI_sim,sim_after_obs_df$LAI_sim,na.rm=TRUE),na.rm=TRUE)
max_LAI_obs=max(sim_before_obs_df$LAI_obs,na.rm=TRUE)
p1=ggplot(sim_before_obs_df, aes(x = LAI_obs, y = LAI_sim)) + 
  geom_point(shape=21, size = 3, color = 'blue',
             fill="white",alpha = 0.8,stroke = 1) +
  theme(text = element_text(size=16)) + 
  theme(aspect.ratio=1) +
  labs(x = "Observed LAI", y = "Simulated LAI") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "blue") +
  xlim(0,max_LAI_obs) +  ylim(0,max_LAI_sim) +
  ggtitle(paste("Before optimization \n RMSE=",round(rmse_before,2))) +
  theme(plot.title = element_text(hjust = 0.5))
p2=ggplot(sim_after_obs_df, aes(x = LAI_obs, y = LAI_sim)) + 
  geom_point(shape=21, size = 3, color = 'blue',
             fill="white",alpha = 0.8,stroke = 1) +
  theme(text = element_text(size=16)) + 
  theme(aspect.ratio=1) +
  labs(x = "Observed LAI", y = "Simulated LAI") +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "blue") +
  xlim(0,max_LAI_obs) +  ylim(0,max_LAI_sim) +
  ggtitle(paste("After optimization \n RMSE=",round(rmse_after,2))) +
  theme(plot.title = element_text(hjust = 0.5))
p=grid.arrange(grobs=list(p1,p2), nrow=1, ncol=2)
# Save the graph
ggsave(file.path(optim_options$path_results,
                 paste0("sim_obs",".png")), plot=p)
  

#' 
#' 
#' This gives:
#' 
## ----eval=TRUE, echo=FALSE, message=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated vs observed LAI before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSpecificVarietal/sim_obs.png")

