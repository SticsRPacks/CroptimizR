

#' 
#' The results printed in output on the R console are the following:
## ----echo=TRUE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## [1] "Estimated value for .Simulations.Replacements.Wheat.Leaf.ExtinctionCoeff.VegetativePhase.FixedValue :  0.432140042063685"
## [1] "Estimated value for .Simulations.Replacements.Wheat.Leaf.Photosynthesis.RUE.FixedValue :  1.6"
## [1] "Minimum value of the criterion: -9.66022388221585"
#' 
#' Complementary graphs and data are stored in the results folder which path is given above. Among them, the `EstimatedVSinit.pdf` file contains the following figures: 
#' 
#' 
## ----eval=TRUE, echo=FALSE, out.width = '50%'-----------------------------------------------------------------------------------------------------------------

knitr::include_graphics("ResultsSimpleCaseApsim/estimInit_ExtinctionCoeff.png")

knitr::include_graphics("ResultsSimpleCaseApsim/estimInit_RUE.png")


#' 
#' Figure 1:  plots of estimated vs initial values of parameters *ExtinctionCoeff* and *RUE*. Numbers represent the repetition number of the minimization. The number in red, 2 in this case, is the minimization that lead to the minimal value of the criterion among all repetitions. In this case, minimizations converge towards different values for the parameters (3 for *ExtinctionCoeff* and 2 for *RUE*), which indicates the presence of local minima. Values of *RUE* are very close to the upper bound value. In realistic calibration cases this may indicate the presence of a large error in the observation values or in the simulated output values (this simple case with only one situation does not allow to derive such conclusion).
#' 
#' 
#' The optim_options$path_results folder also contains the optim_results.Rdata file that store the nlo variable, a list containing the results of the minimization for each repetition. If we print it for repetition 2 ...
## ----eval=FALSE, echo=TRUE------------------------------------------------------------------------------------------------------------------------------------
## load(file.path(optim_options$path_results,"optim_results.Rdata"))
## nlo[[2]]

#' 
#' ... this returns:
## ----echo=FALSE, eval=TRUE------------------------------------------------------------------------------------------------------------------------------------
load(file.path("ResultsSimpleCaseApsim","optim_results.Rdata"))
print(nlo[[2]])

#' 
#' 
#' ## Run the model after optimization
#' 
#' In this case, the `param_values` argument is set so that estimated values of the parameters overwrite the values defined in the model input file ('.apsimx`).
#' 
## ----results='hide', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------

sim_after_optim=apsimx_wrapper(param_values=optim_results$final_values,
                               model_options=model_options)


#' 
#' ## Plot the results
#' 
## ----results='hide', message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------
png(file.path(optim_options$path_results,"sim_obs_plots.png"),
    width = 15, height = 10, units = "cm", res=1000)
par(mfrow = c(1,2))

# Simulated and observed LAI before optimization
Ymax=max(max(obs_list[[sit_name]][,var_name], na.rm=TRUE),
         max(sim_before_optim$sim_list[[1]][[sit_name]][,var_name], na.rm=TRUE))
plot(sim_before_optim$sim_list[[1]][[sit_name]][,c("Date",var_name)],type="l",
     main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[sit_name]]$Date,obs_list[[sit_name]][[var_name]],col="red")
plot(sim_after_optim$sim_list[[1]][[sit_name]][,c("Date",var_name)],type="l",
     main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[sit_name]]$Date,obs_list[[sit_name]][[var_name]],col="red")

dev.off()

#' 
#' this gives: 
## ----eval=TRUE, echo=FALSE, message=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated and observed target variable before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSimpleCaseApsim/sim_obs_plots.png")

#' 
#' 
#' 
