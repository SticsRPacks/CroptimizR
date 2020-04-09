#'
#' The results printed in output on the R console are the following:
## ----echo=TRUE, eval=FALSE----------------------------------------
## [1] "Estimated value for dlaimax :  0.00169614928696274"
## [1] "Estimated value for durvieF :  53.9691276907021"
## [1] "Minimum value of the criterion: 4.72322279544591"
#'
#'
#' Complementary graphs and data are stored in the `optim_options$path_results` folder. Among them, the EstimatedVSinit.pdf file containing the following figures:
#'
## ----eval=TRUE, echo=FALSE, out.width = '45%'---------------------

knitr::include_graphics("ResultsSimpleCase/estimInit_dlaimax.PNG")

knitr::include_graphics("ResultsSimpleCase/estimInit_durvieF.PNG")


#'
#' Figure 1: plots of estimated vs initial values of parameters dlaimax and durvieF. Numbers represent the repetition number of the minimization. The number in red, 2 in this case, is the minimization that lead to the minimal value of the criterion among all repetitions. In this case, the minimizations converge towards 2 different values for the parameters, which indicates the presence of a local minimum. Values of durvieF are close to the bounds. In realistic calibration cases with many observed situations / variables / dates this may indicate the presence of biases in the observation values or in the model output values simulated (this simple case with only one situation does not allow to derive such conclusion).
#'
#' The `optim_options$path_results` folder also contains the optim_results.Rdata file that store the `nlo` variable, a list containing the results of the minimization for each repetition. If we print it for repetition 2 ...
## ----eval=FALSE, echo=TRUE----------------------------------------
## load(file.path(optim_options$path_results,"optim_results.Rdata"))
## nlo[[2]]

#' ... this returns:
## ----echo=FALSE, eval=TRUE, warning=FALSE----------------------------------------
load(file.path("ResultsSimpleCase","optim_results.Rdata"))
print(nlo[[2]])

#'
#' `nlo` is a list of size the number of repetitions. Each element stores many information about the corresponding minimization,
#' including the optimal values of the parameters (field `solution`) and the message indicating why the minimization stopped (field `message`).
#' `nlo` is also returned in output of `estim_param`.
#'
#'
#' ## Run the model after optimization
#'
#' We run here the Stics model using the estimated values of the parameters.
#' In this case, the `param_values` argument of the stics_wrapper function is thus set so that estimated values of the parameters overwrite the values defined in the model input files.
#'
## ----message=FALSE, warning=FALSE---------------------------------
sim_after_optim=stics_wrapper(param_values=optim_results$final_values,
                              model_options=model_options)

#'
#' ## Plot the results
#'
#'
## ----results='hide', warning=FALSE, message=FALSE-----------------
png(file.path(optim_options$path_results,"sim_obs_plots.png"),
    width = 15, height = 10, units = "cm", res=1000)
par(mfrow = c(1,2))

# Simulated and observed LAI before optimization
Ymax=max(max(obs_list[[sit_name]][,var_name], na.rm=TRUE),
         max(sim_before_optim$sim_list[[1]][[sit_name]][,var_name], na.rm=TRUE))
plot(sim_before_optim$sim_list[[1]][[sit_name]][,c("Date",var_name)],type="l",
     main="Before optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[sit_name]],col="green")

# Simulated and observed LAI after optimization
plot(sim_after_optim$sim_list[[1]][[sit_name]][,c("Date",var_name)],type="l",
     main="After optimization",ylim=c(0,Ymax+Ymax*0.1))
points(obs_list[[sit_name]],col="green")

dev.off()

#'
#' This gives:
#'
## ----eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE, out.width = '80%', fig.cap="Figure 2: plots of simulated and observed target variable before and after optimization. The gap between simulated and observed values has been drastically reduced: the minimizer has done its job!"----
knitr::include_graphics("ResultsSimpleCase/sim_obs_plots.png")

#'
