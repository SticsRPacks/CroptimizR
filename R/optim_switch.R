optim_switch <- function(param_names,obs_list,crit_function,model_function,model_options,optim_options,prior_information) {
  #' @title main function for criterion to optimize
  #'
  #' @param param_names Name(s) of parameters to force the value (optional)
  #' @param obs_list List of observed values
  #' @param crit_function Function implementing the criterion to optimize
  #'
  #' @return The value of the criterion
  #'
  #' @export
  #'
  #' @examples

  # Normalize parameters
  # TO DO

  # CALL optim method
  nb_rep=optim_options$rep_nb
  xtol_rel=optim_options$xtol_rel
  maxeval=optim_options$maxeval

  lb_vec=prior_information$lb
  ub_vec=prior_information$ub

  crit_options_loc=list()
  crit_options_loc$param_names=param_names
  crit_options_loc$obs_list=obs_list
  crit_options_loc$crit_function=crit_function
  crit_options_loc$model_function=model_function
  crit_options_loc$model_options=model_options

  # The initial parameters, the final parameters and the minimized criterion
  param_init <- list()
  param_fin <- list()
  all_crit <- vector("numeric")
  for (i in 1:length(lb_vec)){
    param_init[[i]] <- 0
    param_fin[[i]] <- 0
  }

  nlo <- list()
  for (z in 1:nb_rep){

    # random init values
    param_value_vec <- vector("numeric")
    for (i in 1:length(lb_vec)){
      param_value_vec[i] <- runif(1,lb_vec[i], ub_vec[i])
      param_init[[i]][z] <- param_value_vec[i]
    }

    nlo[[z]] <- nloptr(x0 = param_value_vec, eval_f = main_crit, lb = lb_vec, ub = ub_vec,
                       opts = list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=xtol_rel, "maxeval"=maxeval),
                       crit_options=crit_options_loc)

    for (y in 1:length(lb_vec)){
      param_fin[[y]][z] <- nlo[[z]]$solution[y]
    }
    all_crit[z] <- nlo[[z]]$objective
  }

  # Which repetion has the smallest criterion
  min_pos<-which.min(all_crit)

  # PDF
#  pdf(file = file.path(path_results,"EstimatedVSinit.pdf") , width = 9, height = 9, pointsize = 10)
  pdf(file = "EstimatedVSinit.pdf" , width = 9, height = 9, pointsize = 10)
  for (i in 1:length(param_init)) {
    plot(param_init[[i]], param_fin[[i]], main = "Estimated vs Initial values of the
         parameters for different repetitions", text(param_init[[i]], param_fin[[i]], pos=1,col="black"), xlim = c(lb_vec[i],ub_vec[i]),
         ylim =   c(lb_vec[i],ub_vec[i]), xlab = paste("Initial value for", param_names[i]),
         ylab = paste("Estimated value for", param_names[i]))
    text(param_init[[i]][min_pos], param_fin[[i]][min_pos], labels = min_pos, pos=1,col="red")
  }
  dev.off()

  # Display of parameters for the repetion who have the smallest criterion
  for (nb_param in 1:length(param_fin)){
    print(paste("Estimated value for", param_names[nb_param], ": ", param_fin[[nb_param]][min_pos]))
  }
  print(paste("Minimum value of the criterion :", all_crit[min_pos]))

}
