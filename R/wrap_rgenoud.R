#' @title A wrapper for rgenoud package
#'
#' @description This function wraps the rgenoud package
#'
#' @inheritParams optim_switch
#'
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
#'
#' @return prints, graphs and a list containing:
#' `final_values`, the vector of estimated values for optimized parameters
#' for the repetition that lead to the lowest value of the criterion
#' `init_values`, the vector of initial values for optimized parameters
#' `min_crit_value`, the minimum value of the criterion
#' `res_genoud`, the data structure returned by the genoud function
#'
#'@keywords internal
#'
#'

wrap_rgenoud <- function(param_names,optim_options,param_info,crit_options,...) {

  if (is.null((solution.tolerance=optim_options$solution.tolerance))) { solution.tolerance=1e-4 }
  if (is.null((max.generations=optim_options$max.generations))) { max.generations=500 }
  if (is.null((pop.size=optim_options$pop.size))) { pop.size=300 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null((lexical=optim_options$lexical))) { lexical=FALSE }
  if (is.null((print.level=optim_options$print.level))) { print.level=2 }
  if (is.null((P1=optim_options$P1))) { P1=50 }
  if (is.null((P2=optim_options$P2))) { P2=50 }
  if (is.null((P3=optim_options$P3))) { P3=50 }
  if (is.null((P4=optim_options$P4))) { P4=50 }
  if (is.null((P5=optim_options$P5))) { P5=50 }
  if (is.null((P6=optim_options$P6))) { P6=50 }
  if (is.null((P7=optim_options$P7))) { P7=50 }
  if (is.null((P8=optim_options$P8))) { P8=50 }
  if (is.null((P9=optim_options$P9))) { P9=0 }
  if (is.null((P9mix=optim_options$P9mix))) { P9mix=NULL }
  if (is.null((gradient.check=optim_options$gradient.check))) { gradient.check=TRUE }

  if (is.null((debug=optim_options$debug))) { debug=TRUE }
  if (is.null((BFGSburnin=optim_options$BFGSburnin))) { BFGSburnin=0 }

  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  if (is.null((optim.method=optim_options$optim.method))) { optim.method="BFGS" }
  if (!is.null(ranseed)) set.seed(ranseed)

  nb_params=length(param_names)
  bounds=get_params_bounds(param_info)
  user_init_values=get_params_init_values(param_info)

  # Sample initial values and include user provided ones
  init_values=sample_params(param_info,pop.size,ranseed)

  for (param in param_names) {
    idx=which(!is.na(user_init_values[,param]))
    init_values[idx,param]=user_init_values[idx,param]
  }

  if (is.null((starting.values=optim_options$starting.values)))
    { starting.values=init_values }

  domains=matrix(c(bounds$lb,bounds$ub),length(bounds$lb),2)

  ##### Enleve le "loop" car c'est pas chercher minimum local comme nloptr

  try(res_genoud<- rgenoud::genoud(fn = main_crit
                                  ,nvars = nb_params
                                  ,boundary.enforcement=2
                                  ,max=FALSE
                                  ,Domains = domains
                                  #,instance.number= 0
                                  ,starting.values = starting.values
                                  ,crit_options = crit_options
                                  ,solution.tolerance = solution.tolerance
                                  ,optim.method = optim.method
                                  ,pop.size = pop.size
                                  ,max.generations = max.generations
                                  ,lexical = lexical
                                  ,print.level=print.level
                                  ,project.path=NULL
                                  ,P1=P1
                                  ,P2=P2, P3=P3, P4=P4, P5=P5, P6=P6, P7=P7
                                  ,P8=P8, P9=P9, P9mix=P9mix
                                  ,BFGSburnin=BFGSburnin
                                  ,gradient.check = gradient.check
                                  ))


  # Get the estimated values
  est_values=t(res_genoud$par)
  names(est_values) <- param_names

  # Display of parameters for the repetition which has the smallest criterion
  for (ipar in 1:nb_params) {
    print(paste("Estimated value for", param_names[ipar], ": ", est_values[ipar]))
  }
  print(paste("Minimum value of the criterion:", res_genoud$value))
  print(paste("Complementary graphs and results can be found in ", path_results))

  res <- list(final_values = est_values,
              init_values = init_values,
              min_crit_value = res_genoud$value,
              res_genoud = res_genoud)


  # Save the results of genoud
  save(res, file = file.path(path_results,"optim_results.Rdata"))


  return(res)

}


















