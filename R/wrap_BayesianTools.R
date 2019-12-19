#' @title A wrapper for BayesianTools package
#'
#' @inheritParams estim_param
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
#'
#' @return prints, graphs and a list containing:
#'
#' @keywords internal
#'
#' @importFrom BayesianTools createUniformPrior createBayesianSetup runMCMC marginalPlot correlationPlot gelmanDiagnostics getSample MAP
#'

wrap_BayesianTools <- function(param_names,obs_list,crit_function,model_function,model_options=NULL,optim_options=NULL,prior_information) {

  if (is.null(optim_options$iterations)) { optim_options$iterations=10000 }
  if (is.null(optim_options$startValue)) { optim_options$startValue=5 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  optim_options_tmp=list()
  optim_options_tmp$iterations=optim_options$iterations
  optim_options_tmp$startValue=optim_options$startValue

  set.seed(ranseed)

  nb_params=length(param_names)


  crit_options_loc=list()
  crit_options_loc$param_names=param_names
  crit_options_loc$obs_list=obs_list
  crit_options_loc$crit_function=crit_function
  crit_options_loc$model_function=model_function
  crit_options_loc$model_options=model_options
  crit_options_loc$prior_information=prior_information
  likelihood<-function(x) {return(main_crit(x,crit_options_loc))}


  # Create the Bayesian setup if it is an initial run of the method
  bayesianSetup=optim_options$PreviousResults
  if (is.null(bayesianSetup)) {

    bounds=get_params_bounds(prior_information)
    prior=createUniformPrior(lower=bounds$lb, upper=bounds$ub)
    bayesianSetup = createBayesianSetup(likelihood = likelihood, prior=prior,
                                        names=param_names)

  }

  # Perform the Bayesian analysis
  settings = list(iterations = 10000, startValue=5)
  out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DREAMzs",
                 settings = optim_options_tmp)

  # Generate graphs and prints
  grDevices::pdf(file = file.path(path_results,"iterAndDensityPlots.pdf") , width = 9, height = 9)
  graphics::plot(out)
  grDevices::dev.off()

  grDevices::pdf(file = file.path(path_results,"marginalPlots.pdf") , width = 9, height = 9)
  marginalPlot(out)
  grDevices::dev.off()

  if (nb_params>=2) {
    grDevices::pdf(file = file.path(path_results,"correlationPlots.pdf") , width = 9, height = 9)
    correlationPlot(out)
    grDevices::dev.off()
  }

  if (nb_params>=2) {
    # seems that it does not work for a single parameter
    grDevices::pdf(file = file.path(path_results,"gelmanDiagPlots.pdf") , width = 9, height = 9)
    gelmanDiagnostics(out, thin="auto", plot = T)
    grDevices::dev.off()
  }

  # Print results
  codaObject = getSample(out, start = 500, coda = TRUE)
  tmp=summary(codaObject)
  if (nb_params>=2) {
    summary(out) }
  else {
    print(tmp)
  }

  # Save and return the results
  res <- list(statistics = tmp$statistics,
              quantiles = tmp$quantiles,
              MAP = MAP(out)$parametersMAP,
              out = out)
  save(res, file = file.path(path_results,"optim_results.Rdata"))
  return(res)

}
