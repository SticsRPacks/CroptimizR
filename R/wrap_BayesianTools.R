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
  if (is.null(optim_options$thin)) { optim_options$thin=1 }
  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  optim_options_tmp=list()
  optim_options_tmp$iterations=optim_options$iterations
  optim_options_tmp$startValue=optim_options$startValue
  optim_options_tmp$thin=optim_options$thin

  nb_chains=optim_options$startValue
  if ( is.numeric(nb_chains) && (length(nb_chains)==1 || dim(nb_chains)==2) ) {
    if (length(nb_chains)==1) {
      nb_chains=optim_options$startValue
    } else {
      nb_chains=nrow(optim_options$startValue)
    }
  } else {
    stop("optim_options$startValue must be a numeric (number of chains or a matrix of parameters initial values")
  }

  # number of iterations (optim_options$iterations is the total number of evaluation, i.e. nb_chains*nb_iterations)
  nb_iterations=ceiling(optim_options$iterations/nb_chains)


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
    # also, Nbiteration must be > thin+50 otherwise coda::gelman.plot end with an error
    if ( nb_iterations>=(optim_options_tmp$thin+50) ) {
      grDevices::pdf(file = file.path(path_results,"gelmanDiagPlots.pdf") , width = 9, height = 9)
      gelmanDiagnostics(out, thin=optim_options_tmp$thin, plot = T)
      grDevices::dev.off()
    } else {
      gelmanDiagnostics(out, thin=optim_options_tmp$thin, plot = F)
      warning("Number of iterations in DREAM is too low to generate gelmanDiagPlots.pdf (should be superior to thin+50)")
    }
  }

  # Print results
  codaObject = getSample(out, start = 1, coda = TRUE)  # thin=1
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
