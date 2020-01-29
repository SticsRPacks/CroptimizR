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

wrap_BayesianTools <- function(param_names,obs_list,crit_function,model_function,model_options=NULL,optim_options=NULL,param_info) {

  if (is.null((ranseed=optim_options$ranseed))) { ranseed=NULL }
  if (is.null((path_results=optim_options$path_results))) { path_results=getwd() }

  default=applySettingsDefault(settings = NULL, sampler = "DREAMzs")
  # if burnin has not been set , set it by default to adaptation if it is set
  if (is.null((optim_options$thin))) { optim_options$thin=default$thin } # useful for post-treatment
  if (is.null((optim_options$adaptation))) { optim_options$adaptation=default$adaptation }
  if (is.null((optim_options$burnin))) { optim_options$burnin=default$burnin }
  if(optim_options$adaptation <1) optim_options$adaptation <- optim_options$adaptation*optim_options$iterations
  if (optim_options$burnin<optim_options$adaptation) {
    warning(paste0("burnin (=",optim_options$burnin,") < adaptation (=",optim_options$adaptation,")
                  => burnin is set equal to adaptation."))
    optim_options$burnin=optim_options$adaptation
  }

  # Don't pass CroptimizR options to BayesianTools ... this lead to an error ...
  optim_options_DREAMzs=within(optim_options,rm("ranseed","path_results"))

  set.seed(ranseed)
  nb_params=length(param_names)

  crit_options_loc=list()
  crit_options_loc$param_names=param_names
  crit_options_loc$obs_list=obs_list
  crit_options_loc$crit_function=crit_function
  crit_options_loc$model_function=model_function
  crit_options_loc$model_options=model_options
  crit_options_loc$param_info=param_info
  likelihood<-function(x) {return(main_crit(x,crit_options_loc))}

  # Create the Bayesian setup if it is an initial run of the method
  bayesianSetup=optim_options$PreviousResults
  if (is.null(bayesianSetup)) {

    bounds=get_params_bounds(param_info)
    prior=createUniformPrior(lower=bounds$lb, upper=bounds$ub)
    bayesianSetup = createBayesianSetup(likelihood = likelihood, prior=prior,
                                        names=param_names)

  }

  # Perform the Bayesian analysis
  out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DREAMzs",
                 settings = optim_options_DREAMzs)

  # Get a sample of the posterior
  post_sample=getSample(out,coda=FALSE)

  # Useful initializations for post-treatments
  nb_chains=length(out$chain)
  ## number of iterations (optim_options$iterations is the total number of evaluation, i.e. nb_chains*nb_iterations)
  nb_iterations=nrow(post_sample)/nb_chains

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
    if ( nb_iterations>=(optim_options_DREAMzs$thin+50) ) {
      grDevices::pdf(file = file.path(path_results,"gelmanDiagPlots.pdf") , width = 9, height = 9)
      gelmanDiagnostics(out, thin=optim_options_DREAMzs$thin, plot = T)
      grDevices::dev.off()
    } else {
      gelmanDiagnostics(out, thin=optim_options_DREAMzs$thin, plot = F)
      warning(paste0("Number of iterations in DREAM after burnin phase is too low (",nb_iterations,") to generate gelmanDiagPlots.pdf (should be superior to thin+50)"))
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
              post_sample=post_sample,
              out = out)
  save(res, file = file.path(path_results,"optim_results.Rdata"))
  return(res)

}
