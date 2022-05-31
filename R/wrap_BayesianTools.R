#' @title A wrapper for BayesianTools package
#'
#' @inheritParams optim_switch
#'
#' @return prints, graphs and a list containing:
#'
#' @keywords internal
#'
#' @importFrom BayesianTools applySettingsDefault createUniformPrior createBayesianSetup runMCMC marginalPlot correlationPlot gelmanDiagnostics getSample MAP
#'

wrap_BayesianTools <- function(optim_options,param_info,crit_options) {

  if (is.null((ranseed <- optim_options$ranseed))) { ranseed <- NULL }
  if (is.null(optim_options$iterations)) {
    stop("The total number of iterations of the Bayesian method used is missing: please provide it in optim_options$iterations.")
  }
  if (is.null((optim_options$startValue))) { optim_options$startValue <- 3 }
  if (!is.numeric(optim_options$startValue)) {
    stop("startValue should be the number of markov chains. Please use param_info$init_values to prescribe initial values for the parameters.")
  }
  if (is.null((path_results <- optim_options$path_results))) { path_results <- getwd() }
  if (is.null((optim_options$method))) {
    method <- "DREAMzs"
  } else {
    method <- optim_options$method
    optim_options <- within(optim_options,rm("method"))
  }

  # return requested information if only optim_options is given in argument
  if (nargs()==1 & methods::hasArg(optim_options)) {
    return(list(package="BayesianTools", family="Bayesian",
                method=method, init_values_nb=optim_options$startValue))
  }



  crit_options$tot_max_eval <- optim_options$iterations + optim_options$startValue -
    optim_options$iterations %% optim_options$startValue
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  # Sample initial values and include user provided ones
  optim_options$startValue <- as.matrix(init_values)
  default <- applySettingsDefault(settings = NULL, sampler = method)

  # Put default values for options not set by the user
  sapply(names(default),function(x) {if (is.null(optim_options[[x]])) {optim_options[[x]]<<-default[[x]]}})

  # if burnin has not been set , set it by default to adaptation if it is set
  if (method=="DREAMzs") {
    if(optim_options$adaptation <1) optim_options$adaptation <- optim_options$adaptation*optim_options$iterations
    if (optim_options$burnin<optim_options$adaptation) {
      warning(paste0("burnin (=",optim_options$burnin,") < adaptation (=",optim_options$adaptation,")
                  => burnin is set equal to adaptation."))
      optim_options$burnin <- optim_options$adaptation
    }
  }

  # Don't pass CroptimizR options to BayesianTools ... this lead to an error ...
  optim_options_BT<-optim_options
  if (!is.null(optim_options$ranseed)) optim_options_BT<-within(optim_options_BT,rm("ranseed"))
  if (!is.null(optim_options$path_results)) optim_options_BT<-within(optim_options_BT,rm("path_results"))
  if (!is.null(optim_options$out_dir)) optim_options_BT<-within(optim_options_BT,rm("out_dir"))

  set.seed(ranseed)

  likelihood<-function(x) {return(main_crit(x,crit_options))}

  # Create the Bayesian setup if it is an initial run of the method
  bayesianSetup <- optim_options$PreviousResults
  if (is.null(bayesianSetup)) {

    prior <- createUniformPrior(lower=bounds$lb, upper=bounds$ub)
    bayesianSetup <- createBayesianSetup(likelihood = likelihood, prior=prior,
                                        names=param_names)

  } else {
    optim_options_BT <- within(optim_options_BT,rm("PreviousResults"))
  }

  # Perform the Bayesian analysis
  out <- runMCMC(bayesianSetup = bayesianSetup, sampler = method,
                 settings = optim_options_BT)

  # Get a sample of the posterior and associated statistics
  post_sample <- getSample(out,coda=FALSE)
  codaObject <- getSample(out, start = 1, coda = TRUE)  # thin=1
  tmp <- summary(codaObject)

  ## Save the results
  res <- list(statistics = tmp$statistics,
              quantiles = tmp$quantiles,
              MAP = MAP(out)$parametersMAP,
              post_sample=post_sample,
              out = out)

  return(res)

}
