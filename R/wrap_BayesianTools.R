#' @title A wrapper for BayesianTools package
#'
#' @inheritParams optim_switch
#' @param param_names Name(s) of parameters to estimate (a parameter name must
#' be replicated if several groups of situations for this parameter)
#'
#' @return prints, graphs and a list containing:
#'
#' @keywords internal
#'
#' @importFrom BayesianTools createUniformPrior createBayesianSetup runMCMC marginalPlot correlationPlot gelmanDiagnostics getSample MAP
#'

wrap_BayesianTools <- function(param_names,optim_options,param_info,crit_options) {

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

  likelihood<-function(x) {return(main_crit(x,crit_options))}

  # Create the Bayesian setup if it is an initial run of the method
  bayesianSetup=optim_options$PreviousResults
  if (is.null(bayesianSetup)) {

    bounds=get_params_bounds(param_info)
    prior=createUniformPrior(lower=bounds$lb, upper=bounds$ub)
    bayesianSetup = createBayesianSetup(likelihood = likelihood, prior=prior,
                                        names=param_names)

  } else {
    optim_options_DREAMzs=within(optim_options_DREAMzs,rm("PreviousResults"))
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

  ## Print results
  codaObject = getSample(out, start = 1, coda = TRUE)  # thin=1
  tmp=summary(codaObject)
  if (nb_params>=2) {
    summary(out) }
  else {
    print(tmp)
  }
  print(paste("Complementary graphs and results can be found in ", path_results))

  ## Save the results
  res <- list(statistics = tmp$statistics,
              quantiles = tmp$quantiles,
              MAP = MAP(out)$parametersMAP,
              post_sample=post_sample,
              out = out)
  save(res, file = file.path(path_results,"optim_results.Rdata"))

  ## Graphs the results
  tryCatch(
    {
      grDevices::pdf(file = file.path(path_results,"iterAndDensityPlots.pdf") , width = 9, height = 9)
      graphics::plot(out)
      grDevices::dev.off()
    },
    error=function(cond) {
      warning("Error trying to create ",path_results,"/iterAndDensityPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will not be created.")
      message(cond)
      flush.console()
    })

  tryCatch(
    {
      grDevices::pdf(file = file.path(path_results,"marginalPlots.pdf") , width = 9, height = 9)
      marginalPlot(out)
      grDevices::dev.off()
    },
    error=function(cond) {
      warning("Error trying to create ",path_results,"/marginalPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will not be created.")
      message(cond)
      flush.console()
    })

  if (nb_params>=2) {
    tryCatch(
      {
        grDevices::pdf(file = file.path(path_results,"correlationPlots.pdf") , width = 9, height = 9)
        correlationPlot(out)
        grDevices::dev.off()
      },
      error=function(cond) {
        warning("Error trying to create ",path_results,"/correlationPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will not be created.")
        message(cond)
        flush.console()
      })
  }

  if (nb_params>=2) {
    # seems that it does not work for a single parameter
    # also, Nbiteration must be > thin+50 otherwise coda::gelman.plot end with an error
    if ( nb_iterations>=(optim_options_DREAMzs$thin+50) ) {
      tryCatch(
        {
          grDevices::pdf(file = file.path(path_results,"gelmanDiagPlots.pdf") , width = 9, height = 9)
          gelmanDiagnostics(out, thin=optim_options_DREAMzs$thin, plot = T)
          grDevices::dev.off()
        },
        error=function(cond) {
          warning("Error trying to create ",path_results,"/gelmanDiagPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will not be created.")
          message(cond)
          flush.console()
        })

    } else {
      gelmanDiagnostics(out, thin=optim_options_DREAMzs$thin, plot = F)
      warning(paste0("Number of iterations in DREAM after burnin phase is too low (",nb_iterations,") to generate gelmanDiagPlots.pdf (should be superior to thin+50)"))
    }
  }

  return(res)

}
