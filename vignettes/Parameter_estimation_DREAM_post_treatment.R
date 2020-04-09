
#' 
#' 
#' The results printed in output on the R console are the following:
#' 
## ----eval=FALSE, echo=TRUE---------------------------------------------
## ## # # # # # # # # # # # # # # # # # # # # # # # # #
## ## ## MCMC chain summary ##
## ## # # # # # # # # # # # # # # # # # # # # # # # # #
## ##
## ## # MCMC sampler:  DREAMzs
## ## # Nr. Chains:  3
## ## # Iterations per chain:  2669
## ## # Rejection rate:  0.878
## ## # Effective sample size:  423
## ## # Runtime:  54194.02  sec.
## ##
## ## # Parameters
## ##            psf     MAP    2.5%  median   97.5%
## ## dlaimax  1.045   0.001   0.001   0.001   0.001
## ## durvieF1 1.000 289.864 213.438 311.226 398.462
## ## durvieF2 1.009 208.487 147.158 298.312 443.323
## ##
## ## ## DIC:  442.256
## ## ## Convergence
## ##  Gelman Rubin multivariate psrf:
## ##
## ## ## Correlations
## ##          dlaimax durvieF1 durvieF2
## ## dlaimax    1.000   -0.074   -0.059
## ## durvieF1  -0.074    1.000   -0.029
## ## durvieF2  -0.059   -0.029    1.000

#' 
#' The rejection rate is the rate of rejection of proposed values. According to (Vrugt, 2016), a value between 0.7 and 0.85 is usually indicative of good performance of a MCMC simulation method.
#' 
#' Effective sample size should be here the number of different values per chain for the parameter vector in the posterior sample.
#' 
#' Under section "parameters" are given statistics on the posterior sample.
#' MAP means Maximum A Posteriori. It is the values of the parameters among the posterior sample that lead to the maximal value of the posterior density. 
#' 
#' DIC is the Deviance information criterion. It is a commonly applied method to summarize the fit of an MCMC chain. More details about it can be found in the [vignette of the BayesianTools package](https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html).
#' 
#' The output value of `estim_param` contains:
#' * `statistics`, `quantiles` and `MAP`: statistics on the posterior sample,
#' * `post_sample`, a sample of the posterior distribution (excluding the burnin phase),
#' * `out`: the list returned by the package BayesianTools.
#' 
#' It is stored with complementary graphs in the `optim_options$path_results` folder. 
#' 
#' Among these graphs, gelmanDiagPlots.pdf plots the evolution of the Gelman diagnostic. According to (Vrugt, 2016) the algorithm is considered to have converged to the posterior distribution when all the values of this diagnostic are inferior to 1.2. In theory the sample of the posterior distribution should thus be taken only after this (WARNING: it is not the case here, neither in BayesianTools, this should be improved).
#' 
#' In this case, we obtain:
#' 
## ----eval=TRUE, echo=FALSE, out.width = '45%'--------------------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_durvieF1.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/Gelman_durvieF2.PNG")

#' Figure 1: Gelman diagnostic.
#' 
#' 
#' marginalPlots.pdf shows estimation of the prior (blue) and posterior (pink) densities:
#' 
## ----eval=TRUE, echo=FALSE, out.width = '45%'--------------------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_dlaimax.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_durvieF1.PNG")
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/densities_durvieF2.PNG")

#' Figure 2: Prior and posterior densities.
#' 
#' correlationPlots.pdf gives information about the correlation between parameters:
#' 
## ----eval=TRUE, echo=FALSE, out.width = '75%'--------------------------
knitr::include_graphics("ResultsSpecificVarietalDREAM/N10000/correlation_plot.PNG")

#' Figure 3: Correlation plot.
#' 
#' 
#' ## Launch a new estimation starting from previous results 
#' 
#' In case one wants to increase the number of iterations, because the algorithm has not yet converged or the posterior sample is considered to small, there is a possibility to re-launch the method from the point is has stopped using the `PreviousResults` option:
#' 
## ----results='hide', message=FALSE, warning=FALSE----------------------
optim_options$PreviousResults=optim_results$out
optim_options$iterations <- 1000 # Total number of new iterations
optim_results=estim_param(obs_list=obs_list,
                          crit_function=likelihood_log_ciidn,
                          model_function=stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          optim_method="BayesianTools.dreamzs",
                          param_info=param_info)

