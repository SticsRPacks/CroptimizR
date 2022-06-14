# CroptimizR 0.4.0 _2021-07-22_

## Main changes

* Additional information provided in output of estim_param function: parameters' values proposed by the estimation method and associated criterion values, model results ... The level of information returned is controlled by a new argument (info_level)
* Exported new functions to plot the evolution of parameters and criterion values in function of iterations/evaluations (see vignette [https://sticsrpacks.github.io/CroptimizR/articles/Parameter_estimation_simple_case.html])
* Fix: management of param_info$sit_list for only one parameter did not work

# CroptimizR 0.3.0 (2021-06-09)

## Main changes

* Added a new argument in estim_param to force (non-estimated) parameters values during estimation
* Changed estim_init graph for a bubble graph
* Added a function for testing model wrappers (test_wrapper.R)
* Changed initial values sampling method from simple LHS to genetic LHS
* Added checks of observations and simulations
* Updated vignettes to use CroPlotR package
* Improved doc for buildng model wrappers
* A few bugs fixed
* Moved from Travis to GitHub actions for continuous integration tests


# CroptimizR 0.1.0 (2020-10-06)

## Main changes

* Simplified the required interface for model wrappers (i.e. shape of input and output arguments, see the Guidelines for implementing a crop model R wrapper for CroptimizR)
* Added a new argument var_names to estim_param function for requiring the model wrappers to simulate non-observed variables (may be useful if transform_sim / transform_obs arguments are used)

# CroptimizR 0.1.0 (2020-10-06)

## Main changes

* new available criterion: Ordinary Least Square
* new arguments transform_obs and transform_sim in estim_param function for dynamic transformation of observations and/or simulations
* new argument satisfy_par_const in estim_param function for defining contraints on the parameters to estimate (e.g. inequality constraints between parameters)
* estim_param now displays total elapse time
* some bugs fixed
* license changed to CeCILL-C
