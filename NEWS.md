# CroptimizR 1.0.0

This release consolidates the core API and introduces support for standardized
calibration workflows, including the AgMIP Calibration Protocol and user-defined
sequential multi-step workflows.

## Main changes

- Support for **user-defined sequential multi-step calibration workflows**
- Implementation of the **AgMIP Calibration Protocol** (Wallach et al., 2024, 2025), fully integrated into CroptimizR, including ad-hoc diagnostics
- New high-level functions:
  - `run_protocol_agmip()` to execute AgMIP-compliant calibration workflows
  - `load_protocol_agmip()` to load and validate predefined calibration protocols

## Documentation

- New vignette dedicated to the AgMIP Calibration Protocol, illustrating a complete calibration workflow
- Improved consistency and standardization across documentation

## Fixes

- Minor bug fixes and internal consistency improvements
- Improved input validation and error handling in calibration workflows

## Related references

- Wallach D., Buis S., Seserman D.M., Palosuo T., Thorburn P., Mielenz H., Justes E., Kersebaum K.C., Dumont B., Launay M., Seidel S.J. 2024. A calibration protocol for soil-crop models. Environmental Modelling & Software, 180, 106147. https://doi.org/10.1016/j.envsoft.2024.106147
- Wallach, D., Kim, K. S., Hyun, S., Buis, S., Thorburn, P., Mielenz, H., Seidel, S., Alderman, P., Dumont, B., Fallah, M.H., Hoogenboom, G., Justes, E., KErsebaum, K.-C., Launay, M., Leolini, L., Mehmood, M.Z., Moriondo, M., Jing Q., Qian, B., Schultz, S., Palosuo, T. 2025. Evaluating the AgMIP calibration protocol for crop models; case study and new diagnostic tests. European Journal of Agronomy, 168: 127659. https://doi.org/10.1016/j.eja.2025.127659

# CroptimizR 0.7.0(2024-11-13)

## Main Changes

* Enabled support for special characters in parameter names (#18).
* Updated the bundled version of the **BayesianTools** package (#19).
* Enhanced `is_data` function to verify that the tested data structure is not empty (#21).
* Improved validation checks for the `weight` argument (#22).

## Documentation

* Added a LICENSE file (#15).

## Fixes

* Resolved an incompatibility between `forced_param_values` and `param_info$sit_list` (#14).
* Fixed error message when `NA` values are detected in simulated results (#16).
* Corrected the calculation of equality constraints when only one situation exists in `param_values` (#24).
* Addressed various warnings (#14).

## Miscellaneous

* Added a unit test for `crit_wls` (#22).


# CroptimizR 0.6.1  (2023-12-22)

* Fixed check of NA/Inf in simulated results
* Made estim_param stop properly in case of error during the estimation process

# CroptimizR 0.6.0  (2023-12-15)

## Main changes

* Added Weighted Least Squares in the list of available least squares criteria, and weight argument in the estim_param function to provide the weights to use,
* added the possibility to transform simulated and observed variables (e.g. log transformation), using a new argument transform_var,
* added the possibility to introduce equality constraints in argument forced_param_values of the estim_param function. This allows to dynamically compute the values of some parameters in function of others that are estimated.
* License changed for LGPL (CeCILL-C not recognized by CRAN :-( )

## Documentation

* Added comments to explain how to handle in model wrappers the simulated variables that do not depend on date,
* add information in estim_param function documentation on the way simulated and observed variables are compared in the parameter estimation process.

## Fixes

* Fixed test on presence of NA in model results within parameter estimation process (now test Inf or NA values only for variables and dates included in observations, no more in all model results),
* fixed generation of gelman plot (when using Bayesian methods) in case of more than 2 parameters estimated and thin not initialized in optim_options (crashed).

## Misc.

* Names of observed variables used are now stored and printed at the end of the parameter estimation process,
* shortened paths of stored results to solve warning at check,
* increase default value for maxeval so that nloptr never stop on maximum number of evaluation (simplex method).

# CroptimizR 0.5.1 (2023-01-06)

* Fixed cff file generation using github action. Now generated on each commit that modifies the Description file + on release.

* Minor changes to clean the code following CRAN checks procedure application.

# CroptimizR 0.5.0 (2022-11-09)

## Main changes

* A parameter selection procedure following AgMIP Calibration Phase III protocol has been implemented. See this [vignette](https://SticsRPacks.github.io/CroptimizR/articles/AgMIP_Calibration_Phenology_protocol.html) for more details.
* All frequentist diagnostics plots (estimated vs initial values of the parameters, evolution of the minimized criterion value and parameters values in function of the iterations of the minimization, scatter plots of values for couples of parameters) are now automatically generated.
* Added computation and display of elapsed time of model simulations.
* Homogenization of argument names between functions and wrt CroPlotR package.

## Documentation

* Added a vignette on the application of AgMIP Phase III protocol.
* Added a "Getting Started" vignette and revised README.
* Revised list of authors and citation file of the package.

## Fixes

* Collection of information (if info_lvel>1) done in case of model and/or method crash.
* Filter_obs fixed in case no obs found for some situations.
* Plot of minimized criterion evolution did not work in case of null or negative criterion.

## Misc.

* Code reformatting and R check warnings/notes solved for future submission to CRAN.


# CroptimizR 0.4.0 (2021-07-22)

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
