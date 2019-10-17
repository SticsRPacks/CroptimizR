
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SticsOptimizR: An R package for parameter estimation, uncertainty and sensitivity analysis for the [STICS](https://www6.paca.inra.fr/stics_eng/) model <img src="man/figures/logo.png" alt="logo" width="150" align="right" />

## Overview

This package will allow the user to perform, on the Stics model:

  - Probabilistic Uncertainty analysis: evaluate the uncertainty of
    Stics outputs resulting from the propagation of the uncertainty of
    its inputs for a set of USMs

  - Sensitivity analysis: assess how the uncertainty of Stics outputs
    can be allocated to the uncertainty of its inputs / describe the
    effects of the variations of Stics inputs on its outputs, for a set
    of USMs,

  - Parameter estimation / model calibration: estimate the values of a
    selection of Stics parameters (soil, plant .) from observations of a
    set of Stics output variables on a set of USMs

The package is under intensive development, so you can fill an issue or
request a feature
[here](https://github.com/SticsRPacks/SticsOptimizR/issues) at any time.

## Installation

The development version from [GitHub](https://github.com/) can be
installed with:

``` r
devtools::install_github("SticsRPacks/SticsOptimizR")
```

Or using the lightweight
[remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOptimizR")
```

The package is tested routinely to pass all
[CRAN](https://CRAN.R-project.org) tests using Travis-CI (linux) and
AppVeyor (Windows), but it is not released to the CRAN servers because
we believe SticsOptimizR users are not widespread enough to bother CRAN
people and use their free server time.

## Planned features

### Probabilistic Uncertainty analysis

To evaluate the uncertainty of crop models outputs resulting from the
propagation of the uncertainty of their inputs for a given set of
simulated situations.

Main features:

  - inputs’ uncertainties will be described by the user using (usual)
    probability distributions or by users’ given sample (or a mixed of
    both depending on the parameters),

  - facilities will be provided to describe distributions of **groups of
    parameters** (soils, weather, farming practices, initial conditions)
    using map labelling techniques (i.e. distributions of identifiers of
    soil / weather / … files) ,

  - description of **joint probability distributions** including
    dependencies between parameters’ uncertainties should be provided,

  - tools for **probabilistic modeling** could be provided (e.g. to
    generate new values, from a sample given by the user, by assessing
    the probability distribution it is sampled from),

  - several methods wil be available to generate the numerical design of
    experiment: **full-factorial design, random sampling using different
    methods (LHS, Quasi-Monte Carlo …)**,

Results provided: numerical (moments, quantiles …) and graphical
(**histograms / density plots / boxplots / violins, correlation plots,
time series of quantiles for dynamic variables …**) description of the
sample of output variables obtained.

## Sensitivity analysis

To assess how the uncertainty of crop models outputs can be allocated to
the uncertainty of their inputs / to describe the effects of the
variations of crop models inputs on their outputs, for a given set of
simulated situations.

Main features:

  - inputs’ uncertainties will be described as for uncertainty analysis
    functionality,

  - several methods should be provided to cope with diverse users’
    needs:
    
      - **Screening methods** (e.g. Morris) for a quick assessment of
        the effect of numerous inputs,
    
      - **Importance measures**: correlation coefficients,
        Variance-based methods (Sobol, EFAST, PCE…), Moment independent
        methods, for a more precise assessment of their impact,
    
      - **Multivariate Sensitivity Analysis** methods to cope with
        temporal outputs,
    
      - **Graphical methods**: scatter plots, response surface, cobweb
        plots, …
    
      - Variants of these methods adapted to **dependent inputs** should
        be provided … (at least analysis per group of parameters)

Results provided: numerical and graphical representations (bar plots /
pie charts / temporal plots of sensitivity indices …) of the indices
depending on the method

## Parameter estimation / model calibration

To estimate the values of a selection of crop models parameters (soil,
plant …) from observations of a set of their output variables,

Main features:

  - procedures for **automatic selection of the parameters to estimate**
    from a given list (e.g. forward selection using criterions such as
    AIC, MSEP …)

  - facilities to describe and perform **calibration in different
    steps**: different parameters are estimated from different observed
    situations and variables at each step using different **forcings**
    (lai, phenological stages) and using the parameters values estimated
    at the preceding steps,

  - facilities to describe and take into account **prior information and
    constraints** (e.g. inequality constraints) on estimated parameters
    and output variables,

  - **facilities to transform model outputs** in case observations does
    not corresponds to the crop model outputs

  - **different types of criterion** to compare simulations and
    observations (i.e. of objective function to minimize) and different
    ways of combining these criteria for several variables:
    e.g. classical ordinary least-square, weighted-least-square,
    concentrated-likelihood for automatic estimation of error variances,
    criterion that take into account error correlations … (see
    e.g. Wallach et al., 2011)

  - different estimation methods:**frequentists (e.g. Nelder-Meade
    Simplex), Bayesians (e.g. multi-chain MCMC, SIR), global
    optimization (Evolutionnary Algorithms, GLUE), multi-objective
    methods, …**

  - methods for evaluating the **predictive performance** of the
    calibrated model (cross validation …)

Results provided:

  - estimated values and uncertainties of estimated parameters,

  - specific diagnostics depending on the estimation method used,

  - evaluation of the predictive performance of the calibrated model,

  - description of the calibration process followed.

## Examples

A simple example of model calibration is given in a
[vignette](https://SticsRPacks.github.io/SticsOptimizR/articles/Parameter_estimation_simple_case.html).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Authors and acknowledgments

The STICS (Simulateur mulTIdisciplinaire pour les Cultures Standard, or
multidisciplinary simulator for standard crops) model is a dynamic,
generic and robust model aiming to simulate the soil-crop-atmosphere
system. It was first developed in 1996 by INRA -the French National
Institute for Agricultural research- by Nadine Brisson and Dominique
Ripoche. An overview of the model is available
[here](https://www6.paca.inra.fr/stics_eng/About-us/Stics-model-overview).

The SticsOptimizR package is developed by Samuel Buis, Michel Giner and
the \[SticsOptimizR Team\]
(<https://github.com/orgs/SticsRPacks/teams/sticsoptimizr>).
