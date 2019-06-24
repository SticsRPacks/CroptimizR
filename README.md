
<!-- README.md is generated from README.Rmd. Please edit that file -->

SticsOptimizR: An R package for parameter estimation, uncertainty and sensitivity analysis for the [STICS](https://www6.paca.inra.fr/stics_eng/) model <img src="man/figures/logo.png" alt="logo" width="150" align="right" />
==============================================================================================================================================================================================================================

Overview
--------

This package will allow the user to perform, on the Stics model:

-   Probabilistic Uncertainty analysis: evaluate the uncertainty of Stics outputs resulting from the propagation of the uncertainty of its inputs for a set of USMs

-   Sensitivity analysis: assess how the uncertainty of Stics outputs can be allocated to the uncertainty of its inputs / describe the effects of the variations of Stics inputs on its outputs, for a set of USMs,

-   Parameter estimation / model calibration: estimate the values of a selection of Stics parameters (soil, plant .) from observations of a set of Stics output variables on a set of USMs

The package is under intensive development, so you can fill an issue or request a feature [here](https://github.com/SticsRPacks/SticsOptimizR/issues) at any time.

Installation
------------

The development version from [GitHub](https://github.com/) can be installed with:

``` r
devtools::install_github("SticsRPacks/SticsOptimizR")
```

Or using the lightweight [remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOptimizR")
```

The package is tested routinely to pass all [CRAN](https://CRAN.R-project.org) tests using Travis-CI (linux) and AppVeyor (Windows), but it is not released to the CRAN servers because we believe SticsOptimizR users are not widespread enough to bother CRAN people and use their free server time.

Examples
--------

Toy examples will be given here ...

Example data
------------

Link to example data will be given here ...

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

Authors and acknowledgments
---------------------------

The STICS (Simulateur mulTIdisciplinaire pour les Cultures Standard, or multidisciplinary simulator for standard crops) model is a dynamic, generic and robust model aiming to simulate the soil-crop-atmosphere system. It was first developed in 1996 by INRA -the French National Institute for Agricultural research- by Nadine Brisson and Dominique Ripoche. An overview of the model is available [here](https://www6.paca.inra.fr/stics_eng/About-us/Stics-model-overview).

The SticsOptimizR package is developed by Samuel Buis, Michel Giner and the \[SticsOptimizR Team\] (<https://github.com/orgs/SticsRPacks/teams/sticsoptimizr>).
