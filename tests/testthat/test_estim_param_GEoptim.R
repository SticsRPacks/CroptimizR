context("Test the estim_param function using a toy model")
library(tibble)

# Define a toy model and its wrapper
toymodel <- function(nb_days, rB = 0.1, Bmin = 0.1, Bmax = 10, h = 0.5, Bini = 0.01) {
  # Simulate biomass and yield with a simple logistic model
  #
  # Arguments
  #   - rB: growth rate
  #   - Bmin: minimum biomass
  #   - Bmax: maximum biomass
  #   - h: harvest rate
  #   - Bini: initial biomass
  #
  # Value
  #   A list containing the simulated biomass and yield
  #
  biom <- numeric(nb_days)
  biom[1] <- Bini
  for (i in 2:nb_days) {
    biom[i] <- biom[i - 1] + rB * (biom[i - 1] + Bmin) * (1 - biom[i - 1] / Bmax)
  }
  yield <- h * biom
  return(list(biom = biom, yield = yield))
}

toymodel_wrapper <- function(param_values = NULL, situation,
                             model_options, ...) {
  # A wrapper for toymodel
  #
  # Arguments
  #   - param_values: (optional) a named vector or a tibble containing the
  #     values of the toymodel parameters to force.
  #   - situation: Vector of situations names for which results must be
  #     returned.
  #     In this case, the names of the situations are coded as "year_suffix"
  #   - model_options: a tibble containing the begin and end date of simulation
  #     for each situation.
  #   - ...: mandatory since CroptimizR will give additional arguments not used
  #     here
  #
  # Value:
  #   A named list of tibble per situation.
  #   Each tibble contains columns:
  #      - Date (POSIXct dates of simulated results),
  #      - One column per simulated variable (biomass and yield)
  #
  results <- list(
    sim_list = setNames(
      vector("list", length(situation)),
      nm = situation
    ),
    error = FALSE
  )
  attr(results$sim_list, "class") <- "cropr_simulation"

  # Remove dummy in param_values, just here for specific tests
  if (!is.null(param_values) && "dummy" %in% names(param_values)) {
    param_values <- param_values[!names(param_values) %in% "dummy"]
  }

  for (sit in situation) {
    # Retrieve begin and end date from situation name
    begin_date <- dplyr::filter(model_options, situation == sit) %>%
      dplyr::select(begin_date)
    end_date <- dplyr::filter(model_options, situation == sit) %>%
      dplyr::select(end_date)

    date_sequence <- seq(from = begin_date[[1]], to = end_date[[1]], by = "day")

    if (!all(names(param_values) %in% c(
      "rB", "Bmin", "Bmax", "h", "Bini"
    ))) {
      warning(
        paste(
          "Unknown parameters in param_values:",
          paste(names(param_values), collapse = ",")
        )
      )
      results$error <- TRUE
      return(results)
    }

    # Call the toymodel with varying arguments depending on what is given in
    # param_values
    res <- do.call(
      "toymodel",
      c(nb_days = length(date_sequence), as.list(param_values))
    )

    # Fill the result variable
    results$sim_list[[sit]] <-
      dplyr::tibble(
        Date = date_sequence,
        biomass = res$biom,
        yield = res$yield
      )
  }
  return(results)
}

# To make these function accessible to the test environment
.GlobalEnv$toymodel <- toymodel
.GlobalEnv$toymodel_wrapper <- toymodel_wrapper

# Use setup() to define shared data for all tests in this file
setup({
  # These variables will be available in all tests
  .GlobalEnv$model_options <- tibble::tibble(
    situation = c("sit1_2000", "sit1_2001", "sit2_2003", "sit2_2004"),
    begin_date = as.Date(c("2000-05-01", "2001-05-12", "2003-05-05", "2004-05-15")),
    end_date = as.Date(c("2000-11-05", "2001-11-20", "2003-11-15", "2004-11-18"))
  )
  .GlobalEnv$param_true_values <- c(rB = 0.08, h = 0.55, Bmax = 7)

  # Generate synthetic observations
  tmp <- toymodel_wrapper(
    situation = c("sit1_2000", "sit1_2001", "sit2_2003", "sit2_2004"),
    param_values = param_true_values,
    model_options = model_options
  )

  .GlobalEnv$obs_synth <- lapply(tmp$sim_list, function(x) {
    return(dplyr::filter(x, dplyr::row_number() %% 10 == 0))
  })
})

# For cleaning up after tests
teardown({
  if (exists("model_options", envir = .GlobalEnv)) rm(model_options, envir = .GlobalEnv)
  if (exists("param_true_values", envir = .GlobalEnv)) rm(param_true_values, envir = .GlobalEnv)
  if (exists("obs_synth", envir = .GlobalEnv)) rm(obs_synth, envir = .GlobalEnv)
})



# Test estim_param with DEoptim
test_that("estim_param 1 step OLS criterion", {
  param_info <- list(
    rB = list(lb = 0, ub = 1),
    h = list(lb = 0, ub = 1),
    Bmax = list(lb = 5, ub = 15)
  )

  optim_options <- list(
    ranseed = 1234,
    n_params     = length(param_info),
    n_iter       = 60,
    n_particles  = 30,
    n_diff       = 2,
    return_trace = TRUE
  )

  res <- estim_param(
    obs_list = obs_synth,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_method = "graDiEnt",
    crit_function = crit_ols,
    optim_options = optim_options,
    param_info = param_info,
    obs_var = c("biomass", "yield"),
    situation = c("sit1_2000", "sit1_2001", "sit2_2003"),
    out_dir = tempdir()
  )

  expect_equal(res$final_values[["rB"]],
               param_true_values[["rB"]],
               tolerance = param_true_values[["rB"]] * 1e-2
  )
  expect_equal(res$final_values[["h"]],
               param_true_values[["h"]],
               tolerance = param_true_values[["h"]] * 1e-2
  )
  expect_equal(res$final_values[["Bmax"]],
               param_true_values[["Bmax"]],
               tolerance = param_true_values[["Bmax"]] * 1e-2
  )
})
