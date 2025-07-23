context("Test the run_protocol_agmip function using a toy model")

# seems that testthat is not able to load the package automatically
# which prevent the use of CroPlotR summary function in run_protocol_agmip
library(CroPlotR)

# Define a toy model and its wrapper
toymodel <- function(nb_days, year, rB = 0.1, Bmin = 0.1, Bmax = 10, h = 0.5, Bini = 0.01) {
  # Simulate biomass and yield with a simple logistic model
  #
  # Arguments
  #   - nb_days: number of days to simulate
  #   - year: year of simulation (used to add a year effect on biomass)
  #   - rB: growth rate
  #   - Bmin: minimum biomass
  #   - Bmax: maximum biomass
  #   - h: harvest rate
  #   - Bini: initial biomass
  #
  # Value
  #   A list containing the simulated biomass and yield
  #
  set.seed(year)
  biom <- numeric(nb_days)
  biom[1] <- Bini
  for (i in 2:nb_days) {
    biom[i] <- biom[i - 1] + rB * (biom[i - 1] + Bmin) * (1 - biom[i - 1] / Bmax)
    biom[i] <- biom[i] + biom[i] * rnorm(1, mean = 0, sd = 10) / 100
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
    year <- as.numeric(strsplit(sit, "_")[[1]][[2]])

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
      c(nb_days = length(date_sequence), year, as.list(param_values))
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
  .GlobalEnv$model_options <- tibble(
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

# Test the wrapper
test_that("Wrapper OK", {
  res <- test_wrapper(
    model_function = toymodel_wrapper,
    model_options = model_options,
    param_values = c(rB = 0.15, h = 0.55),
    situation = c("sit1_2001")
  )
  expect_true(all(res$test_results))
})

# Test synthetic observations
test_that("Synthetic obs", {
  expect_true(CroptimizR:::is.obs(obs_synth))
})


# First AgMIP protocol test
test_that("First AgMIP protocol test", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 7)
  )
  steps <- list(
    biomass = list(
      param = c("rB"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass")
    ),
    yield = list(
      param = c("h"),
      obs_var = c("yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = tempdir(),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$nb_param_per_var[["biomass"]], 1)
  expect_equal(res$nb_obs_per_var[["biomass"]], 74)
  expect_equal(res$nb_param_per_var[["yield"]], 1)
  expect_equal(res$nb_obs_per_var[["yield"]], 74)

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$init_values[["h"]][[1]]
  )

  # Check that Bmax is kept to its default value
  expect_equal(
    res$forced_param_values[["Bmax"]],
    param_info$Bmax$default
  )

  # Check that estimated values for parameters are close to true values
  expect_equal(res$final_values[["rB"]],
    param_true_values[["rB"]],
    tolerance = param_true_values[["rB"]] * 1e-2
  )
  expect_equal(res$final_values[["h"]],
    param_true_values[["h"]],
    tolerance = param_true_values[["h"]] * 1e-2
  )
})


# Same but Bmax in set as candidate in second step and with a default value far from its true value
# so that i) in step6 the estimated value for rB compensate this error,
#         ii) Bmax is selected in second step, and,
#         iii) hopefully, step7 will lead to estimated values close to the true values for all parameters
test_that("Test efficiency of step7 in case a parameter is estimated late in step6", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 15) # default set to the bound to be sure the candidate is selected
  )
  steps <- list(
    biomass = list(
      param = c("rB"),
      obs_var = c("biomass")
    ),
    yield = list(
      param = c("h"),
      candidate_param = c("Bmax"),
      obs_var = c("yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = tempdir(),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$nb_param_per_var[["biomass"]], 1)
  expect_equal(res$nb_obs_per_var[["biomass"]], 74)
  expect_equal(res$nb_param_per_var[["yield"]], 2)
  expect_equal(res$nb_obs_per_var[["yield"]], 74)

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$init_values[["h"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["Bmax"]],
    res$init_values[["Bmax"]][[1]]
  )

  # Check that estimated values for parameters are close to true values
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

  # Check that the final value of all parameters are closer to the true values than the one at step6
  expect_lt(
    abs(res$final_values[["h"]] - param_true_values[["h"]]),
    abs(res$step6$final_values[["h"]] - param_true_values[["h"]])
  )
  expect_lt(
    abs(res$final_values[["rB"]] - param_true_values[["rB"]]),
    abs(res$step6$final_values[["rB"]] - param_true_values[["rB"]])
  )
  expect_lt(
    abs(res$final_values[["Bmax"]] - param_true_values[["Bmax"]]),
    abs(res$step6$final_values[["Bmax"]] - param_true_values[["Bmax"]])
  )
})



# Check that the weight computation is correct for complex cases such as:
#  * use of the same variable in different steps
#  * obs variable used in step7 but not in step6
# Check that step7 is efficient for variables not included in step6
test_that("Check use of the same variable in different steps and obs variable used in step7 but not in step6", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-3,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 15) # default set to the bound to be sure the candidate is selected
  )
  steps <- list(
    biomass = list(
      param = c("rB", "Bmax"),
      obs_var = c("biomass")
    ),
    biomass = list(
      param = c("h"),
      obs_var = c("biomass")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = tempdir(),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$nb_param_per_var[["biomass"]], 3)
  expect_equal(res$nb_obs_per_var[["biomass"]], 74)
  expect_equal(res$nb_param_per_var[["yield"]], 0)
  expect_equal(res$nb_obs_per_var[["yield"]], 74)

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$init_values[["h"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["Bmax"]],
    res$init_values[["Bmax"]][[1]]
  )

  # Check that estimated values for parameters are close to true values
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

  # Check that the final value of h is closer to the true value than the one at step6
  expect_lt(
    abs(res$final_values[["h"]] - param_true_values[["h"]]),
    abs(res$step6$final_values[["h"]] - param_true_values[["h"]])
  )
})


# Check that the weight computation is correct for complex cases such as:
#  * several observed variables per group
test_that("Check use of several observed variables per group", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 15), # default set to the bound to be sure the candidate is selected
    dummy = list(lb = 0, ub = 1, default = 0.5) # just for defining a second step
  )
  steps <- list(
    common_step = list(
      param = c("rB", "h"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass", "yield")
    ),
    dummy_step = list( # just because the protocol does not work with only one step ...
      param = c("dummy"),
      obs_var = c("biomass", "yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = tempdir(),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$nb_param_per_var[["biomass"]], 4)
  expect_equal(res$nb_obs_per_var[["biomass"]], 74 * 2)
  expect_equal(res$nb_param_per_var[["yield"]], 4)
  expect_equal(res$nb_obs_per_var[["yield"]], 74 * 2)

  # Check that values of weights for biomass and yield are equal
  expect_equal(
    res$weight[["biomass"]],
    res$weight[["yield"]]
  )

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$init_values[["h"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["Bmax"]],
    res$init_values[["Bmax"]][[1]]
  )

  # Check that estimated values for parameters are close to true values
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
