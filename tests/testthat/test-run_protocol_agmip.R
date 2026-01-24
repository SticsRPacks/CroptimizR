context("Test the run_protocol_agmip function using a toy model")

library(testthat)
library(CroPlotR)

# ------------------------------------------------------------------------

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
  .GlobalEnv$model_options <- dplyr::tibble(
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

# ------------------------------------------------------------------------

test_that("Test the wrapper", {
  res <- test_wrapper(
    model_function = toymodel_wrapper,
    model_options = model_options,
    param_values = c(rB = 0.15, h = 0.55),
    situation = c("sit1_2001")
  )
  expect_true(all(res$test_results))
})

# ------------------------------------------------------------------------

test_that("Test synthetic observations", {
  expect_true(CroptimizR:::is.obs(obs_synth))
})

# ------------------------------------------------------------------------

test_that("Basic test AgMIP protocol", {
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
      major_param = c("rB"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass")
    ),
    yield = list(
      major_param = c("h"),
      obs_var = c("yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test1"),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$step7$weights$p, c(1, 1))
  expect_equal(res$step7$weights$n, c(74, 74))

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6 and default values
  expect_equal(
    param_info$rB$default,
    res$step6$Step6.biomass$init_values[["rB"]][1]
  )
  expect_equal(
    param_info$h$default,
    res$step6$Step6.yield$init_values[["h"]][1]
  )
  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6 and default values
  expect_equal(
    c(res$step6$final_values[["rB"]], param_info$rB$default),
    res$step7$init_values[["rB"]][c(1, 2)]
  )
  expect_equal(
    c(res$step6$final_values[["h"]], param_info$h$default),
    res$step7$init_values[["h"]][c(1, 2)]
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

  # Check that Goodness-of-fit for biomass is better than default after biomass step
  # and at worse equal (or very close) after yield step and Step7
  stats_biomass <- res$stats_per_step[res$stats_per_step$variable == "biomass", ]
  expect_true(
    all(
      (stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"] <
        stats_biomass[["MSE"]][stats_biomass$step == "Default"]) &&
        ((stats_biomass[["MSE"]][stats_biomass$step == "Step6.yield"] <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) ||
          abs(stats_biomass[["MSE"]][stats_biomass$step == "Step6.yield"] -
            stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) < stats_biomass[["MSE"]][stats_biomass$step == "Step6.yield"] * 1e-6) &&
        ((stats_biomass[["MSE"]][stats_biomass$step == "Step7"] <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) ||
          abs(stats_biomass[["MSE"]][stats_biomass$step == "Step7"] -
            stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) < stats_biomass[["MSE"]][stats_biomass$step == "Step7"] * 1e-6)
    )
  )
  # Check that Goodness-of-fit for yield is better than default and than at biomass step
  # after yield step
  # and at worse equal (or very close) after Step7
  stats_yield <- res$stats_per_step[res$stats_per_step$variable == "yield", ]
  expect_true(
    all(
      (stats_yield[["MSE"]][stats_yield$step == "Step6.yield"] <
        stats_yield[["MSE"]][stats_yield$step == "Default"]) &&
        (stats_yield[["MSE"]][stats_yield$step == "Step6.yield"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.biomass"]) &&
        ((stats_yield[["MSE"]][stats_yield$step == "Step7"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.yield"]) ||
          abs(stats_yield[["MSE"]][stats_yield$step == "Step7"] -
            stats_yield[["MSE"]][stats_yield$step == "Step6.yield"]) < stats_yield[["MSE"]][stats_yield$step == "Step7"] * 1e-6)
    )
  )
})

# ------------------------------------------------------------------------

# Same but Bmax is set as candidate in second step and with a default value far from its true value
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
      major_param = c("rB"),
      obs_var = c("biomass")
    ),
    yield = list(
      major_param = c("h"),
      candidate_param = c("Bmax"),
      obs_var = c("yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test2"),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$step7$weights$p, c(1, 2))
  expect_equal(res$step7$weights$n, c(74, 74))

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$step7$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$step7$init_values[["h"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["Bmax"]],
    res$step7$init_values[["Bmax"]][[1]]
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

  # Check that Goodness-of-fit for biomass is
  #   - better than default after biomass step
  #   - worse after yield step than after biomass step
  #   - better than ever after step7
  stats_biomass <- res$stats_per_step[res$stats_per_step$variable == "biomass", ]
  expect_true(
    all(
      (stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"] <
        stats_biomass[["MSE"]][stats_biomass$step == "Default"]) &&
        (stats_biomass[["MSE"]][stats_biomass$step == "Step6.yield"] >
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) &&
        (stats_biomass[["MSE"]][stats_biomass$step == "Step7"] <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"])
    )
  )
  # Check that Goodness-of-fit for yield is better after each step
  stats_yield <- res$stats_per_step[res$stats_per_step$variable == "yield", ]
  expect_true(
    all(
      (stats_yield[["MSE"]][stats_yield$step == "Default"] >
        stats_yield[["MSE"]][stats_yield$step == "Step6.biomass"]) &&
        (stats_yield[["MSE"]][stats_yield$step == "Step6.biomass"] >
          stats_yield[["MSE"]][stats_yield$step == "Step6.yield"]) &&
        (stats_yield[["MSE"]][stats_yield$step == "Step6.yield"] >
          stats_yield[["MSE"]][stats_yield$step == "Step7"])
    )
  )
})

# ------------------------------------------------------------------------

# Check that the weight computation is correct for complex cases such as:
#  * use of the same variable in different steps
#  * obs variable used in step7 but not in step6
# Check that step7 is efficient for variables not included in step6
test_that("Check use of the same variable in different steps and obs variable used in step7 but not in step6", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-3,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 15) # default set to the bound to be sure the candidate is selected
  )
  steps <- list(
    biomass_1 = list(
      major_param = c("rB", "Bmax"),
      obs_var = c("biomass")
    ),
    biomass_2 = list(
      major_param = c("h"),
      obs_var = c("biomass")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test3"),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$step7$weights$p, c(3, 0))
  expect_equal(res$step7$weights$n, c(74, 74))

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$step7$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$step7$init_values[["h"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["Bmax"]],
    res$step7$init_values[["Bmax"]][[1]]
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

  # Check that Goodness-of-fit for biomass is
  #   - better than default after biomass_1 step
  #   - equal after biomass_1 and biomass_2 steps
  #   - at least not worse (or very close) after Step7
  stats_biomass <- res$stats_per_step[res$stats_per_step$variable == "biomass", ]
  expect_true(
    all(
      (stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass_1"] <
        stats_biomass[["MSE"]][stats_biomass$step == "Default"]) &&
        (abs(stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass_1"] -
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass_2"]) <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass_1"] * 1e-6) &&
        ((stats_biomass[["MSE"]][stats_biomass$step == "Step7"] <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass_1"]) ||
          abs(stats_biomass[["MSE"]][stats_biomass$step == "Step7"] -
            stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass_1"]) < stats_biomass[["MSE"]][stats_biomass$step == "Step7"] * 1e-6)
    )
  )
  # Check that Goodness-of-fit for yield is better after Step7 than after the other steps
  stats_yield <- res$stats_per_step[res$stats_per_step$variable == "yield", ]
  expect_true(
    all(
      (stats_yield[["MSE"]][stats_yield$step == "Step7"] <
        stats_yield[["MSE"]][stats_yield$step == "Default"]) &&
        (stats_yield[["MSE"]][stats_yield$step == "Step7"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.biomass_1"]) &&
        (stats_yield[["MSE"]][stats_yield$step == "Step7"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.biomass_2"])
    )
  )
})

# ------------------------------------------------------------------------

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
    main_step = list(
      major_param = c("rB", "h"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass", "yield")
    ),
    dummy_step = list( # just because the protocol does not work with only one step ...
      major_param = c("dummy"),
      obs_var = c("biomass", "yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test4"),
    step = steps,
    param_info = param_info
  )

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$step7$weights$p, c(4, 4))
  expect_equal(res$step7$weights$n, c(74 * 2, 74 * 2))

  # Check that values of weights for biomass and yield are equal
  expect_equal(
    res$step7$weights$weight[[1]],
    res$step7$weights$weight[[2]]
  )

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$step7$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$step7$init_values[["h"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["Bmax"]],
    res$step7$init_values[["Bmax"]][[1]]
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

  # Check that Goodness-of-fit for biomass is
  #   - better than default after main_step step
  #   - at least not worse after Step7
  stats_biomass <- res$stats_per_step[res$stats_per_step$variable == "biomass", ]
  expect_true(
    all(
      (stats_biomass[["MSE"]][stats_biomass$step == "Step6.main_step"] <
        stats_biomass[["MSE"]][stats_biomass$step == "Default"]) &&
        ((stats_biomass[["MSE"]][stats_biomass$step == "Step7"] <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.main_step"]) ||
          abs(stats_biomass[["MSE"]][stats_biomass$step == "Step7"] -
            stats_biomass[["MSE"]][stats_biomass$step == "Step6.main_step"]) < stats_biomass[["MSE"]][stats_biomass$step == "Step7"] * 1e-6)
    )
  )
  # Check that Goodness-of-fit for yield is
  #   - better than default after main_step step
  #   - at least not worse (or very close) after Step7
  stats_yield <- res$stats_per_step[res$stats_per_step$variable == "yield", ]
  expect_true(
    all(
      (stats_yield[["MSE"]][stats_yield$step == "Step6.main_step"] <
        stats_yield[["MSE"]][stats_yield$step == "Default"]) &&
        ((stats_yield[["MSE"]][stats_yield$step == "Step7"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.main_step"]) ||
          abs(stats_yield[["MSE"]][stats_yield$step == "Step7"] -
            stats_yield[["MSE"]][stats_yield$step == "Step6.main_step"]) < stats_yield[["MSE"]][stats_yield$step == "Step7"] * 1e-6)
    )
  )

  # Check the order of the steps in the output
  expect_equal(stats_yield$step, c(
    "Default", "Step6.main_step", "Step6.dummy_step",
    "Step7"
  ))
})

# Test using transform_sim and var
test_that("Test using transform_sim and var", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-3,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 7)
  )
  steps <- list(
    biomass = list(
      major_param = c("rB"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass")
    ),
    yield = list(
      major_param = c("h"),
      obs_var = c("yield_kg")
    )
  )
  transform_var <- list(biomass = log)
  obs_list_kg <- obs_synth
  for (i in seq_along(obs_list_kg)) {
    if ("yield" %in% names(obs_list_kg[[i]])) {
      obs_list_kg[[i]]$yield_kg <- obs_list_kg[[i]]$yield * 1000 # to simulate a yield in kg/ha
      obs_list_kg[[i]]$yield <- NULL
    }
  }

  transform_sim <- function(model_results, obs_list, param_values, model_options) {
    for (i in seq_along(model_results$sim_list)) {
      if ("yield" %in% names(model_results$sim_list[[i]])) {
        model_results$sim_list[[i]]$yield_kg <- model_results$sim_list[[i]]$yield * 1000 # to simulate a yield in kg/ha
      }
    }
    return(model_results)
  }

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_list_kg,
    out_dir = file.path(tempdir(), "Test5"),
    step = steps,
    param_info = param_info,
    transform_var = transform_var,
    transform_sim = transform_sim
  )

  # Check the transformed sim and obs variable is used
  expect_true("yield_kg" %in% res$obs_var_list)
  expect_true("yield_kg" %in% res$step7$weights$variable)

  # Check the number of parameters and observations taken into account for weight computation
  expect_equal(res$step7$weights$p, c(1, 1))
  expect_equal(res$step7$weights$n, c(74, 74))

  # Check that initial values used for parameter estimation at step7 are equal to estimated values at end of step6
  expect_equal(
    res$step6$final_values[["rB"]],
    res$step7$init_values[["rB"]][[1]]
  )
  expect_equal(
    res$step6$final_values[["h"]],
    res$step7$init_values[["h"]][[1]]
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

  # Check that Goodness-of-fit for biomass is better than default after biomass step
  # and at worse equal (or very close) after yield step and Step7
  stats_biomass <- res$stats_per_step[res$stats_per_step$variable == "biomass", ]
  expect_true(
    all(
      (stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"] <
        stats_biomass[["MSE"]][stats_biomass$step == "Default"]) &&
        (stats_biomass[["MSE"]][stats_biomass$step == "Step6.yield"] <=
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) &&
        ((stats_biomass[["MSE"]][stats_biomass$step == "Step7"] <
          stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) ||
          abs(stats_biomass[["MSE"]][stats_biomass$step == "Step7"] -
            stats_biomass[["MSE"]][stats_biomass$step == "Step6.biomass"]) < stats_biomass[["MSE"]][stats_biomass$step == "Step7"] * 1e-6)
    )
  )
  # Check that Goodness-of-fit for yield is better than default and than at biomass step
  # after yield step
  # and at worse equal after Step7
  stats_yield <- res$stats_per_step[res$stats_per_step$variable == "yield_kg", ]
  expect_true(
    all(
      (stats_yield[["MSE"]][stats_yield$step == "Step6.yield"] <
        stats_yield[["MSE"]][stats_yield$step == "Default"]) &&
        (stats_yield[["MSE"]][stats_yield$step == "Step6.yield"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.biomass"]) &&
        ((stats_yield[["MSE"]][stats_yield$step == "Step7"] <
          stats_yield[["MSE"]][stats_yield$step == "Step6.yield"]) ||
          abs(stats_yield[["MSE"]][stats_yield$step == "Step7"] -
            stats_yield[["MSE"]][stats_yield$step == "Step6.yield"]) < stats_yield[["MSE"]][stats_yield$step == "Step7"] * 1e-6)
    )
  )
})

# ------------------------------------------------------------------------

# Test with a single step
test_that("Single step check", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1)
  )
  forced_param_values <- list(
    Bmax = 7,
    h = 0.55
  )
  steps <- list(
    biomass = list(
      major_param = c("rB"),
      obs_var = c("biomass")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test6"),
    step = steps,
    forced_param_values = forced_param_values,
    param_info = param_info
  )

  # Check that estimated values for parameters are close to true values
  expect_equal(res$final_values[["rB"]],
    param_true_values[["rB"]],
    tolerance = param_true_values[["rB"]] * 1e-2
  )
})

# ------------------------------------------------------------------------

test_that("Test AgMIP protocol with major param or candidate param set to NULL", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5)
  )
  forced_param_values <- list(
    Bmax = 7
  )
  steps <- list(
    biomass = list(
      major_param = NULL,
      candidate_param = c("rB"),
      obs_var = c("biomass")
    ),
    yield = list(
      major_param = c("h"),
      candidate_param = NULL,
      obs_var = c("yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test7"),
    step = steps,
    param_info = param_info,
    forced_param_values = forced_param_values
  )

  # Check that estimated parameters only include rB and h
  expect_equal(names(res$final_values), c("rB","h"))

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

# ------------------------------------------------------------------------

# Test obs variables included in obs_list but not used in step6 are used in step7
test_that("Test obs variables not used in step6 are used in step7", {
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
      major_param = c("rB"),
      obs_var = c("biomass")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test8"),
    step = steps,
    param_info = param_info
  )

  # Check that estimated parameters only include rB
  expect_equal(names(res$final_values), c("rB"))
  # Check that obs used include Yield for step7
  expect_true("yield" %in% res$step7$obs_var_list)
  # Check that obs used for step6 is biomass
  expect_equal(res$step6$obs_var_list, "biomass")
})

# ------------------------------------------------------------------------

# Test AgMIP protocol without default values for the parameters
test_that("Test AgMIP protocol without param default values", {
  optim_options <- list(
    nb_rep = 3, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1),
    h = list(lb = 0, ub = 1, default = 0.3),
    Bmax = list(lb = 5, ub = 15)
  )
  steps <- list(
    biomass = list(
      major_param = c("rB"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass")
    ),
    yield = list(
      major_param = c("h"),
      obs_var = c("yield")
    )
  )

  res <- run_protocol_agmip(
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    obs_list = obs_synth,
    out_dir = file.path(tempdir(), "Test9"),
    step = steps,
    param_info = param_info
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
