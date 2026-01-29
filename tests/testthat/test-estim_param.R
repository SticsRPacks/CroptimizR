context("Test the estim_param function using a toy model")

library(testthat)

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

test_that("Test estim_param 1 step OLS criterion", {
  param_info <- list(
    rB = list(lb = 0, ub = 1),
    h = list(lb = 0, ub = 1)
  )

  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )

  forced_param_values <- c(Bmax = 7)

  res <- estim_param(
    obs_list = obs_synth,
    model_function = toymodel_wrapper,
    model_options = model_options,
    crit_function = crit_ols,
    optim_options = optim_options,
    param_info = param_info,
    obs_var = c("biomass", "yield"),
    forced_param_values = forced_param_values,
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
  expect_equal(
    res$obs_situation_list,
    c("sit1_2000", "sit1_2001", "sit2_2003")
  )
})

# ------------------------------------------------------------------------

test_that("Test estim_param 2 steps crit_ols with param selection", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5),
    Bmax = list(lb = 5, ub = 15, default = 7)
  )
  steps <- list(
    list(
      major_param = c("rB"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass")
    ),
    list(
      major_param = c("h"),
      obs_var = c("yield")
    )
  )

  res_final <- estim_param(
    obs_list = obs_synth,
    crit_function = crit_ols,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info,
    step = steps,
    out_dir = tempdir()
  )

  # Load results from step 1
  load(file.path(tempdir(), "Step1/optim_results.Rdata"))
  res1 <- res
  load(file.path(tempdir(), "Step1/param_select_step1/optim_results.Rdata"))
  res1a <- res
  load(file.path(tempdir(), "Step1/param_select_step2/optim_results.Rdata"))
  res1b <- res

  # Load results from step 2
  load(file.path(tempdir(), "Step2/optim_results.Rdata"))
  res2 <- res

  nb_eval_steps <- sum(sapply(list(res1a, res1b, res2), function(x) nrow(x$params_and_crit)))
  nb_substeps <- 3

  expect_equal(res_final$final_values[["rB"]],
    param_true_values[["rB"]],
    tolerance = param_true_values[["rB"]] * 1e-2
  )
  expect_equal(res_final$final_values[["h"]],
    param_true_values[["h"]],
    tolerance = param_true_values[["h"]] * 1e-2
  )
  expect_true(res_final$total_eval_count >= nb_eval_steps)
  expect_true(res_final$total_eval_count <= (nb_eval_steps + 2 * nb_substeps))
})

# ------------------------------------------------------------------------

test_that("Test estim_param 2 steps without param selection", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5)
  )
  forced_param_values <- c(Bmax = 7)
  step <- list(
    list(
      major_param = c("rB"),
      obs_var = c("biomass")
    ),
    list(
      major_param = c("h"),
      obs_var = c("yield")
    )
  )

  res_final <- estim_param(
    obs_list = obs_synth,
    crit_function = crit_ols,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info,
    forced_param_values = forced_param_values,
    step = step,
    out_dir = tempdir()
  )

  expect_equal(res_final$final_values[["rB"]],
    param_true_values[["rB"]],
    tolerance = param_true_values[["rB"]] * 1e-2
  )
})

# ------------------------------------------------------------------------

test_that("Test estim_param 1 steps with param selection", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1),
    Bmax = list(lb = 5, ub = 15)
  )
  forced_param_values <- c(h = 0.55)

  res_final <- estim_param(
    obs_list = obs_synth,
    crit_function = crit_ols,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info,
    candidate_param = c("Bmax"),
    obs_var = "biomass",
    forced_param_values = forced_param_values,
    out_dir = tempdir()
  )

  expect_equal(res_final$final_values[["rB"]],
    param_true_values[["rB"]],
    tolerance = param_true_values[["rB"]] * 1e-2
  )
})

# ------------------------------------------------------------------------

test_that("Test step check undefined candidate", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1)
  )

  expect_error(estim_param(
    obs_list = obs_synth,
    crit_function = crit_ols,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info,
    candidate_param = c("Bmax"),
    obs_var = "biomass",
    out_dir = tempdir()
  ), regexp = "candidate parameter")
})

# ------------------------------------------------------------------------

test_that("Test step check undefined observed variable", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1)
  )

  expect_error(suppressWarnings(estim_param(
    obs_list = obs_synth,
    crit_function = crit_ols,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info,
    obs_var = "LAI",
    out_dir = tempdir()
  )), regexp = "")
})

# ------------------------------------------------------------------------

# Test estim_param - equality constraints
## Test that the parameter defined as equal to an estimated parameter
## is effectively set to the value of this estimated parameter
test_that("estim_param equality constraints", {
  param_info <- list(
    rB = list(lb = 0, ub = 1),
    h = list(lb = 0, ub = 1)
  )

  optim_options <- list(
    nb_rep = 2, xtol_rel = 1e-2, maxeval = 3,
    ranseed = 1234
  )

  forced_param_values <- c(Bmax = 7, dummy = "rB")

  res <- estim_param(
    obs_list = obs_synth,
    model_function = toymodel_wrapper,
    model_options = model_options,
    crit_function = crit_ols,
    optim_options = optim_options,
    param_info = param_info,
    obs_var = c("biomass", "yield"),
    forced_param_values = forced_param_values,
    situation = c("sit1_2000", "sit1_2001", "sit2_2003"),
    out_dir = tempdir()
  )

  expect_equal(res$forced_param_values[["dummy"]],
    res$final_values[["rB"]],
    tolerance = res$final_values[["rB"]] * 1e-8
  )
})

# ------------------------------------------------------------------------

test_that("Test estim_param empty sim-obs intersection lead to an error", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5)
  )
  forced_param_values <- c(Bmax = 7)
  obs_synth_new <- lapply(obs_synth, function(x) {
    dplyr::mutate(x, biomass_NEW = biomass)
  })
  step <- list(
    list(
      major_param = c("rB"),
      obs_var = c("biomass_NEW")
    ),
    list(
      major_param = c("h"),
      obs_var = c("yield")
    )
  )

  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth_new,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    ))
  )
})

# ------------------------------------------------------------------------

test_that("Test estim_param 2 steps without major_param", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5)
  )
  forced_param_values <- c(Bmax = 7)
  step <- list(
    list(
      obs_var = c("biomass")
    ),
    list(
      obs_var = c("yield")
    )
  )

  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    ))
  )
})

# ------------------------------------------------------------------------

test_that("Test estim_param with common major and candidate param", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1),
    h = list(lb = 0, ub = 1, default = 0.5)
  )
  forced_param_values <- c(Bmax = 7)

  # common major params
  step <- list(
    list(
      major_param = c("rB"),
      obs_var = c("biomass")
    ),
    list(
      major_param = c("rB"),
      obs_var = c("yield")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "major parameter"
  )

  # common candidate params
  step <- list(
    list(
      major_param = c("rB"),
      candidate_param = c("Bmax"),
      obs_var = c("biomass")
    ),
    list(
      major_param = c("h"),
      candidate_param = c("Bmax"),
      obs_var = c("yield")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "candidate parameter"
  )

  # common params between major and candidate
  step <- list(
    list(
      major_param = c("rB"),
      candidate_param = c("rB"),
      obs_var = c("biomass")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "major and candidate parameters"
  )
})

# ------------------------------------------------------------------------

test_that("Test estim_param without major param", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1)
  )
  forced_param_values <- c(Bmax = 7)

  # major param not defined
  step <- list(
    list(
      candidate_param = c("rB"),
      obs_var = c("biomass")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "major_param"
  )

  # No major and no candidate
  step <- list(
    list(
      major_param = NULL,
      obs_var = c("biomass")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "major_param"
  )

  # major param set to NULL
  step <- list(
    list(
      major_param = NULL,
      candidate_param = c("rB"),
      obs_var = c("biomass")
    )
  )
  res_final <- estim_param(
    obs_list = obs_synth,
    crit_function = crit_ols,
    model_function = toymodel_wrapper,
    model_options = model_options,
    optim_options = optim_options,
    param_info = param_info,
    forced_param_values = forced_param_values,
    step = step,
    out_dir = tempdir()
  )
  expect_identical(names(res_final$final_values), "rB")
  expect_equal(res_final$final_values[["rB"]],
    param_true_values[["rB"]],
    tolerance = param_true_values[["rB"]] * 1e-2
  )
})

# ------------------------------------------------------------------------

test_that("Test if error is catched in case an obs in step is not in obs_list", {
  optim_options <- list(
    nb_rep = 5, xtol_rel = 1e-2,
    ranseed = 1234
  )
  param_info <- list(
    rB = list(lb = 0, ub = 1, default = 0.1)
  )
  forced_param_values <- c(Bmax = 7)

  # obs not defined
  step <- list(
    list(
      major_param = NULL,
      candidate_param = c("rB"),
      obs_var = c("toto")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "but not found"
  )

  # situation not defined
  step <- list(
    list(
      major_param = NULL,
      candidate_param = c("rB"),
      obs_var = c("biomass"),
      situation = c("toto")
    )
  )
  expect_error(
    suppressWarnings(estim_param(
      obs_list = obs_synth,
      crit_function = crit_ols,
      model_function = toymodel_wrapper,
      model_options = model_options,
      optim_options = optim_options,
      param_info = param_info,
      forced_param_values = forced_param_values,
      step = step,
      out_dir = tempdir()
    )),
    regexp = "but not found"
  )
})
