#' @title Test model wrappers
#'
#' @description This function perform some tests of CroptimizR model wrappers.
#' See @details for more information.
#'
#' @param model_function Crop Model wrapper function to use.
#'
#' @param model_options List of options for the Crop Model wrapper (see help of
#' the Crop Model wrapper function used).
#'
#' @param param_values a named vector that contains values and names for AT LEAST TWO model parameters THAT ARE EXPECTED TO PLAY ON ITS RESULTS.
#'
#' @param situation Vector of situations names for which results must be tested.
#'
#' @param var (optional) Vector of variables names for which results must be tested.
#'
#' @param sit_names `r lifecycle::badge("deprecated")` `sit_names` is no
#'   longer supported, use `situation` instead.
#'
#' @param var_names `r lifecycle::badge("deprecated")` `var_names` is no
#'   longer supported, use `var` instead.
#'
#' @details This function runs the wrapper consecutively with different subsets of param_values.
#' It then checks:
#'    - the format of the returned results
#'    - the results are different when different subsets of param_values are used,
#'    - the results are identical when same subsets of param_values are used.
#'
#' @return
#' A list containing:
#'    - test_results: a vector of boolean indicating which test succeeded (TRUE) or failed (FALSE)
#'    - param_values_1: first subset of param_values
#'    - param_values_2: second subset of param_values
#'    - sim_1: results obtained with param_values_1
#'    - sim_2: results obtained with param_values_2
#'    - sim_3: results obtained for second run with param_values_1
#'
#' @export
#'
test_wrapper <- function(model_function, model_options, param_values, situation,
                         var = NULL, sit_names = lifecycle::deprecated(),
                         var_names = lifecycle::deprecated()) {
  # Managing parameter names changes between versions:
  if (lifecycle::is_present(sit_names)) {
    lifecycle::deprecate_warn("0.5.0", "test_wrapper(sit_names)", "test_wrapper(situation)")
  } else {
    sit_names <- situation # to remove when we update inside the function
  }
  if (lifecycle::is_present(var_names)) {
    lifecycle::deprecate_warn("0.5.0", "test_wrapper(var_names)", "test_wrapper(var)")
  } else {
    var_names <- var # to remove when we update inside the function
  }


  if (length(param_values) <= 1) {
    stop("param_values argument must include at least TWO parameters.")
  }

  test_results <- c(test1 = FALSE, test2 = FALSE, test3 = FALSE)

  param_values_1 <- param_values[1]
  param_values_2 <- param_values
  # hum, this test is a bit uggly but seems to be necessary
  # (wrappers may have or not the arguments situation and var or their old name sit_names and var_names
  # and they may have both in case the use deprecated arguments for sit_names and var_names ...
  if ((("situation" %in% names(formals(model_function))) | ("var" %in% names(formals(model_function)))) |
    !(("sit_names" %in% names(formals(model_function))) | ("var_names" %in% names(formals(model_function))))) {
    sim_1 <- model_function(
      param_values = param_values_1, model_options = model_options,
      situation = sit_names, var = var_names
    )
    sim_2 <- model_function(
      param_values = param_values_2, model_options = model_options,
      situation = sit_names, var = var_names
    )
    sim_3 <- model_function(
      param_values = param_values_1, model_options = model_options,
      situation = sit_names, var = var_names
    )
  } else if (("sit_names" %in% names(formals(model_function))) | ("var_names" %in% names(formals(model_function)))) {
    sim_1 <- model_function(
      param_values = param_values_1, model_options = model_options,
      sit_names = sit_names, var_names = var_names
    )
    sim_2 <- model_function(
      param_values = param_values_2, model_options = model_options,
      sit_names = sit_names, var_names = var_names
    )
    sim_3 <- model_function(
      param_values = param_values_1, model_options = model_options,
      sit_names = sit_names, var_names = var_names
    )
    lifecycle::deprecate_warn("0.5.0", "model_function(sit_names)", "model_function(situation)")
    lifecycle::deprecate_warn("0.5.0", "model_function(var_names)", "model_function(var)")
  }

  results <- list(
    test_results = test_results,
    param_values_1 = param_values_1,
    param_values_2 = param_values_2,
    sim_1 = sim_1,
    sim_2 = sim_2,
    sim_3 = sim_3
  )

  cat(crayon::green("Test the wrapper returns outputs in expected format ...\n"))
  if (!("error" %in% names(sim_1))) {
    cat(crayon::red("... test failed\n"))
    cat(crayon::red("The list returned by the wrapper must contain a variable called error.\n"))
  } else if (!("sim_list" %in% names(sim_1))) {
    cat(crayon::red("... test failed\n"))
    cat(crayon::red("The list returned by the wrapper must contain a variable called sim_list.\n"))
  } else if (!is.sim(sim_1$sim_list)) {
    cat(crayon::red("... test failed\n"))
    cat(crayon::red("The sim_list variable returned by the wrapper is not in the expected format (see warnings for more details).\n"))
  } else {
    cat(crayon::green("... OK\n"))
    results$test_results[1] <- TRUE
  }
  cat("\n")


  cat(crayon::green("Test the wrapper gives identical results when running with same inputs ...\n"))
  if (identical(sim_1, sim_3)) {
    cat(crayon::green("... OK\n"))
    results$test_results[2] <- TRUE
  } else {
    cat(crayon::red("... test failed\n"))
    cat(crayon::red("The wrapper gave different results although executed two times with the same inputs param_values_1 (see results in sim_1 and sim_3).\n"))
    cat(crayon::red("This may be due to model input files that do not come back to their original state at the end of the wrapper.\n"))
  }
  cat("\n")
  cat(crayon::green("Test the wrapper gives different results when running with different inputs ...\n"))
  if (!identical(sim_1, sim_2)) {
    cat(crayon::green("... OK\n"))
    results$test_results[3] <- TRUE
  } else {
    cat(crayon::red("... test failed\n"))
    cat(crayon::red("The wrapper gave same results although executed with different param_values arguments param_values_1 and param_values_2 (see results in sim_1 and sim_2).\n"))
    cat(crayon::red("Either param_values is not correctly handled in the model wrapper (in part. make sure that model inputs are modified according to param_values) or the selected parameters do not play on the model outputs (test with other parameters in param_values).\n"))
  }

  return(invisible(results))
}
