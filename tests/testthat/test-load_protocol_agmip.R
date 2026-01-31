context("Test the load of the AgMIP protocol description file")

library(testthat)

# ------------------------------------------------------------------------

test_that("Missing file triggers error", {
  expect_error(
    load_protocol_agmip("this_file_does_not_exist.xlsx"),
    "Protocol file not found"
  )
})

# ------------------------------------------------------------------------

test_that("Non-character path triggers error", {
  expect_error(
    load_protocol_agmip(123),
    "must be a single character string"
  )
})

# ------------------------------------------------------------------------

test_that("Test no error in reading the Agmip protocol file used for the vignette", {
  protocol_file_path <- file.path(
    system.file(package = "CroptimizR"), "extdata", "AgMIP_protocol",
    "agmip_protocol_vignette.xlsx"
  )
  expect_no_error(load_protocol_agmip(protocol_file_path))
})

# ------------------------------------------------------------------------

test_that("Test no error in reading the example protocol file", {
  protocol_file_path <- file.path(
    system.file(package = "CroptimizR"), "extdata",
    "agmip_protocol_example.xlsx"
  )
  expect_no_error(load_protocol_agmip(protocol_file_path))
})

# ------------------------------------------------------------------------

test_that("Valid protocol loads correctly", {
  file <- tempfile(fileext = ".xlsx")
  make_protocol_xlsx(file)

  res <- load_protocol_agmip(file)

  expect_type(res, "list")
  expect_named(res, c("step", "param_info"))

  expect_named(res$step, c("G1", "G2"))

  expect_equal(res$step$G1$major_param, "p1")
  expect_null(res$step$G1$candidate_param)

  expect_null(res$step$G2$major_param)
  expect_equal(res$step$G2$candidate_param, "p2")

  expect_equal(res$step$G1$obs_var, c("v1", "v2"))
  expect_equal(res$step$G2$obs_var, "v3")
})

# ------------------------------------------------------------------------

test_that("Group order follows variables sheet", {
  file <- tempfile(fileext = ".xlsx")

  variables <- data.frame(
    variable = c("v1", "v2"),
    group = c("B1", "A")
  )

  major <- data.frame(
    parameter = "p1",
    group = "A",
    default_value = 1,
    lower_bound = 0,
    upper_bound = 2,
    check.names = FALSE
  )

  candidate <- data.frame(
    parameter = "p2",
    group = "B1",
    default_value = 5,
    lower_bound = 0,
    upper_bound = 10,
    check.names = FALSE
  )

  make_protocol_xlsx(file, variables = variables, major = major, candidate = candidate)

  res <- load_protocol_agmip(file)

  expect_equal(names(res$step), c("B1", "A"))
})

# ------------------------------------------------------------------------

test_that("Unknown group in major triggers error", {
  file <- tempfile(fileext = ".xlsx")

  major <- data.frame(
    parameter = "p1",
    group = "UNKNOWN",
    default_value = 1,
    lower_bound = 0,
    upper_bound = 2,
    check.names = FALSE
  )

  make_protocol_xlsx(file, major = major)

  expect_error(
    load_protocol_agmip(file),
    "not present in sheet 'variables'"
  )
})

# ------------------------------------------------------------------------

test_that("Group with no major and no candidate triggers error", {
  file <- tempfile(fileext = ".xlsx")

  variables <- data.frame(
    variable = c("v1", "V2"),
    group = c("G1", "G2")
  )

  major <- data.frame(
    parameter = "p1",
    group = "G1",
    default_value = 0.5,
    lower_bound = 0,
    upper_bound = 1,
    check.names = FALSE
  )

  candidate <- data.frame(
    parameter = character(0),
    group = character(0),
    default_value = numeric(0),
    lower_bound = numeric(0),
    upper_bound = numeric(0),
    check.names = FALSE
  )

  make_protocol_xlsx(file, variables = variables, major = major, candidate = candidate)

  expect_error(
    load_protocol_agmip(file),
    "has neither major nor candidate parameters"
  )
})

# ------------------------------------------------------------------------

test_that("Default value out of bounds triggers error", {
  file <- tempfile(fileext = ".xlsx")

  major <- data.frame(
    parameter = "p1",
    group = "G1",
    default_value = 10,
    lower_bound = 0,
    upper_bound = 2,
    check.names = FALSE
  )

  make_protocol_xlsx(file, major = major)

  expect_error(
    load_protocol_agmip(file),
    "Default value out of bounds"
  )
})

# ------------------------------------------------------------------------

test_that("Constraints sheet is loaded correctly", {
  file <- tempfile(fileext = ".xlsx")

  constraints <- data.frame(
    parameter = c("p3"),
    value_or_formula = c("2 * p1"),
    check.names = FALSE
  )

  make_protocol_xlsx(file, constraints = constraints)

  res <- load_protocol_agmip(file)

  expect_true("p3" %in% names(res$forced_param_values))
  expect_equal(res$forced_param_values[["p3"]], "2 * p1")
  expect_equal(
    compute_eq_const(res$forced_param_values, param_values = c(p1 = 2)),
    c(p3 = 4)
  )
})

# ------------------------------------------------------------------------

test_that("major_param is NULL when no major parameters exist for a group", {
  file <- tempfile(fileext = ".xlsx")

  # Define variables and candidate parameters only
  variables <- data.frame(
    variable = c("v1"),
    group = c("G1")
  )

  major <- data.frame(
    parameter = character(0),
    group = character(0),
    default_value = numeric(0),
    lower_bound = numeric(0),
    upper_bound = numeric(0),
    check.names = FALSE
  )

  candidate <- data.frame(
    parameter = c("p1"),
    group = c("G1"),
    default_value = 0.5,
    lower_bound = 0,
    upper_bound = 1,
    check.names = FALSE
  )

  # Create the test protocol Excel file
  make_protocol_xlsx(file, variables = variables, major = major, candidate = candidate)

  res <- load_protocol_agmip(file)

  # Check that major_param is NULL for G2 (no major parameters for this group)
  expect_true("G1" %in% names(res$step))
  expect_null(res$step[["G1"]]$major_param)

  # Also verify that major_param exists as a field
  expect_named(res$step[["G1"]], c("major_param", "candidate_param", "obs_var"))
})

# ------------------------------------------------------------------------

test_that("Returned structure from load_protocol_agmip has correct fields and values", {
  file <- tempfile(fileext = ".xlsx")

  # Sample protocol
  variables <- data.frame(
    variable = c("v1", "v2"),
    group = c("G1", "G2")
  )

  major <- data.frame(
    parameter = c("p1"),
    group = c("G1"),
    default_value = 0.5,
    lower_bound = 0,
    upper_bound = 1,
    check.names = FALSE
  )

  candidate <- data.frame(
    parameter = c("p2"),
    group = c("G2"),
    default_value = 5,
    lower_bound = 0,
    upper_bound = 10,
    check.names = FALSE
  )

  make_protocol_xlsx(file, variables = variables, major = major, candidate = candidate)

  res <- load_protocol_agmip(file)

  # Check that step is a named list
  expect_type(res$step, "list")
  expect_named(res$step, c("G1", "G2"))

  # Check each group's fields
  for (group in names(res$step)) {
    expect_named(res$step[[group]], c("major_param", "candidate_param", "obs_var"))
    expect_true(is.null(res$step[[group]]$major_param) || is.character(res$step[[group]]$major_param))
    expect_true(is.null(res$step[[group]]$candidate_param) || is.character(res$step[[group]]$candidate_param))
    expect_true(is.character(res$step[[group]]$obs_var))
  }

  # Check param_info contains required fields
  expect_named(res$param_info, c("lb", "ub", "default"))

  # Check all parameters are included
  all_params <- c(major$parameter, candidate$parameter)
  expect_setequal(names(res$param_info$lb), all_params)
  expect_setequal(names(res$param_info$ub), all_params)
  expect_setequal(names(res$param_info$default), all_params)

  # Check that the values match
  expect_equal(res$param_info$lb[[major$parameter]], major$lower_bound)
  expect_equal(res$param_info$ub[[major$parameter]], major$upper_bound)
  expect_equal(res$param_info$default[[major$parameter]], major$default_value)

  expect_equal(res$param_info$lb[[candidate$parameter]], candidate$lower_bound)
  expect_equal(res$param_info$ub[[candidate$parameter]], candidate$upper_bound)
  expect_equal(res$param_info$default[[candidate$parameter]], candidate$default_value)
})
