obs_list <- list(
  sit1 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
    var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
  ),
  sit2 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
    var1 = c(1.3, 2)
  ),
  sit3 = data.frame(
    Date = as.POSIXct(c("2010-10-03", "2010-10-04")),
    var1 = c(1., 1.1), var2 = c(2.0, 2.1)
  )
)

# Check if nothing is captured if sim and obs lists are identical
res <- eval(parse(
  text = "CroptimizR:::make_obsSim_consistent(obs_list, obs_list)"
))
test_that("obs and sim are identical", {
  expect_identical(res$obs_list, obs_list)
  expect_identical(res$sim_list, obs_list)
})


# Check if it captures that there are non consistent types for some sim and obs
# variables
sim_list <- obs_list
sim_list[[1]]$var1 <- as.character(sim_list[[1]]$var1)
sim_list[[3]]$var2 <- as.character(sim_list[[3]]$var2)
test_that("Capture non-consistent variable types", {
  expect_error(
    eval(parse(
      text =
        "CroptimizR:::check_obsSim_consistency(sim_list, obs_list)"
    )),
    "different types"
  )
})

# Check if it captures that some Date columns are of non expected type
sim_list <- obs_list
sim_list[[1]]$Date <- as.character(sim_list[[1]]$Date)
sim_list[[3]]$Date <- as.character(sim_list[[3]]$Date)
test_that("Capture unexpected type for Date columns", {
  expect_error(
    eval(
      parse(text = "CroptimizR:::check_obsSim_consistency(sim_list, obs_list)")
    ),
    "incorrect format"
  )
})

# Check if it corrects non consistent types for sim and obs Dates
sim_list <- obs_list
sim_list[[1]]$Date <- as.Date(sim_list[[1]]$Date)
sim_list[[3]]$Date <- as.Date(sim_list[[3]]$Date)
res <- eval(parse(
  text = "CroptimizR:::make_obsSim_consistent(sim_list, obs_list)"
))
test_that("Check correction of dates types", {
  expect_true(lubridate::is.POSIXct(res$sim_list[[1]]$Date))
  expect_true(lubridate::is.POSIXct(res$sim_list[[3]]$Date))
})


# Check if is_sim_inf_or_na return FALSE when it must
sim_list <- obs_list
param_values <- c(p1 = 1.0, p2 = 2.0)
res <- eval(parse(
  text = "CroptimizR:::is_sim_inf_or_na(sim_list, obs_list, param_values)"
))
test_that("Check is_sim_inf_or_na return FALSE when sim is not Inf neither NA when there is a corresponding observed value", {
  expect_false(res)
})

# Check if is_sim_inf_or_na return TRUE when it must, missing value for one variable, one situation and one date
sim_list <- obs_list
sim_list$sit1$var1[[1]] <- Inf
param_values <- c(p1 = 1.0, p2 = 2.0)
test_that("Check is_sim_inf_or_na return TRUE when sim is Inf or NA when there is a corresponding observed value, case 1", {
  expect_warning(
    eval(parse(
      text = "CroptimizR:::is_sim_inf_or_na(sim_list, obs_list, param_values)"
    )),
    "sit1.*var1.*2009-11-30"
  )
})

# Check if is_sim_inf_or_na return TRUE when it must, missing values for several dates
sim_list <- obs_list
sim_list$sit3$var2 <- NA
param_values <- c(p1 = 1.0, p2 = 2.0)
test_that("Check is_sim_inf_or_na return TRUE when sim is Inf or NA when there is a corresponding observed value, case 1", {
  expect_warning(
    eval(parse(
      text = "CroptimizR:::is_sim_inf_or_na(sim_list, obs_list, param_values)"
    )),
    "sit3.*var2.*2010-10-03.*2010-10-04"
  )
})


# Check if is_sim_inf_or_na return TRUE when it must, missing values for several situations, variables and dates
sim_list <- obs_list
sim_list$sit1$var1[[1]] <- Inf
sim_list$sit3$var1[[2]] <- NA
sim_list$sit3$var2 <- NA
param_values <- c(p1 = 1.0, p2 = 2.0)
test_that("Check is_sim_inf_or_na return TRUE when sim is Inf or NA when there is a corresponding observed value, case 1", {
  expect_warning(
    eval(parse(
      text = "CroptimizR:::is_sim_inf_or_na(sim_list, obs_list, param_values)"
    )),
    "sit1.*var1.*2009-11-30.*sit3.*var1.*2010-10-04.*var2.*2010-10-03.*2010-10-04"
  )
})
