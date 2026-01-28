test_that("obs_list is null", {
  expect_error(suppressWarnings(get_obs_var(NULL)))
})

test_that("Single situation", {
  obs_list <- list(
    sit1 = data.frame(
      Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
      var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
    )
  )
  expect_equal(get_obs_var(obs_list), c("var1", "var2"))
})

test_that("Several situations with same variables", {
  obs_list <- list(
    sit1 = data.frame(
      Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
      var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
    ),
    sit2 = data.frame(
      Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
      var1 = c(1.3, 2)
    )
  )
  expect_equal(get_obs_var(obs_list), c("var1", "var2"))
})

test_that("Plant column", {
  obs_list <- list(
    sit1 = data.frame(
      Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
      var1 = c(1.1, 1.5), var2 = c(NA, 2.1), Plant = c(1, 2)
    ),
    sit2 = data.frame(
      Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
      var1 = c(1.3, 2), Plant = c(1, 2)
    )
  )
  expect_equal(get_obs_var(obs_list), c("var1", "var2"))
})
