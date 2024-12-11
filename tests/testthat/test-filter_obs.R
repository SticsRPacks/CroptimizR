obs_list <- list(
  sit1 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
    var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
  ),
  sit2 = data.frame(
    Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
    var1 = c(NA, NA)
  ),
  sit3 = data.frame(
    Date = as.POSIXct(c("2010-10-03", "2010-10-04")),
    var1 = c(NA, NA), var2 = c(2.0, 2.1)
  )
)

test_that("filter_obs filters-out correctly", {
  # Remove all variables:
  expect_null(suppressWarnings(filter_obs(obs_list, var = c("var1", "var2"))))
  expect_warning(
    filter_obs(obs_list, var = c("var1", "var2")),
    "All variables have been excluded from the list"
  )
  expect_equivalent(
    suppressWarnings(filter_obs(obs_list, var = c("var1"))),
    list(
      sit1 = obs_list[[1]][2, c(1, 3)],
      sit3 = obs_list[[3]][, c(1, 3)]
    )
  )
  expect_warning(
    filter_obs(obs_list, var = c("var1")),
    "No observations found in situation\\(s\\) sit2",
  )
  expect_equal(
    suppressWarnings(filter_obs(obs_list, var = c("var2"))),
    list(sit1 = obs_list[[1]][, 1:2])
  )
  expect_warning(
    filter_obs(obs_list, var = c("var2")),
    "No observations found in situation\\(s\\) sit2, sit3",
  )
})


test_that("filter_obs filters-in correctly", {
  expect_equal(
    suppressWarnings(filter_obs(obs_list, var = c("var1"), include = TRUE)),
    list(sit1 = obs_list[[1]][, 1:2])
  )

  expect_equivalent(
    suppressWarnings(filter_obs(obs_list, var = c("var2"), include = TRUE)),
    list(
      sit1 = obs_list[[1]][2, c(1, 3)],
      sit3 = obs_list[[3]][, c(1, 3)]
    )
  )

  expect_warning(
    filter_obs(obs_list, var = c("var1"), include = TRUE),
    "No observations found in situation\\(s\\) sit2, sit3",
  )

  expect_warning(
    filter_obs(obs_list, var = c("var2"), include = TRUE),
    "No observations found in situation\\(s\\) sit2",
  )
})
