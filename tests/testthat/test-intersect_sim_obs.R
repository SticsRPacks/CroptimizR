obs_list <- list(sit1 = data.frame(
  Date = as.POSIXct(c("2009-10-31", "2009-11-10")),
  var1 = rep(1, 2), var2 = c(NA, 2)
))
sim_list <- list(sit1 = data.frame(
  Date = as.POSIXct(c(
    "2009-10-31", "2009-11-02",
    "2009-11-05", "2009-11-10"
  )),
  var1 = rep(2, 4), var2 = c(NA, NA, NA, 4)
))
res <- eval(parse(text = "CroptimizR:::intersect_sim_obs(obs_list,sim_list)"))
res2 <- eval(parse(text = "CroptimizR:::intersect_sim_obs(obs_list,obs_list)"))
test_that("intersect_sim_obs", {
  expect_true(all.equal(res$sim_list$sit1$Date, res$obs_list$sit1$Date))
  expect_true(all.equal(
    colnames(res$sim_list$sit1),
    colnames(res$obs_list$sit1)
  ))
  expect_true(all.equal(res2$sim_list$sit1$Date, res2$obs_list$sit1$Date))
  expect_true(all.equal(
    colnames(res2$sim_list$sit1),
    colnames(res2$obs_list$sit1)
  ))
})
obs_list <- list(sit1 = data.frame(
  Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
  var1 = rep(1, 2), var2 = c(NA, 2)
))
sim_list <- list(sit1 = data.frame(
  Date = as.POSIXct(c(
    "2009-10-31", "2009-11-02",
    "2009-11-05", "2009-11-10"
  )),
  var1 = rep(2, 4), var2 = c(NA, NA, NA, 4)
))
res <- suppressWarnings(eval(parse(
  text = "CroptimizR:::intersect_sim_obs(obs_list,sim_list)"
)))
test_that("intersect_sim_obs", {
  expect_warning(eval(parse(
    text = "CroptimizR:::intersect_sim_obs(obs_list,sim_list)"
  )), "dates")
  expect_equal(res, NA)
})
obs_list <- list(sit1 = data.frame(
  Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
  var5 = rep(1, 2), var6 = c(NA, 2)
))
sim_list <- list(sit1 = data.frame(
  Date = as.POSIXct(c(
    "2009-10-31", "2009-11-02",
    "2009-11-05", "2009-11-10"
  )),
  var1 = rep(2, 4), var2 = c(NA, NA, NA, 4)
))
res <- suppressWarnings(eval(parse(
  text = "CroptimizR:::intersect_sim_obs(obs_list,sim_list)"
)))
test_that("intersect_sim_obs", {
  expect_warning(eval(parse(
    text = "CroptimizR:::intersect_sim_obs(obs_list,sim_list)"
  )), "*variables")
  expect_equal(res, NA)
})
