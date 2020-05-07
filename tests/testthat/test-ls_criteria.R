context("Test the least square criterion")

obs_list=list(sit1=data.frame(var1=rep(1,4),var2=c(NA,NA,NA,2)))
sim_list=list(sit1=data.frame(var1=rep(2,4),var2=c(NA,NA,NA,4)))
obs_list2=list(sit1=data.frame(var1=rep(1,4),var2=c(NA,NA,NA,2)),
               sit2=data.frame(var2=c(1,NA,NA,3)))
sim_list2=list(sit1=data.frame(var1=rep(2,4),var2=c(NA,NA,NA,4)),
               sit2=data.frame(var2=c(3,NA,NA,6)))

test_that("crit_ols", {
  expect_equal(crit_ols(sim_list,sim_list), 0)
  expect_equal(crit_ols(obs_list,sim_list), 8)
  expect_equal(crit_ols(obs_list2,sim_list2), 21)
})
test_that("crit_log_cwss", {
  expect_equal(crit_log_cwss(sim_list,sim_list), -Inf)
  expect_equal(crit_log_cwss(obs_list,sim_list), log(2))
  expect_equal(crit_log_cwss(obs_list2,sim_list2), log((17/3)^(3/2)))
})
test_that("crit_log_cwss_corr", {
  expect_equal(crit_log_cwss_corr(sim_list,sim_list), -Inf)
  expect_equal(crit_log_cwss_corr(obs_list,sim_list), log(2))
  expect_equal(crit_log_cwss_corr(obs_list2,sim_list2), log((21/4)))
})
