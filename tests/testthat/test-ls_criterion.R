context("Test the least square criterion")

obs_list=list(sit1=data.frame(var1=rep(1,4),var2=c(NA,NA,NA,2)))
sim_list=list(sit1=data.frame(var1=rep(2,4),var2=c(NA,NA,NA,4)))
test_that("crit_cwss", {
  expect_equal(crit_cwss(sim_list,sim_list), 0)
  expect_equal(crit_cwss(obs_list,sim_list), 2)
})
