context("Test the likelihood criterion")

obs_list <- list(sit1=data.frame(var1=rep(1,4),var2=c(NA,NA,NA,2)))
sim_list <- list(sit1=data.frame(var1=rep(2,4),var2=c(NA,NA,NA,4)))
obs_list2 <- list(sit1=data.frame(var1=rep(1,4),var2=c(NA,NA,NA,2)),
               sit2=data.frame(var2=c(1,NA,NA,3)))
sim_list2 <- list(sit1=data.frame(var1=rep(2,4),var2=c(NA,NA,NA,4)),
               sit2=data.frame(var2=c(3,NA,NA,6)))

test_that("likelihood_log_ciidn", {
  expect_equal(likelihood_log_ciidn(sim_list,sim_list), Inf)
  expect_equal(likelihood_log_ciidn(obs_list,sim_list), log((4^(-4)*4^(-2.5))))
  expect_equal(likelihood_log_ciidn(obs_list2,sim_list2),
               log((4^(-4)*17^(-7/2))))
})

test_that("likelihood_log_ciidn_corr", {
  expect_equal(likelihood_log_ciidn_corr(sim_list,sim_list), Inf)
  expect_equal(likelihood_log_ciidn_corr(obs_list,sim_list), log((4^(-5/2))))
  expect_equal(likelihood_log_ciidn_corr(obs_list2,sim_list2),
               log(((4+13/2)^(-3))))
})


