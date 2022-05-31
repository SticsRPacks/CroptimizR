context("Test the information criteria")

obs_list <- list(sit1=data.frame(Date=rep(NA,4), var1=rep(1,4),var2=c(NA,NA,NA,2)),
               sit2=data.frame(Date=rep(NA,4), var2=c(1,NA,NA,3)))

test_that("AIC", {
  expect_equal(eval(parse(text = "AIC(obs_list, crit_value=0, param_nb=5)")), -Inf)
  expect_equal(eval(parse(text = "AIC(obs_list, crit_value=5, param_nb=Inf)")), Inf)
})
test_that("AICc", {
  expect_equal(eval(parse(text = "AICc(obs_list, crit_value=0, param_nb=5)")), -Inf)
})
test_that("BIC", {
  expect_equal(eval(parse(text = "BIC(obs_list, crit_value=0, param_nb=5)")), -Inf)
  expect_equal(eval(parse(text = "BIC(obs_list, crit_value=5, param_nb=Inf)")), Inf)
})
test_that("AICc=AIC=BIC if p=0", {
  expect_equal(eval(parse(text = "AICc(obs_list, crit_value=5, param_nb=0)")),
               eval(parse(text = "AIC(obs_list, crit_value=5, param_nb=0)")))
  expect_equal(eval(parse(text = "AICc(obs_list, crit_value=5, param_nb=0)")),
               eval(parse(text = "BIC(obs_list, crit_value=5, param_nb=0)")))
})
