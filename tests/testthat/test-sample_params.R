bounds_list <- list(lb = c(0, 1, 2), ub = c(1, 2, 3))
res1 <- eval(parse(text = "CroptimizR:::sample_params(bounds_list, 5, seed = 1234)"))
Sys.sleep(2)
res2 <- eval(parse(text = "CroptimizR:::sample_params(bounds_list, 5)"))
res3 <- eval(parse(text = "CroptimizR:::sample_params(bounds_list, 5, seed = 1234)"))

test_that("sample_params", {
  expect_false(isTRUE(all.equal(res1, res2)))
  expect_identical(res1, res3)
})
