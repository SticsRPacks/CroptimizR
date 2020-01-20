bounds_list <- list(lb = c(0, 1, 2), ub = c(1, 2, 3))
res1=CroptimizR:::sample_params(bounds_list, 5)
Sys.sleep(2)
res2=CroptimizR:::sample_params(bounds_list, 5)

test_that("sample_params", {
  expect_false(isTRUE(all.equal(res1,res2)))
  expect_equal(CroptimizR:::sample_params(bounds_list, 2,seed=2),matrix(data=c(0.4075589,0.6488130, 1.213337, 1.915974, 2.028080, 2.528263),ncol=3,nrow = 2),tolerance=1e-3)
})
