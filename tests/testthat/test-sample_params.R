bounds_list <- list(lb = c(0, 1, 2), ub = c(1, 2, 3))
res1=CroptimizR:::sample_params(bounds_list, 5)
Sys.sleep(2)
res2=CroptimizR:::sample_params(bounds_list, 5)

test_that("sample_params", {
  expect_false(isTRUE(all.equal(res1,res2)))
  # for the next one we only test the 2 first columns since it seems that DiceDesign is not portable (values on the last columns are sometimes twisted)
  expect_equal(CroptimizR:::sample_params(bounds_list, 2,seed=2)[,1:2],matrix(data=c(0.4075589,0.6488130, 1.213337, 1.915974),ncol=2,nrow = 2),tolerance=1e-3)
})
