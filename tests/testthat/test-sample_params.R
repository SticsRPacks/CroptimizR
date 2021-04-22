bounds_list <- list(lb = c(0, 1, 2), ub = c(1, 2, 3))
res1=CroptimizR:::sample_params(bounds_list, 5)
Sys.sleep(2)
res2=CroptimizR:::sample_params(bounds_list, 5)

test_that("sample_params", {
  local_edition(3)
  expect_false(isTRUE(all.equal(res1,res2)))
  res <- CroptimizR:::sample_params(bounds_list, 2,seed=2)
  expect_snapshot(res)
})
