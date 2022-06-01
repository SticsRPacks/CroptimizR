context("Test the filter_param_info function")

param_info1 <- list(
  lb = c(dlaimax = 0.0005, durvieF = 50),
  ub = c(dlaimax = 0.0025, durvieF = 400)
)
param_info1_res <- list(
  lb = c(durvieF = 50),
  ub = c(durvieF = 400)
)

# A case with groups of situations per parameter
param_info2 <- list()
param_info2$dlaimax <- list(
  sit_list = list(c(
    "bou99t3", "bou00t3", "bou99t1", "bou00t1",
    "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
  )),
  lb = 0.0005, ub = 0.0025
)
param_info2$durvieF <- list(
  sit_list = list(
    c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
    c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
  ),
  lb = c(50, 100), ub = c(400, 500)
)
param_info2_res <- param_info2["durvieF"]

test_that("filter_param_info", {
  expect_equal(eval(parse(text = "CroptimizR:::filter_param_info(param_info1, \"durvieF\")")), param_info1_res)
  expect_equal(eval(parse(text = "CroptimizR:::filter_param_info(param_info2, \"durvieF\")")), param_info2_res)
})
