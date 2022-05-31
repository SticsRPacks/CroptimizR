context("Test the complete_init_values function")


# No user initial values provided nor constraints
param_info <- list(init_values=data.frame(dlaimax=c(0.001, NA), durvieF=c(75, 100)),
                lb=c(dlaimax=0.0005, durvieF=50),
                ub=c(dlaimax=0.0025, durvieF=400))
satisfy_par_const <- function(param_values, ...) {
  if (param_values$dlaimax<0.001) return(FALSE)
  return(TRUE)
}
test_that("Initial values and constraints", {
  res1 <- eval(parse(text = "CroptimizR:::complete_init_values(param_info, nb_values=5,
                                    ranseed=1234)"))
  res2 <- eval(parse(text = "CroptimizR:::complete_init_values(param_info, nb_values=5,
                                           ranseed=4321)"))
  res3 <- eval(parse(text = "CroptimizR:::complete_init_values(param_info, nb_values=100,
                                          satisfy_par_const=satisfy_par_const,
                                           ranseed=1234)"))
  expect_false(isTRUE(all.equal(res1,res2)))
  expect_equal(res2$init_values$dlaimax[1], param_info$init_values$dlaimax[1])
  expect_equal(res2$init_values$durvieF[1:2], param_info$init_values$durvieF[1:2])
  expect_true(all(sapply(1:nrow(res3$init_values), function(x) satisfy_par_const(res3$init_values[x,]))))

})


context("Test the set_init_values function")

# Simple case 1
param_info <- list(init_values=c(dlaimax=0.001, durvieF=200),
                lb=c(dlaimax=0.0001, durvieF=50),
                ub=c(dlaimax=0.01, durvieF=400))
eval(parse(text = "init_values <- CroptimizR:::get_init_values(param_info)"))
param_info_new <- within(param_info, rm(init_values))
eval(parse(text = "param_info_new <- CroptimizR:::set_init_values(param_info, init_values)"))
eval(parse(text = "init_values_new <- CroptimizR:::get_init_values(param_info_new)"))

test_that("set_init_values: simple case 1", {
  expect_identical(init_values, init_values_new)
})

# Simple case 2
param_info <- list(init_values=data.frame(dlaimax=c(0.001,0.002), durvieF=c(50,200)),
                lb=c(dlaimax=0.0001, durvieF=50),
                ub=c(dlaimax=0.01, durvieF=400))
eval(parse(text = "init_values <- CroptimizR:::get_init_values(param_info)"))
param_info_new <- within(param_info, rm(init_values))
eval(parse(text = "param_info_new <- CroptimizR:::set_init_values(param_info, init_values)"))
eval(parse(text = "init_values_new <- CroptimizR:::get_init_values(param_info_new)"))

test_that("set_init_values: simple case 2", {
  expect_identical(init_values, init_values_new)
})


# Case 1 with groups of situations per parameter
param_info <- list()
param_info$dlaimax <- list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
                                        "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
                        init_values=0.001,lb=0.0001,ub=0.1)
param_info$durvieF <- list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
                                      c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
                        init_values=c(200,300),lb=50,ub=400)
eval(parse(text = "init_values <- CroptimizR:::get_init_values(param_info)"))
param_info_new <- param_info
param_info_new$dlaimax$init_values <- NULL
param_info_new$durvieF$init_values <- NULL
eval(parse(text = "param_info_new <- CroptimizR:::set_init_values(param_info, init_values)"))
eval(parse(text = "init_values_new <- CroptimizR:::get_init_values(param_info_new)"))

test_that("set_init_values: Case 1 with groups of situations per parameter", {
  expect_identical(init_values, init_values_new)
})




# Case 2 with groups of situations per parameter

param_info <- list()
param_info$dlaimax <- list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
                                        "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
                        init_values=c(0.001,0.002),lb=0.0001,ub=0.1)
param_info$durvieF <- list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
                                      c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
                        init_values=data.frame(c(200,300),c(250,350)),lb=50,ub=400)
eval(parse(text = "init_values <- CroptimizR:::get_init_values(param_info)"))
param_info_new <- param_info
param_info_new$dlaimax$init_values <- NULL
param_info_new$durvieF$init_values <- NULL
eval(parse(text = "param_info_new <- CroptimizR:::set_init_values(param_info, init_values)"))
eval(parse(text = "init_values_new <- CroptimizR:::get_init_values(param_info_new)"))

test_that("set_init_values: Case 2 with groups of situations per parameter", {
  expect_identical(init_values, init_values_new)
})

