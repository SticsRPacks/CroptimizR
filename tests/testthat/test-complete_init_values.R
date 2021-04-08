context("Test the complete_init_values function")


# No user initial values provided nor constraints
lb = c(dlaimax = 0.0005, durvieF = 50)
ub = c(dlaimax = 0.0025, durvieF = 400)
init_values = data.frame(dlaimax=c(0.001, NA), durvieF=c(75, 100))
satisfy_par_const = function(param_values, ...) {
  if (param_values$dlaimax<0.001) return(FALSE)
  return(TRUE)
}
test_that("Initial values and constraints", {
  local_edition(3)
  res1 <- CroptimizR:::complete_init_values(init_values=NULL, nb_values=5, lb=lb, ub=ub,
                                    ranseed=1234)
  res2 <- CroptimizR:::complete_init_values(init_values=init_values, nb_values=5, lb=lb, ub=ub,
                                           ranseed=1234)
  res3 <- CroptimizR:::complete_init_values(init_values=init_values, nb_values=5, lb=lb, ub=ub,
                                          satisfy_par_const=satisfy_par_const,
                                           ranseed=1234)
  expect_snapshot(res1)
  expect_snapshot(res2)
  expect_snapshot(res3)
})
