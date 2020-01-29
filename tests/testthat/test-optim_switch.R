test_that("Returns an error when unknown method is given", {
  expect_error(optim_switch(param_names=NULL,obs_list=NULL,crit_function=NULL,
               model_function=NULL,optim_method="unknown_method",
               param_info=NULL),
               "Unknown method")
})
