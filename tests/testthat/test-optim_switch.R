test_that("Returns an error when unknown method is given", {
  expect_error(optim_switch(param_names=NULL,optim_method="unknown_method",
               param_info=NULL, crit_options=NULL),
               "Unknown method")
})
