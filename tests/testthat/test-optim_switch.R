test_that("Returns an error when unknown method is given", {
  expect_error(
    eval(parse(
      text =
        "CroptimizR:::optim_switch(optim_method=\"unknown_method\",
                            optim_options=NULL, param_info=NULL,
                          crit_options=NULL)"
    )),
    "Unknown method"
  )
})
