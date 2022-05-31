context("Test the test_wrapper function")

wrapper_OK <- function(param_values, ...){
  results <- list()
  results$error <- FALSE
  results$sim_list <- list(sit1=data.frame(as.list(param_values),
                                           Date=as.POSIXct(c("2009-11-30"))))
  return(results)
}
res <- test_wrapper(model_function = wrapper_OK,
                    model_options = NULL,param_values=c(P1=1,P2=2),
                    situation=NULL)
test_that("Wrapper OK", {
  expect_true(all(res$test_results))
})

wrapper_fail_test1 <- function(param_values, ...){
  results <- list()
  results$error <- FALSE
  results$sim_list <- param_values
  return(results)
}
res <- suppressWarnings(test_wrapper(model_function = wrapper_fail_test1,
                    model_options = NULL,param_values=c(P1=1,P2=2),
                    situation=NULL))
test_that("Wrapper OK", {
  expect_false(res$test_results["test1"])
  expect_true(res$test_results["test2"])
  expect_true(res$test_results["test3"])
})

wrapper_fail_test2 <- function(param_values, ...){
  results <- list()
  results$error <- FALSE
  results$sim_list <- list(sit1=data.frame(var=runif(1,0,1),
                                           Date=as.POSIXct(c("2009-11-30"))))
  return(results)
}
res <- suppressWarnings(test_wrapper(model_function = wrapper_fail_test2,
                    model_options = NULL,param_values=c(P1=1,P2=2),
                    situation=NULL))
test_that("Wrapper fails test 2", {
  expect_true(res$test_results["test1"])
  expect_false(res$test_results["test2"])
  expect_true(res$test_results["test3"])
})

wrapper_fail_test3 <- function(param_values, ...){
  results <- list()
  results$error <- FALSE
  results$sim_list <- list(sit1=data.frame(var=1,
                                           Date=as.POSIXct(c("2009-11-30"))))
  return(results)
}
res <- suppressWarnings(test_wrapper(model_function = wrapper_fail_test3,
                    model_options = NULL,param_values=c(P1=1,P2=2),
                    situation=NULL))
test_that("Wrapper fails test 3", {
  expect_true(res$test_results["test1"])
  expect_true(res$test_results["test2"])
  expect_false(res$test_results["test3"])
})

