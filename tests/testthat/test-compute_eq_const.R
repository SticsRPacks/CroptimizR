v_param_values <- c(p1 = 1, p2 = 10)
t_param_values <- tibble::tibble(
  situation = c("sit1", "sit2", "sit3"),
  p1 = c(1, 2, 3),
  p2 = c(10, 20, 30)
)
t_param_values_one_row <- tibble::tibble(
  situation = c("sit1"),
  p2 = c(10)
)

forced_param_values <- list(
  p3 = 100,
  p4 = "p2*100+1"
)

eval(parse(text = "v_new_forced_param_values <-
           CroptimizR:::compute_eq_const(forced_param_values, v_param_values)"))
eval(parse(text = "t_new_forced_param_values <-
           CroptimizR:::compute_eq_const(forced_param_values, t_param_values)"))
eval(parse(text = "t_new_forced_param_values_v2 <-
           CroptimizR:::compute_eq_const(forced_param_values,
           t_param_values_one_row)"))

test_that("correct format", {
  expect_true(is.vector(v_new_forced_param_values))
  expect_equal(length(v_new_forced_param_values), length(forced_param_values))
  expect_true(tibble::is_tibble(t_new_forced_param_values))
  expect_equal(nrow(t_new_forced_param_values), nrow(t_param_values))
  expect_equal(ncol(t_new_forced_param_values), length(forced_param_values))
  expect_equal(nrow(t_new_forced_param_values_v2), nrow(t_param_values_one_row))
  expect_equal(ncol(t_new_forced_param_values_v2), length(forced_param_values))
})

test_that("correct content", {
  expect_equal(v_new_forced_param_values, c(p3 = 100, p4 = 1001))
  expect_equal(v_new_forced_param_values, c(p3 = 100, p4 = 1001))
  expect_equal(t_new_forced_param_values, tibble::tibble(p3 = c(100, 100, 100),
                                                  p4 = c(1001, 2001, 3001)))
  expect_equal(t_new_forced_param_values_v2,
               tibble::tibble(p3 = c(100),
                              p4 = c(1001)))
})
