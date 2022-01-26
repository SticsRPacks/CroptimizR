#' @title Summarizes results of bayesian methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by bayesian method wrappers
#'
#' @return Prints results of bayesian methods
#'
#' @keywords internal
#'
summary_bayesian <- function(optim_options, param_info, optim_results) {

  param_names <- get_params_names(param_info)
  nb_params=length(param_names)
  path_results <- optim_options$path_results
  out <- optim_results$out

  ## Print results
  codaObject = getSample(out, start = 1, coda = TRUE)  # thin=1
  tmp=summary(codaObject)
  if (nb_params>=2) {
    summary(out) }
  else {
    print(tmp)
  }
  print(paste("Complementary graphs and results can be found in ", path_results))

}
