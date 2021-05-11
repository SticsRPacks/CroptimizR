#' @title sample values in given distribution
#'
#' @param bounds_list Either a (optionnally named) list containing a vector of
#' upper and lower bounds (`ub` and `lb`), or a named list containing
#' for each parameter the list of situations per group (`sit_list`)
#' and the vector of upper and lower bounds (one value per group) (`ub` and
#' `lb`)
#' @param n The number of values to sample for each parameter
#' @param seed The seed of the random generator (optional, default value=NULL)
#'
#' @details For the moment only works with uniform distributions but will be
#' hopefully soon extended to any distribution. Use genetic LHS (from
#' lhs package)
#'
#' @return A vector or data.frame containing the sampled values (nrow=n)
#'
#' @examples
#' bounds_list <- list(lb = c(0, 1, 2), ub = c(1, 2, 3))
#' CroptimizR:::sample_params(bounds_list, 5)
#' @keywords internal
#'
sample_params <- function(bounds_list, n, seed = NULL) {
  bounds <- get_params_bounds(bounds_list)

  nb_params <- length(bounds$lb)

  if (!is.null(seed)) set.seed(seed)
  out <- lhs::geneticLHS(n = n, k = nb_params)
  res <- sapply(
    1:nb_params,
    function(x) bounds$lb[x] + out[, x] * (bounds$ub[x] - bounds$lb[x])
  )
  res <- matrix(res, nrow = n, ncol = nb_params)
  colnames(res) <- names(bounds$lb)

  return(res)
}
