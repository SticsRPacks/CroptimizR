#' @title Extract bounds from parameter information
#'
#' @inheritParams estim_param
#'
#' @details For the moment only works with uniform distributions but will be
#' hopefully soon extended to any distribution
#'
#' @return A list containing the vectors of lower and upper bounds (`ub` and `lb`)
#'
#' @examples
#'
#' # A simple case
#' param_info <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::get_params_bounds(param_info)
#'
#' # A case with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   lb = 0.0005, ub = 0.0025
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   lb = c(50, 100), ub = c(400, 500)
#' )
#' CroptimizR:::get_params_bounds(param_info)
#'
#'
#' @keywords internal
#'
get_params_bounds <- function(param_info) {
  if (!is.null(param_info$lb) && !is.null(param_info$ub)) {
    lb <- param_info$lb
    ub <- param_info$ub
  } else {
    lb <- unlist(sapply(param_info, function(x) x$lb))
    ub <- unlist(sapply(param_info, function(x) x$ub))
  }

  return(list(lb = lb, ub = ub))
}
