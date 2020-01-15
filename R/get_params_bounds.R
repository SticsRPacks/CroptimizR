#' @title Extract bounds from prior information
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
#' prior_information <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::get_params_bounds(prior_information)
#'
#' # A case with groups of situations per parameter
#' prior_information <- list()
#' prior_information$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   lb = 0.0005, ub = 0.0025
#' )
#' prior_information$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   lb = c(50, 100), ub = c(400, 500)
#' )
#' CroptimizR:::get_params_bounds(prior_information)
#'
#'
#' @keywords internal
#'
get_params_bounds <- function(prior_information) {
  if (!is.null(prior_information$lb) && !is.null(prior_information$ub)) {
    lb <- prior_information$lb
    ub <- prior_information$ub
  } else {
    lb <- unlist(sapply(prior_information, function(x) x$lb))
    ub <- unlist(sapply(prior_information, function(x) x$ub))
  }

  return(list(lb = lb, ub = ub))
}
