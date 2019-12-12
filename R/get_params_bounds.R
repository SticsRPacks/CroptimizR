#' @title Extract bounds from prior information
#'
#' @param prior_information Prior information on the parameters to estimate.
#' For the moment only uniform distribution are allowed.
#' Either a list containing (named) vectors of upper and lower
#' bounds (`ub` and `lb`), or a named list containing for each
#' parameter the list of situations per group (`sit_list`)
#' and the vector of upper and lower bounds (one value per group) (`ub` and `lb`)
#'
#' @details For the moment only works with uniform distributions but will be
#' hopefully soon extended to any distribution
#'
#' @return A list containing the vectors of lower and upper bounds (`ub` and `lb`)
#'
#' @examples
#' \dontrun{
#' # A simple case
#' prior_information=list(lb=c(dlaimax=0.0005, durvieF=50),
#'                        ub=c(dlaimax=0.0025, durvieF=400))
#' get_params_bounds(prior_information)
#'
#' # A case with groups of situations per parameter
#' prior_information=list()
#' prior_information$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1", "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),lb=0.0005,ub=0.0025)
#' prior_information$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"), c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),lb=c(50,50),ub=c(400,400))
#' SticsOptimizR:::get_params_bounds(prior_information)
#' }

get_params_bounds <- function(prior_information) {

  if (!is.null(prior_information$lb) && !is.null(prior_information$ub)) {
    lb=prior_information$lb
    ub=prior_information$ub
  } else {
    lb=unlist(sapply(prior_information, function(x) x$lb))
    ub=unlist(sapply(prior_information, function(x) x$ub))
  }

  return(list(lb=lb,ub=ub))

}
