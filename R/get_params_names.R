#' @title Extract param names from prior information
#'
#' @inheritParams estim_param
#'
#' @param short_list TRUE to return a list without replicated parameters which
#' happens for simultaneous estimation of specific and varietal parameters
#' (optional, default=FALSE)
#'
#' @return A vector of parameter names
#'
#' @examples
#'
#' # A simple case
#' prior_information <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::get_params_names(prior_information)
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
#'   lb = c(50, 50), ub = c(400, 400)
#' )
#' CroptimizR:::get_params_names(prior_information)
#'
#'
#' @keywords internal
#'
get_params_names <- function(prior_information, short_list = FALSE) {
  if (!is.null(prior_information$lb) && !is.null(prior_information$ub)) {
    return(names(prior_information$lb))
  } else {
    if (short_list) {
      res <- names(prior_information)
    } else {
      nb_groups <- sapply(prior_information, function(x) length(x$sit_list))

      # build suffix
      suffix <- rep("", sum(nb_groups))
      for (i in 1:length(nb_groups)) {
        if (nb_groups[i] > 1) {
          suffix[(sum(nb_groups[1:(i - 1)]) + 1):sum(nb_groups[1:i])] <- as.character(1:nb_groups[i])
        }
      }

      # The name of the parameter is replicated by its number of groups and a number is added
      res <- names(prior_information)[unlist(sapply(1:length(nb_groups), function(x) rep(x, nb_groups[x])))]
      res <- paste0(res, suffix)
    }

    return(res)
  }
}
