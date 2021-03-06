#' @title Extract param names from parameter information
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
#' param_info <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::get_params_names(param_info)
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
#'   lb = c(50, 50), ub = c(400, 400)
#' )
#' CroptimizR:::get_params_names(param_info)
#' CroptimizR:::get_params_names(param_info, short_list=TRUE)
#'
#'
#' @keywords internal
#'
get_params_names <- function(param_info, short_list = FALSE) {
  if (!is.null(param_info$lb) && !is.null(param_info$ub)) {
    return(names(param_info$lb))
  } else {
    if (short_list) {
      res <- names(param_info)
    } else {
      nb_groups <- sapply(param_info, function(x) length(x$sit_list))
      nb_groups[nb_groups==0]=1
      nb_params_sl <- length(nb_groups)
      # build suffix
      suffix <- rep("", sum(nb_groups))
      count=1
      for (i in 1:nb_params_sl) {
        if (nb_groups[i] > 1) {
          suffix[count:(count+nb_groups[i]-1)] <- as.character(1:nb_groups[i])
          count=count+nb_groups[i]
        } else {
          count=count+1
        }
      }

      # The name of the parameter is replicated by its number of groups and a number is added
      res <- names(param_info)[unlist(sapply(1:length(nb_groups), function(x) rep(x, nb_groups[x])))]
      res <- paste0(res, suffix)
    }

    return(res)
  }
}
