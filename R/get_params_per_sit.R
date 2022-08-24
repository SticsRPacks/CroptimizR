#' @title Return a list of parameters active for a given situation
#'
#' @param sit_groups Defines the groups of situations for each parameter
#' A named list containing for each parameter the list of situations per group
#'  (`sit_list`)
#' @param situation The name of the situation
#' @param param_vec The (named)vector of parameters from which to extract values
#'
#' @details Names of parameters in `param_vec` and `sit_groups` should
#' be the same. If `param_vec` is not named, the parameters in `param_vec`
#' will be considered in the same order as in `sit_group`
#'
#' @return A subvector of `param_vec` containing only the values that are
#' active for the given situation
#'
#' @examples
#' sg <- list(
#'   p1 = list(sit_list = list(
#'     c("sit1", "sit2", "sit3"),
#'     c("sit4", "sit5", "sit6")
#'   )),
#'   p2 = list(sit_list = list(c(
#'     "sit1", "sit2", "sit3", "sit4", "sit5",
#'     "sit6"
#'   ))),
#'   p3 = list(sit_list = list(c("sit1", "sit2", "sit3"), c(
#'     "sit4", "sit5",
#'     "sit6"
#'   )))
#' )
#' vec <- c(1, 2, 3, 4, 5)
#' names(vec) <- CroptimizR:::get_params_names(sg)
#' CroptimizR:::get_params_per_sit(sg, "sit2", vec)
#' should give c(p1=1,p2=3,p3=4)
#' CroptimizR:::get_params_per_sit(sg, "sit4", vec)
#' should give c(p1=2,p2=3,p3=5)
#' @keywords internal
#'
get_params_per_sit <- function(sit_groups, situation, param_vec) {
  param_names <- CroptimizR:::get_params_names(sit_groups, short_list = TRUE)
  index <- sapply(
    sit_groups,
    function(x1) {
      which(sapply(
        x1$sit_list,
        function(x2) is.element(situation, x2)
      ))
    }
  )
  index <- unlist(index[sapply(index, function(x) length(x) > 0)])
  nb_groups <- sapply(sit_groups, function(x) length(x$sit_list))
  offset <- c(0, cumsum(nb_groups[-length(nb_groups)]))
  names(offset) <- names(nb_groups)
  index <- index + offset[names(index)]
  res <- stats::setNames(rep(NA, length(param_names)), param_names)
  res[names(index)] <- param_vec[index]
  return(res)
}
