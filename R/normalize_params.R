#' @title Normalize parameters
#'
#' @inheritParams estim_param
#'
#' @details first version just for a test ... should include different ways of handling
#' normalization (from bounds or from given values) + handle the different shapes of param_info ...
#'
#' @return Returns normalized values for param_info
#'
#' @examples
#'
#'
#' @keywords internal
#'
normalize_params <- function(param_info) {

  param_names <- names(param_info$lb)
  nb_param <- length(param_names)
  param_info_new <- list()
  param_info_new$lb <- setNames(rep(0,nb_param),param_names)
  param_info_new$ub <- setNames(rep(1,nb_param),param_names)

  if (is.null(nrow(param_info$init_values))) {
    param_info_new$init_values <- (param_info$init_values - param_info$lb ) /
      ( param_info$ub - param_info$lb )
  } else {
    lb <- as.data.frame(as.list(param_info$lb))
    ub <- as.data.frame(as.list(param_info$ub))
    n <- nrow(param_info$init_values)
    param_info_new$init_values <- ( param_info$init_values - lb[rep(1,n),] )  /
      ( ub[rep(1,n),] - lb[rep(1,n),] )
  }


  return(param_info_new)

}

#' @title Denormalize parameters
#'
#' @inheritParams estim_param
#' @param param_values Named vector of normalized parameters to denormalize.
#'
#' @details first version just for a test ... should include different ways of handling
#' normalization (from bounds or from given values) + handle the different shapes of param_info ...
#'
#' @return Returns denormalized values of parameters
#'
#' @examples
#'
#' @keywords internal
#'
denormalize_params <- function(param_info, param_values) {

  if (is.null(nrow(param_values))) {
    param_values <- param_info$lb + param_values  * ( param_info$ub - param_info$lb )
  } else {
    lb <- as.data.frame(as.list(param_info$lb))
    ub <- as.data.frame(as.list(param_info$ub))
    n <- nrow(param_values)
    param_values <- lb[rep(1,n),] + param_values  * ( ub[rep(1,n),] - lb[rep(1,n),] )
  }

  return(param_values)

}
