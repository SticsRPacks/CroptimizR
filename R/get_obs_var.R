#' @title Get names of observed variables
#'
#' @inheritParams estim_param
#'
#' @return Vector of names of observed variables
#'
#' @export
#'
get_obs_var <- function(obs_list) {
  # Test obs_list format
  if (!is.obs(obs_list)) {
    stop("Incorrect format for argument obs_list.")
  }

  # Get unique variable names from the observation list
  reserved_keywords <- get_reserved_keywords() # names of columns that are not variables
  unique(
    unlist(
      lapply(
        obs_list,
        function(x) setdiff(names(x), reserved_keywords)
      )
    )
  )
}
