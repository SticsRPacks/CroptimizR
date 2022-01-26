#' @title Return reserved keywords that should not be used as (observed/simulated) variables names
#'
#' @return A vector including the names of the keywords
#'
#' @keywords internal
#'
get_reserved_keywords <- function() {
 return(c("Date","Plant"))
}
