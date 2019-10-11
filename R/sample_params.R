#' @title sample values in given distribution
#'
#' @param bounds_list Either a (optionnally named) list containing a vector of
#' upper and lower bounds (\code{ub} and \code{lb}), or a named list containing
#' for each parameter the list of situations per group (\code{sit_list})
#' and the vector of upper and lower bounds (one value per group) (\code{ub} and
#' \code{lb})
#' @param n The number of values to sample for each parameter
#'
#' @details For the moment only works with uniform distributions but will be
#' hopefully soon extended to any distribution. Use randomized LHS (from
#' DiceDesign package)
#'
#' @return A vector or data.frame containing the sampled values (nrow=n)
#'
#' @export
#'
#' @examples
#' library(DiceDesign)
#' bounds_list=list(lb=c(0,1,2),ub=c(1,2,3))
#' sample_params(bounds_list,5)
#'
sample_params <- function(bounds_list,n) {

  bounds=get_params_bounds(bounds_list)

  nb_params=length(bounds$lb)

  out <- lhsDesign(n, dimension=nb_params)$design
  res=sapply(1:nb_params,
             function(x) bounds$lb[x]+out[,x]*(bounds$ub[x]-bounds$lb[x]))
  res=matrix(res,nrow=n,ncol=nb_params)
  colnames(res)=names(bounds$lb)

  return(res)

}
