#' @title Return a list of parameters active for a given situation
#'
#' @param sit_groups Defines the groups of situations for each parameter
#' A named list containing for each parameter the list of situations per group (`sit_list`)
#' @param situation The name of the situation
#' @param param_vec The (named) vector of parameters from which to extract values
#'
#' @details Names of parameters in `param_vec` and `sit_groups` should
#' be the same. If `param_vec` is not named, the parameters in `param_vec`
#' will be considered in the same order as in `sit_group`
#'
#' @return A subvector of `param_vec` containing only the values that are
#' active for the given situation
#'
#' @examples
#' sg=list(p1=list(sit_list=list(c("sit1","sit2","sit3"),c("sit4","sit5","sit6"))),p2=list(sit_list=list(c("sit1","sit2","sit3","sit4","sit5","sit6"))))
#' vec=c(1,2,3)
#' CroptimizR:::get_params_per_sit(sg,"sit2",vec)   # should give c(1,3)
#' CroptimizR:::get_params_per_sit(sg,"sit4",vec)   # should give c(2,3)
#' names(vec)=c("p2","p1","p1")
#' CroptimizR:::get_params_per_sit(sg,"sit2",vec)   # should give c(1,2)
#' CroptimizR:::get_params_per_sit(sg,"sit4",vec)   # should give c(1,3)
#'
get_params_per_sit <- function(sit_groups, situation, param_vec) {

  if (is.list(sit_groups[[1]])) {

    param_names=unique(names(param_vec))
    if ( !is.null(names(param_vec)) && all(sort(param_names)==sort(names(sit_groups))) ) {
      sit_groups=sit_groups[param_names]
    }
    index=sapply(sit_groups, function (x1) which(sapply(x1$sit_list, function(x2) is.element(situation,x2))))
    nb_groups=sapply(sit_groups, function (x) length(x$sit_list))
    nb_groups=c(0,nb_groups[-length(nb_groups)])
    index=index+nb_groups
    return(param_vec[index])

  } else {  # no groups of situations defined

    return(param_vec)

  }

}
