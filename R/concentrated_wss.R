concentrated_wss <- function(sim_list,obs_list) {
  #' @title Compute concentrated version of weighted sum of squares from simulation and observation list. More details about this criterion are given in Wallach et al., 2011 (equation 5).
  #'
  #' @param sim_list List of simulated variables
  #' @param obs_list List of observed variables
  #'
  #' @details `sim_list` and `obs_list` must have the same structure (i.e. same number of variables, dates, situations, ... use intersect_sim_obs for that).
  #'
  #' @return The value of the concentrated weighted sum of squares
  #'
  #' @export

  var_list=unique(unlist(lapply(obs_list,function (x) colnames(x))))
  var_list=setdiff(var_list,"Date")

  result=1

  for (var in var_list) {
    obs=unlist(sapply(obs_list,function (x) x[is.element(colnames(x),var)]))
    sim=unlist(sapply(sim_list,function (x) x[is.element(colnames(x),var)]))
    res=obs-sim
    res=res[!is.na(res)]

    sz=length(res)

    result=result *((1/sz)*(res%*%res))^(sz/2)
  }

  return(result)
}
