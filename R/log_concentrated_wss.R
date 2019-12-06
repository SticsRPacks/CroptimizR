log_concentrated_wss <- function(sim_list,obs_list) {
  #' @title Compute log transformation of concentrated version of weighted sum of squares (see function concentrated_wss) from simulation and observation list
  #'
  #' @param sim_list List of simulated variables
  #' @param obs_list List of observed variables
  #'
  #' @details This criterion can be useful in place of concentrated_wss if the sum of residues are null for a given situation (e.g. when one optimize on integers such as phenological stages days ...)
  #' `sim_list` and `obs_list` must have the same structure (i.e. same number of variables, dates, situations, ... use intersect_sim_obs for that).
  #'
  #' @return The value of the log of the concentrated weighted sum of squares
  #'
  #' @export

  var_list=unique(unlist(lapply(obs_list,function (x) colnames(x))))
  var_list=setdiff(var_list,"Date")

  result=0

  for (var in var_list) {
    obs=unlist(sapply(obs_list,function (x) x[is.element(colnames(x),var)]))
    sim=unlist(sapply(sim_list,function (x) x[is.element(colnames(x),var)]))
    res=obs-sim
    res=res[!is.na(res)]

    sz=length(res)

    result=result + (sz/2)*(log(1/sz)+log(res%*%res+1e-300))
  }

  return(result)
}
