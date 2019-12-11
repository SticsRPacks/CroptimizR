#' @title Intersect simulation and observation lists
#'
#' @param sim_list List of simulated values
#' @param obs_list List of observed values
#'
#' @return A list containing the new obs and sim lists having same situations, variables, dates
#'
# @examples
intersect_sim_obs <- function(sim_list,obs_list) {

  # Intersect situations
  situations=intersect(names(sim_list),names(obs_list))
  if (length(situations)<length(names(sim_list))) {
    sim_list[[setdiff(names(sim_list),situations)]]=NULL
  }
  if (length(situations)<length(names(obs_list))) {
    obs_list[[setdiff(names(obs_list),situations)]]=NULL
  }

  # Intersect variables
  list_vars=sapply(situations,
                   function (x) intersect(colnames(sim_list[[x]]),
                               colnames(obs_list[[x]])),simplify = F)
  idx=sapply(list_vars, function(x) length(x)>=2)  # keep situations with common variables
  situations=situations[idx]
  list_vars=list_vars[situations]
  if (length(list_vars)==0) {
    warning("Simulations and observations do not contain common variables.")
    return(NA)
  }

  # Intersect dates
  sim_list=sapply(situations,
                  function(x) sim_list[[x]][,is.element(colnames(sim_list[[x]]),list_vars[[x]])], simplify = F)
  obs_list=sapply(situations,
                  function(x) obs_list[[x]][,is.element(colnames(obs_list[[x]]),list_vars[[x]])], simplify = F)
  list_dates=sapply(situations,
                    function (x) intersect(sim_list[[x]]$Date,obs_list[[x]]$Date), simplify = F)
  idx=sapply(list_dates, function(x) length(x)>0)  # keep situations with common dates
  situations=situations[idx]
  list_dates=list_dates[situations]
  if (length(list_dates)==0) {
    warning("Simulations and observations do not contain common dates")
    return(NA)
  }

  sim_list=sapply(situations,
                  function(x) sim_list[[x]][is.element(sim_list[[x]]$Date,list_dates[[x]]),], simplify = F)
  obs_list=sapply(situations,
                  function(x) obs_list[[x]][is.element(obs_list[[x]]$Date,list_dates[[x]]),], simplify = F)

  return(list(sim_list=sim_list,obs_list=obs_list))


}
