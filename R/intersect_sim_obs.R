intersect_sim_obs <- function(sim_list,obs_list) {
  #' @title Intersect simulation and observation lists
  #'
  #' @param sim_list List of simulated values
  #' @param obs_list List of observed values
  #'
  #' @return A list containing the new obs and sim lists having same situations, variables, dates
  #'
  #' @export
  #'
  #' @examples


  situations=intersect(names(sim_list),names(obs_list))
  if (length(situations)<length(names(sim_list))) {
    sim_list[[setdiff(names(sim_list),situations)]]=NULL
  }
  if (length(situations)<length(names(obs_list))) {
    obs_list[[setdiff(names(obs_list),situations)]]=NULL
  }

  list_vars=sapply(situations, function (x) intersect(colnames(sim_list[[x]]),colnames(obs_list[[x]])))
  sim_list=sapply(situations, function(x) sim_list[[x]][,is.element(colnames(sim_list[[x]]),list_vars[[x]])])
  obs_list=sapply(situations, function(x) obs_list[[x]][,is.element(colnames(obs_list[[x]]),list_vars[[x]])])

  list_dates=sapply(situations, function (x) intersect(sim_list[[x]]$Date,obs_list[[x]]$Date))
  sim_list=sapply(situations, function(x) sim_list[[x]][is.element(sim_list[[x]]$Date,list_dates[[x]]),])
  obs_list=sapply(situations, function(x) obs_list[[x]][is.element(obs_list[[x]]$Date,list_dates[[x]]),])


  return(list(sim_list=sim_list,obs_list=obs_list))


}
