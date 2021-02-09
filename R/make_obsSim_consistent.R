#' @title Check the consistency of observation and simulation lists and make them consistent if possible
#'
#' @param sim_list List of simulated values
#' @param obs_list List of observed values
#'
#' @return A list containing consistent obs and sim lists
#'
#' @details This function checks:
#' (i) the homogeneity of types of obs and sim Date columns
#' (ii) that variables in obs and sim list have same types and are numeric
#'
#' @keywords internal
#'
make_obsSim_consistent <- function(sim_list, obs_list) {

  # Check homogeneity of types of obs and sim Date columns
  # ------------------------------------------------------

  isPosixObs <- sapply(obs_list,function(x) lubridate::is.POSIXct(x$Date)); isAllPosixObs <- all(isPosixObs)
  isDateObs <- sapply(obs_list,function(x) lubridate::is.Date(x$Date)); isAllDateObs <- all(isDateObs)
  isPosixSim <- sapply(sim_list,function(x) lubridate::is.POSIXct(x$Date)); isAllPosixSim <- all(isPosixSim)
  isDateSim <- sapply(sim_list,function(x) lubridate::is.Date(x$Date)); isAllDateSim <- all(isDateSim)

  if (!all(isPosixObs | isDateObs)) {
    stop(paste("Error: incorrect format for Date column of observation list for situations",
               paste(names(obs_list)[!(isPosixObs | isDateObs)],collapse = ", "),". \n Date columns should be of type Date or POSIXct."))
  }
  if (!all(isPosixSim | isDateSim)) {
    stop(paste("Error: incorrect format for Date column of simulation list for situations",
               paste(names(sim_list)[!(isPosixSim | isDateSim)],collapse = ", "),". \n Date columns should be of type Date or POSIXct."))
  }

  # if types of obs$*$Date are not homogeneous => convert obs$*$Date to the format of sim$*$Date
  if (!isAllPosixObs && !isAllDateObs) {
    if (isAllPosixSim) {
      sapply(names(obs_list),function(x) obs_list[[x]]$Date <<- as.POSIXct(as.character(obs_list[[x]]$Date),tz="UTC"))
      isAllPosixObs <-TRUE
    } else if (isAllDateSim) {
      sapply(names(obs_list),function(x) obs_list[[x]]$Date <<- as.Date(obs_list[[x]]$Date))
      isAllDateObs <-TRUE
    } else {
      sapply(names(obs_list),function(x) obs_list[[x]]$Date <<- as.POSIXct(as.character(obs_list[[x]]$Date),tz="UTC"))
      isAllPosixObs <-TRUE
    }
  }

  # if types of sim$*$Date are not homogeneous => convert sim$*$Date to the format of obs$*$Date
  if (!isAllPosixSim && !isAllDateSim) {
    if (isAllPosixObs) {
      sapply(names(sim_list),function(x) sim_list[[x]]$Date <<- as.POSIXct(as.character(sim_list[[x]]$Date),tz="UTC"))
      isAllPosixSim <-TRUE
    } else if (isAllDateObs) {
      sapply(names(sim_list),function(x) sim_list[[x]]$Date <<- as.Date(sim_list[[x]]$Date))
      isAllDateSim <-TRUE
    }
  }

  # if types of Date in Sim and Obs are different => convert obs$*$Date to the format of sim$*$Date
  if (isAllPosixSim && isAllDateObs) {
    sapply(names(obs_list),function(x) obs_list[[x]]$Date <<- as.POSIXct(as.character(obs_list[[x]]$Date),tz="UTC"))
  } else if (isAllDateSim && isAllPosixObs) {
    sapply(names(obs_list),function(x) obs_list[[x]]$Date <<- as.Date(obs_list[[x]]$Date))
  }


  # Check that variables in obs and sim list have same types
  # --------------------------------------------------------

  situations <- intersect(names(obs_list),names(sim_list))
  nonCoherent_sit <- NULL
  nonNumeric_sit <- NULL
  for (situation in situations) {
    var_names <- intersect(names(obs_list[[situation]]),names(sim_list[[situation]]))
    var_names <- setdiff(var_names,c("Date","Plant"))
    if (!all(sapply(var_names,function(x) class(obs_list[[situation]][[x]])[[1]]==class(sim_list[[situation]][[x]])[[1]]))) {
      nonCoherent_sit <- c(nonCoherent_sit,situation)
    }
    if (!all(sapply(var_names,function(x) is.numeric(obs_list[[situation]][[x]])))) {
      nonNumeric_sit <- c(nonNumeric_sit,situation)
    }
  }
  if (!is.null(nonCoherent_sit)) {
    stop(paste("Error: some variables in observation and simulation lists have different types for situations",
               paste(nonCoherent_sit,collapse = ",")))
  }
  if (!is.null(nonNumeric_sit)) {
    warning(paste("some variables in observation and simulation lists are non numeric for situations",
                  paste(nonNumeric_sit,collapse = ",")))
  }

  return(list(obs_list=obs_list,sim_list=sim_list))
}
