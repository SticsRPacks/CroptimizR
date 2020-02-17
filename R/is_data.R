#' @title Check format of data storage (for simulation results or observations)
#'
#' @param data_list List of values to check
#'
#' @return TRUE if data_list has the expected format, FALSE otherwise (+ warnings)
#'
#' @details data is expected to be stored in a vector of named list containing for
#' each situation a data.frame with a column "Date" plus one column per variable
#'
#' @examples
#'
#' data_list <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' CroptimizR:::is.data(data_list)
#'
#' # Missing Date column
#' data_list <- list(sit1=data.frame(var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' CroptimizR:::is.data(data_list)
#'
#' # Bad Date format
#' data_list <- list(sit1=data.frame(Date=c("2009-11-30","2009-12-10"),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=c("2009-11-30","2009-12-5"),var1=c(1.3,2)))
#' CroptimizR:::is.data(data_list)
#'
#' @keywords internal
#'
is.data <- function(data_list) {

  # Check data_list format
  if (!is.list(data_list) || !all(sapply(data_list, function(x) is.data.frame(x))) ) {
    warning("Incorrect format: Should be a named list containing data.frames.")
    return(FALSE)
  }
  # Check existence of Date column
  if (!all(sapply(data_list, function(x) is.element("Date",colnames(x))))) {
    warning("Incorrect format: all data.frames in the list must contain a column named Date.")
    return(FALSE)
  }
  # Check format of Date column
  if (!all(sapply(data_list, function(x) all(lubridate::is.POSIXct(x$Date)))) &&
      !all(sapply(data_list, function(x) all(lubridate::is.Date(x$Date)))) ) {
    warning("Incorrect format: Date column in data.frame must contain values in Date or POSIXct format.")
    return(FALSE)
  }

  return(TRUE)

}


#' @title Check format of simulated data list
#'
#' @param sim_list List of simulation results to check
#'
#' @return TRUE if sim_list has the good format, FALSE otherwise (+ warnings)
#'
#' @examples
#'
#' sim_list <- vector("list",2)
#' sim_list[[1]] <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' sim_list[[2]] <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' CroptimizR:::is.sim(sim_list)
#'
#' # Missing Date column
#' sim_list <- vector("list",2)
#' sim_list[[1]] <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' sim_list[[2]] <- list(sit1=data.frame(var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' CroptimizR:::is.sim(sim_list)
#'
#' # Bad Date format
#' sim_list <- vector("list",1)
#' sim_list[[1]] <- list(sit1=data.frame(Date=c("2009-11-30","2009-12-10"),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=c("2009-11-30","2009-12-5"),var1=c(1.3,2)))
#' CroptimizR:::is.sim(sim_list)
#'
#' @keywords internal
#'
is.sim <- function(sim_list) {

  if (!all(sapply(sim_list, CroptimizR:::is.data))) {
    warning("Variable storing simulated data has an incorrect format.")
    return(FALSE)
  }

  return(TRUE)

}


#' @title Check format of observation list
#'
#' @param obs_list List of observed values to check
#'
#' @return TRUE if obs_list has the good format, FALSE otherwise (+ warnings)
#'
#' @examples
#'
#' obs_list <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' CroptimizR:::is.obs(obs_list)
#'
#' # Missing Date column
#' obs_list <- list(sit1=data.frame(var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),var1=c(1.3,2)))
#' CroptimizR:::is.obs(obs_list)
#'
#' # Bad Date format
#' obs_list <- list(sit1=data.frame(Date=c("2009-11-30","2009-12-10"),var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=c("2009-11-30","2009-12-5"),var1=c(1.3,2)))
#' CroptimizR:::is.obs(obs_list)
#'
#' @keywords internal
#'
is.obs <- function(obs_list) {

  if (!CroptimizR:::is.data(obs_list)) {
    warning("Variable storing observed data has an incorrect format.")
    return(FALSE)
  }

return(TRUE)

}

