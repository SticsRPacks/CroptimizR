#' @title Check format of data storage (for simulation results or observations)
#'
#' @param data_list List of values to check
#'
#' @return TRUE if data_list has the expected format,
#' FALSE otherwise (+ warnings)
#'
#' @details data is expected to be stored in a vector of named list containing
#' for each situation a data.frame with a column "Date" plus one column per
#'  variable
#'
#' @examples
#' \donttest{
#' data_list <- list(
#'   sit1 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
#'     var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
#'   ),
#'   sit2 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
#'     var1 = c(1.3, 2)
#'   )
#' )
#' CroptimizR:::is.data(data_list)
#'
#' # Missing Date column
#' data_list <- list(
#'   sit1 = data.frame(var1 = c(1.1, 1.5), var2 = c(NA, 2.1)),
#'   sit2 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
#'     var1 = c(1.3, 2)
#'   )
#' )
#' CroptimizR:::is.data(data_list)
#'
#' # Bad Date format
#' data_list <- list(
#'   sit1 = data.frame(
#'     Date = c("2009-11-30", "2009-12-10"),
#'     var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
#'   ),
#'   sit2 = data.frame(Date = c("2009-11-30", "2009-12-5"), var1 = c(1.3, 2))
#' )
#' CroptimizR:::is.data(data_list)
#' }
#'
#' @keywords internal
#'
is.data <- function(data_list) {
  # Check data_list format
  if (length(data_list) == 0 || !is.list(data_list)) {
    warning("Incorrect format: Should be a (non-empty) list.")
    return(FALSE)
  }
  if (is.null(names(data_list))) {
    warning("Incorrect format: Should be a named list.")
    return(FALSE)
  }
  if (!all(sapply(
    data_list,
    function(x) is.data.frame(x)
  ))) {
    warning("Incorrect format: The named list must contain data.frames.")
    return(FALSE)
  }
  # Check existence of Date column
  if (!all(sapply(data_list, function(x) is.element("Date", colnames(x))))) {
    warning("Incorrect format: all data.frames in the list must contain a column named Date.")
    return(FALSE)
  }
  # Check format of Date column
  if (!all(sapply(data_list, function(x) all(lubridate::is.POSIXct(x$Date)))) &&
    !all(sapply(data_list, function(x) all(lubridate::is.Date(x$Date))))) {
    warning("Incorrect format: Date column in data.frame must contain values in Date or POSIXct format.")
    return(FALSE)
  }
  # Check there are no replicated Dates
  if (any(sapply(
    data_list,
    function(x) length(unique(x$Date)) < length(x$Date)
  ))) {
    warning(paste(
      "Incorrect format, Date column include replicated dates for situations",
      paste(
        names(data_list)[sapply(
          data_list,
          function(x) length(unique(x$Date)) < length(x$Date)
        )],
        collapse = ","
      )
    ))
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
#' \donttest{
#' sim_list <- list(
#'   sit1 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
#'     var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
#'   ),
#'   sit2 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
#'     var1 = c(1.3, 2)
#'   )
#' )
#' CroptimizR:::is.sim(sim_list)
#'
#' # Missing Date column
#' sim_list <- list(
#'   sit1 = data.frame(var1 = c(1.1, 1.5), var2 = c(NA, 2.1)),
#'   sit2 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
#'     var1 = c(1.3, 2)
#'   )
#' )
#' CroptimizR:::is.sim(sim_list)
#'
#' # Bad Date format
#' sim_list <- list(
#'   sit1 = data.frame(
#'     Date = c("2009-11-30", "2009-12-10"),
#'     var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
#'   ),
#'   sit2 = data.frame(Date = c("2009-11-30", "2009-12-5"), var1 = c(1.3, 2))
#' )
#' CroptimizR:::is.sim(sim_list)
#' }
#'
#' @keywords internal
#'
is.sim <- function(sim_list) {
  if (!is.data(sim_list)) {
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
#' \donttest{
#' obs_list <- list(
#'   sit1 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-10")),
#'     var1 = c(1.1, 1.5), var2 = c(NA, 2.1)
#'   ),
#'   sit2 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
#'     var1 = c(1.3, 2)
#'   )
#' )
#' # CroptimizR:::is.obs(obs_list)
#'
#' # Missing Date column
#' obs_list <- list(
#'   sit1 = data.frame(var1 = c(1.1, 1.5), var2 = c(NA, 2.1)),
#'   sit2 = data.frame(
#'     Date = as.POSIXct(c("2009-11-30", "2009-12-5")),
#'     var1 = c(1.3, 2)
#'   )
#' )
#' # CroptimizR:::is.obs(obs_list)
#'
#' # Bad Date format
#' obs_list <- list(
#'   sit1 = data.frame(
#'     Date = c("2009-11-30", "2009-12-10"), var1 = c(1.1, 1.5),
#'     var2 = c(NA, 2.1)
#'   ),
#'   sit2 = data.frame(Date = c("2009-11-30", "2009-12-5"), var1 = c(1.3, 2))
#' )
#' # CroptimizR:::is.obs(obs_list)
#' }
#'
#' @keywords internal
#'
is.obs <- function(obs_list) {
  if (!is.data(obs_list)) {
    warning("Variable storing observed data has an incorrect format.")
    return(FALSE)
  }

  return(TRUE)
}
