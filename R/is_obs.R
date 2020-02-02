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


is.obs <- function(obs_list) {

  # Check obs_list format
  if (!is.list(obs_list) || !all(sapply(obs_list, function(x) is.data.frame(x))) ) {
    warning("Incorrect format for argument obs_list. Should be a named list containing data.frames.")
    return(FALSE)
  }
  if (!all(sapply(obs_list, function(x) is.element("Date",colnames(x))))) {
    warning("Incorrect format for argument obs_list: all data.frames in the list must contain a column named Date.")
    return(FALSE)
  }
# Not mandatory ? seems that with APSIM it works without ...
#  if (!all(sapply(obs_list, function(x) all(lubridate::is.POSIXct(x$Date))))) {
#    warning("Incorrect format for argument obs_list: Date columns in data.frame must contains values in POSIXct format.")
#    return(FALSE)
#  }

  return(TRUE)

}
