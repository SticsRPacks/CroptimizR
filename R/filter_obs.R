#' @title Filter observation list to exclude situations, variables or dates
#'
#' @inheritParams estim_param
#' @param var (optional, if not given all variables will be kept) Vector containing the names of the variables to include or exclude
#' @param situation (optional, if not given all situations will be kept) Vector containing the names of the situations to include or exclude
#' @param dates (optional, if not given all dates will be kept) Vector containing the dates (POSIXct format) to include or exclude
#' @param include (optional, FALSE by default) Flag indicating if the variables / situations / dates listed in inputs must be included (TRUE) or not (FALSE) in the resulting list
#' @param var_names `r lifecycle::badge("deprecated")` `var_names` is no
#'   longer supported, use `var` instead.
#' @param sit_names `r lifecycle::badge("deprecated")` `sit_names` is no
#'   longer supported, use `situation` instead.
#'
#' @return obs_list List of filtered observed values (same format as `obs_list` input argument)
#'
#' @seealso For more detail and examples, see the different vignettes in
#' [CroptimizR website](https://sticsrpacks.github.io/CroptimizR/)
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' obs_list <- list(sit1=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-10")),
#'                                  var1=c(1.1,1.5),var2=c(NA,2.1)),
#'                  sit2=data.frame(Date=as.POSIXct(c("2009-11-30","2009-12-5")),
#'                                  var1=c(1.3,2)))
#'
#' # Keep only var1
#' filter_obs(obs_list,var=c("var1"),include=TRUE)
#'
#' # Exclude observations at date "2009-11-30"
#' filter_obs(obs_list,dates=as.POSIXct(c("2009-11-30")))
#'
#'

filter_obs <- function(obs_list, var=NULL, situation=NULL, dates=NULL,
                       include=FALSE, var_names=lifecycle::deprecated(),
                       sit_names=lifecycle::deprecated()) {

  # Managing parameter names changes between versions:
  if (lifecycle::is_present(sit_names)) {
    lifecycle::deprecate_warn("0.5.0", "filter_obs(sit_names)", "filter_obs(situation)")
  } else {
    sit_names <- situation # to remove when we update inside the function
  }
  if (lifecycle::is_present(var_names)) {
    lifecycle::deprecate_warn("0.5.0", "filter_obs(var_names)", "filter_obs(var)")
  } else {
    var_names <- var # to remove when we update inside the function
  }

  # Check obs_list format
  if (is.null(obs_list)) {
    stop("obs_list is NULL.")
  }
  if (!CroptimizR:::is.obs(obs_list)) {
    stop("Incorrect format for argument obs_list.")
  }

  # Filter Situations
  ## check that sit_names are in obs_list
  if (!is.null(sit_names)) {
    tmp=intersect(sit_names,names(obs_list))
    if (is.null(tmp) || !setequal(tmp,sit_names)) {
      warning("Argument sit_names contains situations that are not included in obs_list. \n obs_list contains: ",paste(names(obs_list), collapse=" "))
      sit_names=tmp
    }
    ## Filter
    if (include) {
      obs_list=obs_list[sit_names]
    } else {
      obs_list[sit_names]=NULL
      if (length(obs_list)==0) {
        warning("All situations have been excluded from the list")
        return(NULL)
      }
    }
  }


  # Transform obs_list in a data.frame for easier filtering of var and dates
  df=dplyr::bind_rows(obs_list,.id="id")

  # Filter Variables
  ## check that var_names are in obs_list
  if (!is.null(var_names)) {
    tmp=intersect(var_names,names(df))
    if (is.null(tmp) || !setequal(tmp,var_names)) {
      warning("Argument var_names contains variables that are not included in obs_list. \n obs_list contains: ",paste(colnames(df),collapse=" "))
      if(length(tmp) == 0) return(list()) # If variable does not exist at all, return empty df
      var_names = tmp
    }
    ## Filter
    if (include) {
      keep=c("id","Date",intersect("Plant",names(df)))
      df=df[,c(keep,var_names)]
    } else {
      df[var_names]=NULL
      if (ncol(df)==2) {
        warning("All variables have been excluded from the list")
        return(NULL)
      }
    }
  }

  # Filter Dates
  ## check that dates are in obs_list
  if (!is.null(dates)) {
    included=sapply(dates, function(x) any(df$Date==x))
    if (!all(c=included)) {
      warning("Argument dates contains dates that are not included in obs_list: ",paste(dates[!included], collapse=" "))
      dates=dates[included]
    }
    ## Filter
    if (include) {
      df= dplyr::filter(df,.data$Date==dates)
    } else {
      df= dplyr::filter(df,.data$Date!=dates)
      if (nrow(df)==0) {
        warning("All dates have been excluded from the list.")
        return(NULL)
      }
    }
  }

  if("Plant" %in% colnames(df)){
    skeepcols = 3
  }else{
    skeepcols = 2
  }

  # Remove rows with only NAs:
  df = df[rowSums(is.na(df[,(skeepcols+1):ncol(df), drop = FALSE])) != (ncol(df) - skeepcols), ]

  if(!all(names(obs_list) %in% unique(df$id))){
    warning(
      "No observations found in USM(s) ",
      paste(names(obs_list)[!(names(obs_list) %in% unique(df$id))], collapse = ", ")
    )
  }

  # Re-transform the df into a list
  obs_list=split(df, df$id)

  # Remove column "id" and remove columns with only NAs:
  # utils::globalVariables("where")
  obs_list = lapply(
    obs_list,
    function(x){
      select(x, !.data$id & where(~!all(is.na(.x))))
    }
  )

  # Add warning when variable is remove from a situation:
  mapply(
    function(x,y){
      var_not_in_sit = var_names[!var_names %in% colnames(x)]
      if(length(var_not_in_sit) > 0){
        warning(
          "No observations found for variable(s) ",
          var_not_in_sit,
          " in USM ",
          y
        )
      }
      return()
    },
    obs_list, names(obs_list))
  return(obs_list)

}
