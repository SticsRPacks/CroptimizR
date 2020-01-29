#' @title Extract parameter initial values from parameter information
#'
#' @inheritParams estim_param
#'
#' @return A dataframe containing initial values for the different parameters to
#' estimated (one column per parameter)
#'
#' @examples
#' # Simple cases
#' param_info=list(init_values=c(dlaimax=0.001, durvieF=200),
#'                        lb=c(dlaimax=0.0001, durvieF=50),
#'                        ub=c(dlaimax=0.01, durvieF=400))
#' CroptimizR:::get_params_init_values(param_info)
#'
#' param_info=list(init_values=data.frame(dlaimax=c(0.001,0.002), durvieF=c(50,200)),
#'                        lb=c(dlaimax=0.0001, durvieF=50),
#'                        ub=c(dlaimax=0.01, durvieF=400))
#' CroptimizR:::get_params_init_values(param_info)
#'
#' # Cases with groups of situations per parameter
#' param_info=list()
#' param_info$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'                                                "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
#'                                init_values=0.001,lb=0.0001,ub=0.1)
#' param_info$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'                                              c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
#'                                init_values=c(200,300),lb=50,ub=400)
#' CroptimizR:::get_params_init_values(param_info)
#'
#' param_info=list()
#' param_info$dlaimax=list(sit_list=list(c("bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'                                                "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+")),
#'                                init_values=c(0.001,0.002),lb=0.0001,ub=0.1)
#' param_info$durvieF=list(sit_list=list(c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'                                              c("bou99t3", "bou00t3", "bou99t1", "bou00t1")),
#'                                init_values=data.frame(c(200,300),c(250,350)),lb=50,ub=400)
#' CroptimizR:::get_params_init_values(param_info)
#'
#' @keywords internal
#'
get_params_init_values <- function(param_info) {

  init_values=NULL
  params_names=get_params_names(param_info)


  # Simple case, no simultaneous estimation of varietal and specific parameters
  if (!is.null(param_info$init_values)) {

    init_values=as.data.frame(param_info$init_values)

    # check if colnames were set to params_names, if not set them
    # and handle translation if necessary
    if (is.element(rownames(init_values)[1],params_names)) {
      init_values=as.data.frame(t(init_values))
      rownames(init_values)=1:nrow(init_values)
      init_values=init_values[,params_names]  # to ensure that params_names and
                                              # init_values have columns in same order
    } else if (!is.element(colnames(init_values)[1],params_names)) {
      if (ncol(init_values)!=length(params_names)) {
        init_values=t(init_values)
      }
      names(init_values)=params_names
      rownames(init_values)=1:nrow(init_values)
    } else {
      init_values=init_values[,params_names] # to ensure that params_names and
                                             # init_values have columns in same order
    }

  # Case of simultaneous estimation of varietal and specific parameters
  } else if (is.list(param_info[[1]])) {

    # check if colnames were set to params_names, if not set them
    # and handle translation if necessary
    for (i in 1:length(param_info)) {

      if (!is.null(param_info[[i]]$init_values)) {

        param_info[[i]]$init_values=
          as.data.frame(param_info[[i]]$init_values)

        if (ncol(param_info[[i]]$init_values) !=
            length(param_info[[i]]$sit_list)) {

          param_info[[i]]$init_values=t(param_info[[i]]$init_values)
          rownames(param_info[[i]]$init_values)=
            1:nrow(param_info[[i]]$init_values)

        }

      } else {

        param_info[[i]]$init_values=data.frame(NA)

      }

    }

    init_values=do.call(cbind, sapply(param_info, function(x) x$init_values))
    if (all(is.na(init_values))) {
      init_values=NULL
    } else {
      colnames(init_values)=params_names
    }

  }

  return(init_values)

}
