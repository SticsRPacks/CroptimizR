#' @title Extract param names from parameter information
#'
#' @inheritParams estim_param
#'
#' @param short_list TRUE to return a list without replicated parameters which
#' happens for simultaneous estimation of specific and varietal parameters
#' (optional, default=FALSE)
#'
#' @return A vector of parameter names
#'
#' @examples
#' \donttest{
#' # A simple case
#' param_info <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::get_params_names(param_info)
#'
#' # A case with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   lb = 0.0005, ub = 0.0025
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   lb = c(50, 50), ub = c(400, 400)
#' )
#' CroptimizR:::get_params_names(param_info)
#' CroptimizR:::get_params_names(param_info, short_list = TRUE)
#' }
#'
#' @keywords internal
#'
get_params_names <- function(param_info, short_list = FALSE) {
  if (!is.null(param_info$lb) && !is.null(param_info$ub)) {
    return(names(param_info$lb))
  } else {
    if (short_list) {
      res <- names(param_info)
    } else {
      nb_groups <- sapply(param_info, function(x) length(x$sit_list))
      nb_groups[nb_groups == 0] <- 1
      nb_params_sl <- length(nb_groups)
      # build suffix
      suffix <- rep("", sum(nb_groups))
      count <- 1
      for (i in 1:nb_params_sl) {
        if (nb_groups[i] > 1) {
          suffix[count:(count + nb_groups[i] - 1)] <- as.character(1:nb_groups[i])
          count <- count + nb_groups[i]
        } else {
          count <- count + 1
        }
      }

      # The name of the parameter is replicated by its number of groups and a number is added
      res <- names(param_info)[unlist(sapply(1:length(nb_groups), function(x) rep(x, nb_groups[x])))]
      res <- paste0(res, suffix)
    }

    return(res)
  }
}


#' @title Extract bounds from parameter information
#'
#' @inheritParams estim_param
#'
#' @details For the moment only works with uniform distributions but will be
#' hopefully soon extended to any distribution
#'
#' @return A list containing the vectors of lower and upper bounds (`ub` and `lb`)
#'
#' @examples
#' \donttest{
#' # A simple case
#' param_info <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::get_params_bounds(param_info)
#'
#' # A case with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   lb = 0.0005, ub = 0.0025
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   lb = c(50, 100), ub = c(400, 500)
#' )
#' CroptimizR:::get_params_bounds(param_info)
#' }
#'
#' @keywords internal
#'
get_params_bounds <- function(param_info) {
  if (!is.null(param_info$lb) && !is.null(param_info$ub)) {
    lb <- param_info$lb
    ub <- param_info$ub
  } else {
    lb <- unlist(sapply(param_info, function(x) x$lb, simplify = FALSE))
    ub <- unlist(sapply(param_info, function(x) x$ub, simplify = FALSE))
  }

  return(list(lb = lb, ub = ub))
}


#' @title Filter param_info list for a subset of parameters
#'
#' @inheritParams estim_param
#'
#' @param param_names Names of the parameters to filter
#'
#' @return A list similar to the one given in input but filtered
#'
#' @examples
#' \donttest{
#' param_info <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400)
#' )
#' CroptimizR:::filter_param_info(param_info, "durvieF")
#'
#' # A case with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   lb = 0.0005, ub = 0.0025
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   lb = c(50, 100), ub = c(400, 500)
#' )
#' CroptimizR:::filter_param_info(param_info, "durvieF")
#' }
#'
#' @keywords internal
#'
filter_param_info <- function(param_info, param_names) {
  if (!is.null(param_info$lb) && !is.null(param_info$ub)) {
    if (!all(param_names %in% names(param_info$lb))) {
      stop(paste(
        "Error filtering param_info, parameters",
        paste(setdiff(param_names, names(param_info$lb)), collapse = ","),
        "not included in param_info."
      ))
    }
    param_info <- lapply(param_info, function(x) x[param_names])
  } else {
    if (!all(param_names %in% names(param_info))) {
      stop(paste(
        "Error filtering param_info, parameters",
        paste(setdiff(param_names, names(param_info)), collapse = ","),
        "not included in param_info."
      ))
    }
    param_info <- param_info[param_names]
  }

  return(param_info)
}


#' @title Set initial values in param_info
#'
#' @inheritParams estim_param
#'
#' @param init_values A data.frame containing the initial values to set in param_info
#' (one column per parameter)
#'
#' @return A list similar to param_info but with the new initial values
#'
#' @keywords internal
#'
set_init_values <- function(param_info, init_values) {
  param_names <- colnames(init_values)
  if (!is.null(param_info$lb) && !is.null(param_info$ub)) {
    if (!all(param_names %in% names(param_info$lb))) {
      stop(paste(
        "Error in param_info: parameters",
        paste(setdiff(param_names, names(param_info$lb)), collapse = ","),
        "not defined."
      ))
    }
    param_info$init_values <- init_values
  } else {
    lapply(
      names(param_info),
      function(x) {
        if (all(get_params_names(param_info[x]) %in% names(init_values))) {
          param_info[[x]]$init_values <<- init_values[, get_params_names(param_info[x])]
        } else {
          param_info[[x]]$init_values <<- NA
        }
      }
    )
  }

  return(param_info)
}


#' @title Extract parameter initial values from parameter information
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @examples
#' \donttest{
#' # Simple cases
#' param_info <- list(
#'   init_values = c(dlaimax = 0.001, durvieF = 200),
#'   lb = c(dlaimax = 0.0001, durvieF = 50),
#'   ub = c(dlaimax = 0.01, durvieF = 400)
#' )
#' CroptimizR:::get_params_init_values(param_info)
#'
#' param_info <- list(
#'   init_values = data.frame(dlaimax = c(0.001, 0.002), durvieF = c(50, 200)),
#'   lb = c(dlaimax = 0.0001, durvieF = 50),
#'   ub = c(dlaimax = 0.01, durvieF = 400)
#' )
#' CroptimizR:::get_params_init_values(param_info)
#' # ->
#' CroptimizR:::get_init_values(param_info)
#'
#' # Cases with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   init_values = 0.001, lb = 0.0001, ub = 0.1
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   init_values = c(200, 300), lb = 50, ub = 400
#' )
#' CroptimizR:::get_params_init_values(param_info)
#' # ->
#' CroptimizR:::get_init_values(param_info)
#'
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   init_values = c(0.001, 0.002), lb = 0.0001, ub = 0.1
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   init_values = data.frame(c(200, 300), c(250, 350)), lb = 50, ub = 400
#' )
#' CroptimizR:::get_params_init_values(param_info)
#' # ->
#' CroptimizR:::get_init_values(param_info)
#' }
#'
#' @keywords internal
#'
get_params_init_values <- function(param_info) {
  lifecycle::deprecate_warn("0.5.0", "get_params_init_values()", "get_init_values()")
  return(get_init_values(param_info))
}


#' @title Extract parameter initial values from parameter information
#'
#' @inheritParams estim_param
#'
#' @return A dataframe containing initial values for the different parameters to
#' estimated (one column per parameter)
#'
#' @examples
#' \donttest{
#' # Simple cases
#' param_info <- list(
#'   init_values = c(dlaimax = 0.001, durvieF = 200),
#'   lb = c(dlaimax = 0.0001, durvieF = 50),
#'   ub = c(dlaimax = 0.01, durvieF = 400)
#' )
#' CroptimizR:::get_init_values(param_info)
#'
#' param_info <- list(
#'   init_values = data.frame(dlaimax = c(0.001, 0.002), durvieF = c(50, 200)),
#'   lb = c(dlaimax = 0.0001, durvieF = 50),
#'   ub = c(dlaimax = 0.01, durvieF = 400)
#' )
#' CroptimizR:::get_init_values(param_info)
#'
#' # Cases with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   init_values = 0.001, lb = 0.0001, ub = 0.1
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   init_values = c(200, 300), lb = 50, ub = 400
#' )
#' CroptimizR:::get_init_values(param_info)
#'
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   init_values = c(0.001, 0.002), lb = 0.0001, ub = 0.1
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   init_values = data.frame(c(200, 300), c(250, 350)), lb = 50, ub = 400
#' )
#' CroptimizR:::get_init_values(param_info)
#' }
#'
#' @keywords internal
#'
get_init_values <- function(param_info) {
  init_values <- NULL
  params_names <- get_params_names(param_info)


  # Simple case, no simultaneous estimation of varietal and specific parameters
  if (!is.null(param_info$init_values)) {
    init_values <- as.data.frame(param_info$init_values)

    # check if colnames were set to params_names, if not set them
    # and handle translation if necessary
    if (is.element(rownames(init_values)[1], params_names)) {
      init_values <- as.data.frame(t(init_values))
      rownames(init_values) <- 1:nrow(init_values)
      init_values <- init_values[, params_names, drop = FALSE] # to ensure that params_names and
      # init_values have columns in same order
    } else if (!is.element(colnames(init_values)[1], params_names)) {
      if (ncol(init_values) != length(params_names)) {
        init_values <- t(init_values)
      }
      names(init_values) <- params_names
      rownames(init_values) <- 1:nrow(init_values)
    } else {
      init_values <- init_values[, params_names, drop = FALSE] # to ensure that params_names and
      # init_values have columns in same order
    }

    # Case of simultaneous estimation of varietal and specific parameters
  } else if (is.list(param_info[[1]])) {
    # check if colnames were set to params_names, if not set them
    # and handle translation if necessary
    for (i in 1:length(param_info)) {
      if (!is.null(param_info[[i]]$init_values)) {
        param_info[[i]]$init_values <-
          as.data.frame(param_info[[i]]$init_values)

        if (!is.null(param_info[[i]]$sit_list) && ncol(param_info[[i]]$init_values) !=
          length(param_info[[i]]$sit_list)) {
          param_info[[i]]$init_values <- t(param_info[[i]]$init_values)
        }
      } else {
        if (!is.null(param_info[[i]]$sit_list)) {
          param_info[[i]]$init_values <- data.frame(t(rep(NA, length(param_info[[i]]$sit_list))))
        } else {
          param_info[[i]]$init_values <- data.frame(NA)
        }
      }
    }

    init_values <- do.call(cbind, sapply(param_info, function(x) x$init_values))
    if (!is.data.frame(init_values)) init_values <- as.data.frame(init_values)
    if (all(is.na(init_values))) {
      init_values <- NULL
    } else {
      colnames(init_values) <- params_names
      rownames(init_values) <- 1:nrow(init_values)
    }
  }

  return(init_values)
}


#' @title Complete initial values in param_info by sampling values from bounds taking into account inequality constraints between parameters
#'
#' @inheritParams estim_param
#'
#' @param nb_values Total number of initial values required for the parameters
#'
#' @param ranseed Set random seed so that each execution give the same results.
#' If you want randomization, set it to NULL
#'
#' @return A list similar to param_info including nb_values initial values. If
#' param_info initially contains less than nb_values initial values, additional ones
#' are randomly sampled in the defined bounds taking into account the possible constraints
#' between the parameters.
#'
#' @keywords internal
#'
complete_init_values <- function(param_info, nb_values, ranseed = NULL,
                                 satisfy_par_const = NULL) {
  tmp <- get_params_bounds(param_info)
  lb <- tmp$lb
  ub <- tmp$ub
  init_values <- get_init_values(param_info)

  if (!is.null(lb) && !is.null(ub)) {
    param_names <- names(lb)
    sampled_values <- as.data.frame(sample_params(list(lb = lb, ub = ub), nb_values, ranseed))
    if (!is.null(satisfy_par_const)) {
      idx <- which(sapply(1:nrow(sampled_values), function(x) {
        satisfy_par_const(sampled_values[x, ])
      }))
      sampled_values <- sampled_values[idx, ]
      count <- 1
      # sample values until the number of required values satisfying the constraints are reached
      while (nrow(sampled_values) < nb_values && count < 1000) {
        seed <- sample(1000, 1, replace = FALSE)
        sampled_tmp <- as.data.frame(sample_params(list(lb = lb, ub = ub), nb_values,
          seed = seed
        ))
        idx <- which(sapply(1:nrow(sampled_tmp), function(x) {
          satisfy_par_const(sampled_tmp[x, ])
        }))
        sampled_values <- dplyr::bind_rows(sampled_values, sampled_tmp[idx, ])
        count <- count + 1
      }
      if (count >= 1000) {
        stop(paste(
          "Error, number of sampling of initial values reached maximum allowed value: ", count,
          "samplings have been performed but this did not allow to gather", nb_values, "satisfying the prescribed constraints on the parameters."
        ))
      }
      sampled_values <- sampled_values[1:nb_values, ]
    }
    for (param in param_names) {
      idx <- which(!is.na(init_values[, param]))
      if (length(idx) > 0) {
        sampled_values[idx[1:min(nb_values, length(idx))], param] <- init_values[idx[1:min(nb_values, length(idx))], param]
      }
    }
  } else {
    if (nrow(init_values) < nb_values || any(is.na(init_values))) {
      stop("Init_values must contain initial values for all parameters and repetitions if ub and lb are not provided.")
    }
    sampled_values <- init_values
  }

  return(set_init_values(param_info, sampled_values))
}


#' @title Extract default values from parameter information
#'
#' @inheritParams estim_param
#'
#' @return A named vector of default values
#'
#' @examples
#' \donttest{
#' # A simple case
#' param_info <- list(
#'   lb = c(dlaimax = 0.0005, durvieF = 50),
#'   ub = c(dlaimax = 0.0025, durvieF = 400),
#'   default = c(dlaimax = 0.001, durvieF = 200)
#' )
#' CroptimizR:::get_params_default(param_info)
#'
#' # A case with groups of situations per parameter
#' param_info <- list()
#' param_info$dlaimax <- list(
#'   sit_list = list(c(
#'     "bou99t3", "bou00t3", "bou99t1", "bou00t1",
#'     "bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"
#'   )),
#'   lb = 0.0005, ub = 0.0025, default = 0.001
#' )
#' param_info$durvieF <- list(
#'   sit_list = list(
#'     c("bo96iN+", "lu96iN+", "lu96iN6", "lu97iN+"),
#'     c("bou99t3", "bou00t3", "bou99t1", "bou00t1")
#'   ),
#'   lb = c(50, 100), ub = c(400, 500), default = c(200, 300)
#' )
#' CroptimizR:::get_params_default(param_info)
#' }
#'
#' @keywords internal
#'
get_params_default <- function(param_info) {
  if (!is.null(param_info$lb)) { # check the shape of param_info (list per parameter or per information)
    default <- param_info$default
  } else {
    default <- unlist(sapply(param_info, function(x) x$default, simplify = FALSE))
  }

  return(default)
}
