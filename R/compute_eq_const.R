#' @title Compute equality constraints on model wrapper input parameters
#'
#' @inheritParams estim_param
#' @param param_values Named vector or tibble, value(s) of the estimated parameters
#' as provided by the parameter estimation algorithm (and possibly transformed in
#' main_crit function)
#'
#' @return Named vector or tibble (same shape as param_values) containing the values
#' for the model parameters to force.
#'
#' @keywords internal
#'
compute_eq_const <- function(forced_param_values, param_values) {

  comp_forced_values <- NULL
  if (!is.null(forced_param_values)) {

    param_values <- tibble::tibble(!!!param_values)
    param_values$situation <- NULL
    nrows <- max(1,seq_len(nrow(param_values)))
    comp_forced_values <- matrix(ncol = length(forced_param_values),
                                                nrow = nrows)
    colnames(comp_forced_values) <- names(forced_param_values)

    for (irow in 1:nrows) {

      expr_ls <-
        lapply(names(forced_param_values), function(x) paste(x,"<-",forced_param_values[[x]]))
      names(expr_ls) <- names(forced_param_values)

      for (par in names(param_values)) {
        eval(parse(text = paste(par,"<-",param_values[[irow, par]])))
      }
      for (par in names(forced_param_values)) {
        eval(parse(text = expr_ls[[par]]))
        eval(parse(text = paste0("comp_forced_values[irow,\"",par,"\"] <- ",par)))
      }

    }

    if (nrows==1) {
      comp_forced_values <- comp_forced_values[1,]
    } else {
      comp_forced_values <- tibble::as_tibble(comp_forced_values)
    }

  }

  return(comp_forced_values)

}

