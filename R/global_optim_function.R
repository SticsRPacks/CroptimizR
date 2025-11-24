#' @title Summarizes results of global optim methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by global optim method wrappers
#'
#' @return Prints results of global optim methods
#'
summary_global_optim <- function(optim_options, param_info, optim_results, out_dir) {
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  final_values <- optim_results$final_values
  min_crit_value <- optim_results$min_crit_value
  est_values <- matrix(final_values, nrow = 1, dimnames = list(NULL, param_names))

  cat(paste(
    "\nList of observed variables used:",
    paste(optim_results$obs_var_list, collapse = ", ")
  ))
  # Display of parameters values for the candidate which has the
  # smallest criterion
  for (ipar in 1:nb_params) {
    cat(paste(
      "\nEstimated value for", param_names[ipar], ": ",
      format(final_values[[ipar]],
        scientific = FALSE,
        digits = 2, nsmall = 0
      )
    ))
  }
  cat(paste(
    "\nMinimum value of the criterion:",
    format(min_crit_value, scientific = FALSE, digits = 2, nsmall = 0)
  ))
  cat(paste(
    "\nComplementary graphs and results can be found in ", out_dir,
    "\n"
  ))
}

#' @title Post-treat results of global optim methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by global optim method wrappers
#' @param crit_options List containing several arguments given to `estim_param`
#'  function: `param_names`, `obs_list`, `model_function`,
#'  `model_options`, `param_info`, `transform_obs`, `transform_sim`
#' that must be passed to main_crit function by the methods wrappers.
#'
#' @return Updated results of global optim method
#'

post_treat_global_optim <- function(optim_options, param_info, optim_results,
                                    crit_options) {
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  info_crit_list <- crit_options$info_crit_list

  # Recompute final value of minimized criterion
  # (just to check it is correct and to get the observation list used)
  info_final <- main_crit(
    param_values = optim_results$final_values,
    crit_options = c(crit_options, return_detailed_info = TRUE)
  )
  if (info_final$crit != optim_results$min_crit_value) {
    stop(paste(
      "Internal error: incoherent computation of minimum criterion value. \nValue obtained in method wrapper:",
      optim_results$min_crit_value, "\nValue obtained afterwards:",
      info_final$crit
    ))
  }
  optim_results$forced_param_values <- info_final$forced_param_values
  sapply(info_crit_list, function(x) {
    final_info_crit <- x(
      obs_list = info_final$obs_intersect,
      crit = info_final$crit,
      param_nb = nb_params
    )
    optim_results[x()$name] <<- final_info_crit
  })
  return(optim_results)
}


#' @title Generate plots for global optim methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by global optim method wrappers
#'
#' @return Returns the list of plots + save them in a pdf file.
#'
#' @keywords internal
#'
plot_global_optim <- function(optim_options, param_info, optim_results, out_dir) {
  est_values <- optim_results$est_values
  trace_df <- optim_results$trace_df
  p_all <- list()

  # ValuesVSit

  if (!is.null(trace_df)) {
    tryCatch(
      {
        grDevices::pdf(
          file = file.path(out_dir, "ValuesVSit_globaloptim.pdf"),
          width = 9, height = 9
        )
      },
      error = function(cond) {
        filename <- paste0("ValuesVSit_new.pdf")
        warning(
          "Error trying to create ", out_dir,
          "/ValuesVSit.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
          filename
        )
        grDevices::pdf(
          file = file.path(out_dir, filename),
          width = 9, height = 9
        )
      }
    )

    # trace_df : params, ind, iter, crit, eval, rep, method
    p <- plot_valuesVSit_go(
      df = trace_df,
      param_info = param_info,
      iter_or_eval = "iter",
      crit_log = FALSE,
      ind_label = "begin"
    )
    for (plot in p) print(plot)
    grDevices::dev.off()
    p_all$valuesVSit <- p
  }


  #  ValuesVSit_2D

  if (!is.null(trace_df)) {
    df_2d <- trace_df

    tryCatch(
      {
        grDevices::pdf(
          file = file.path(out_dir, "ValuesVSit_2D.pdf"),
          width = 9, height = 9
        )
      },
      error = function(cond) {
        filename <- paste0("ValuesVSit_2D_new.pdf")
        warning(
          "Error trying to create ", out_dir,
          "/ValuesVSit_2D.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
          filename
        )
        grDevices::pdf(
          file = file.path(out_dir, filename),
          width = 9, height = 9
        )
      }
    )

    p <- plot_valuesVSit_2D_go(
      df = df_2d,
      param_info = param_info,
      iter_or_eval = "eval",
      fill = "eval",
      crit_log = FALSE,
      lines = FALSE,
      rep_label = "end"
    )

    for (plot in p) print(plot)
    grDevices::dev.off()
    p_all$valuesVSit_2D <- p
  }

  return(p_all)
}


#' @title Create plots of parameters and criterion values per iteration or
#' evaluation number
#'
#' @inheritParams estim_param
#' @param df Data.frame containing values of each individual and iteration parameters values (one column per
#' estimated parameter), criterion (crit column), individual index (ind),
#' iteration number (iter) and evaluation number (eval)
#'  (similar to params_and_crit).
#' See Details section for comments about the difference between evaluations
#' and iterations.
#' @param iter_or_eval Values of the x axis: "iter" for iteration number,
#' "eval" for evaluation number
#' @param crit_log If TRUE, consider criterion values in log scale
#' @param ind_label Indicate if labels for the individual number must be
#' plotted at both beginning and end of lines ("begin_end"),
#' only at the beginning ("begin") or only at the end ("end")
#'
#' @return A named list containing one plot per parameter and a plot for the
#' criterion.
#'
#' @details Evaluation means evaluation of the criterion from proposed values of
#' the parameters by the parameter estimation algorithm.
#' An iteration is reached when an evaluation lead to a better value of the
#' criterion than the previously obtained values.
#' There are thus more evaluations than iterations. The criterion decreases when
#' iteration number increases while it is not the case when evaluation number
#' increases.
#'
#' @importFrom ggplot2 ggplot aes_string theme element_text geom_point
#' scale_color_gradient2 geom_line geom_label aes labs scale_y_log10
#' @importFrom dplyr select filter %>%
#'
#' @export
#'

plot_valuesVSit_go <- function(df, param_info, iter_or_eval = c("iter", "eval"),
                               crit_log = TRUE,
                               ind_label = c("begin_end", "begin", "end")) {
  param_names <- get_params_names(param_info)
  bounds <- get_params_bounds(param_info)

  lab <- "evaluations"
  if (iter_or_eval[1] == "iter") {
    df <- filter(df, !is.na(.data$iter))
    lab <- "iterations"
  }
  have_crit <- "crit" %in% names(df) && !all(is.na(df$crit))
  trans <- "identity"
  mid <- NA_real_
  if (have_crit) {
    mid <- (max(df$crit) - min(df$crit)) / 2 + min(df$crit)
    if (crit_log) {
      if (all(df$crit > 0)) {
        trans <- "log10"
        mid <- (max(log10(df$crit)) -
          +min(log10(df$crit))) / 2 + min(log10(df$crit))
      } else {
        warning("The criterion takes negative values, log transformation will not be done.")
        crit_log <- FALSE
      }
    }
  } else {
    crit_log <- FALSE
  }
  tmp <- rbind(bounds$lb, bounds$ub, select(df, all_of(param_names)))
  tmp[tmp == Inf | tmp == -Inf] <- NA
  minvalue <- apply(tmp, 2, min, na.rm = TRUE)
  maxvalue <- apply(tmp, 2, max, na.rm = TRUE)
  minvalue <- minvalue - 0.05 * (maxvalue - minvalue)
  maxvalue <- maxvalue + 0.05 * (maxvalue - minvalue)

  p <- list()

  for (param_name in param_names) {
    p[[param_name]] <- ggplot(
      df, aes(
        x = !!rlang::sym(iter_or_eval[1]),
        y = !!rlang::sym(param_name),
        color = crit
      )
    ) +
      labs(
        title = paste0(
          "Evolution of ", param_name,
          " \n in function of the minimization ", lab
        ),
        y = param_name,
        x = paste(lab, "number"),
        fill = "Criterion"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_point(alpha = 0.5)

    if (have_crit) {
      p[[param_name]] <- p[[param_name]] +
        scale_color_gradient2(
          midpoint = mid, low = "blue", mid = "yellow",
          high = "red", space = "Lab", trans = trans
        )
    }
    for (iind in unique(df$ind)) {
      p[[param_name]] <- p[[param_name]] +
        geom_line(data = filter(df, ind == iind))
      if (ind_label[1] == "begin_end" || ind_label[1] == "begin") {
        p[[param_name]] <- p[[param_name]] +
          geom_label(aes(label = ind),
            data = filter(df, ind == iind) %>% filter(eval == min(.data$eval)),
            size = 3
          )
      }
      if (ind_label[1] == "begin_end" || ind_label[1] == "end") {
        p[[param_name]] <- p[[param_name]] +
          geom_label(aes(label = ind),
            data = filter(df, ind == iind) %>% filter(eval == max(.data$eval)),
            size = 3
          )
      }
    }
    p[[param_name]] <- p[[param_name]] +
      ylim(minvalue[param_name], maxvalue[param_name])
  }

  df$ind <- as.factor(df$ind)
  p[["criterion"]] <- ggplot(df, aes_string(
    x = iter_or_eval[1], y = "crit",
    color = "ind"
  )) +
    labs(
      title = paste0(
        "Evolution of the minimized criterion \n in function of the minimization ",
        lab
      ),
      y = "Minimized criterion",
      x = paste(lab, "number"),
      fill = "Individual"
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(alpha = 0.5)

  for (iind in unique(df$ind)) {
    p[["criterion"]] <- p[["criterion"]] +
      geom_line(data = filter(df, ind == iind)) +
      geom_label(aes(label = ind),
        data = filter(df, ind == iind) %>% filter(eval == min(.data$eval)),
        size = 3
      ) +
      geom_label(aes(label = ind),
        data = filter(df, ind == iind) %>% filter(eval == max(.data$eval))
      )
  }

  if (crit_log) {
    p[["criterion"]] <- p[["criterion"]] + scale_y_log10()
  }

  return(p)
}


#' @title Create 2D plots of parameters values evolution per iteration or
#' evaluation number
#'
#' @inheritParams estim_param
#' @param df Data.frame containing values of parameters (one column per
#' estimated parameter), criterion (crit column), repetition number (rep),
#' iteration number (iter) and evaluation number (eval)
#'  (similar to params_and_crit).
#' See Details section for comments about the difference between evaluations
#' and iterations.
#' @param iter_or_eval "iter" for plotting the values for each iteration,
#' "eval" for plotting the values for each evaluation
#' @param fill If "crit", colours the points and lines in function of the
#' minimized criterion value, if "rep" colours in function of the
#' repetition number.
#' @param crit_log If TRUE, consider criterion values in log scale
#' @param lines If TRUE add lines between points of a same repetition
#' @param rep_label Indicate if labels for the repetition number must be plotted
#' at both beginning and end of lines ("begin_end"), only at the beginning
#' ("begin") or only at the end ("end")
#'
#' @return A list containing one plot per parameter pair.
#'
#' @details Evaluation means evaluation of the criterion from proposed values of
#' the parameters by the parameter estimation algorithm.
#' An iteration is reached when an evaluation lead to a better value of the
#' criterion than the previously obtained values.
#' There are thus more evaluations than iterations. The criterion decreases when
#' iteration number increases while it is not the case when evaluation number
#' increases.
#'
#' @importFrom ggplot2 ggplot aes_string theme element_text geom_point labs
#' xlim ylim geom_path scale_y_log10
#' @importFrom dplyr select filter %>%
#'
#' @export
#'
plot_valuesVSit_2D_go <- function(df, param_info, iter_or_eval = c("eval", "iter"),
                                  fill = c("crit", "rep"), crit_log = TRUE,
                                  lines = FALSE,
                                  rep_label = c("begin_end", "begin", "end")) {
  param_names <- get_params_names(param_info)
  if (length(param_names) <= 1) {
    return()
  }
  bounds <- get_params_bounds(param_info)

  lab <- "evaluations"
  if (iter_or_eval[1] == "iter") {
    df <- filter(df, !is.na(.data$iter))
    lab <- "iterations"
  }
  if ("rep" %in% names(df)) {
    df$rep <- as.factor(df$rep)
  }
  has_crit <- "crit" %in% names(df)
  trans <- "identity"
  mid <- NA_real_
  if (fill[1] == "crit" && has_crit) {
    mid <- (max(df$crit) - min(df$crit)) / 2 + min(df$crit)
    if (crit_log) {
      if (all(df$crit > 0)) {
        trans <- "log10"
        mid <- (max(log10(df$crit)) -
          min(log10(df$crit))) / 2 + min(log10(df$crit))
      } else {
        warning("The criterion takes negative values, log transformation will not be done.")
        crit_log <- FALSE
      }
    }
  } else {
    crit_log <- FALSE
  }
  tmp <- rbind(bounds$lb, bounds$ub, select(df, all_of(param_names)))
  # check ...
  tmp[tmp == Inf | tmp == -Inf] <- NA
  minvalue <- apply(tmp, 2, min, na.rm = TRUE)
  maxvalue <- apply(tmp, 2, max, na.rm = TRUE)
  minvalue <- minvalue - 0.05 * (maxvalue - minvalue)
  maxvalue <- maxvalue + 0.05 * (maxvalue - minvalue)

  p <- list()

  df_pairs <- utils::combn(param_names, 2)

  for (ipair in seq_len(ncol(df_pairs))) {
    p[[ipair]] <- ggplot(df, aes_string(
      x = df_pairs[1, ipair],
      y = df_pairs[2, ipair], color = fill[1]
    )) +
      labs(
        title = paste0(
          "Evolution of ", df_pairs[1, ipair], " and ",
          df_pairs[2, ipair], " \n in function of the minimization ",
          lab
        ),
        y = paste("Estimated value for", df_pairs[2, ipair]),
        x = paste("Estimated value for", df_pairs[1, ipair]),
        fill = "Criterion"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_point(alpha = 0.5)

    if (fill[1] == "crit" && has_crit) {
      p[[ipair]] <- p[[ipair]] +
        scale_color_gradient2(
          midpoint = mid, low = "blue", mid = "yellow",
          high = "red", space = "Lab", trans = trans
        )
    } else if (fill[1] == "eval") {
      p[[ipair]] <- p[[ipair]] +
        scale_color_gradient2(
          low  = "lightblue",
          high = "darkblue"
        )
    }

    if (lines) {
      for (irep in unique(df$rep)) {
        p[[ipair]] <- p[[ipair]] +
          geom_path(data = filter(df, rep == irep))
        if (rep_label[1] == "begin_end" || rep_label[1] == "begin") {
          p[[ipair]] <- p[[ipair]] +
            geom_label(aes(label = rep),
              data = filter(df, rep == irep) %>% filter(eval == min(.data$eval)),
              size = 3
            )
        }
        if (rep_label[1] == "begin_end" || rep_label[1] == "end") {
          p[[ipair]] <- p[[ipair]] +
            geom_label(aes(label = rep),
              data = filter(df, rep == irep) %>% filter(eval == max(.data$eval)),
              size = 3
            )
        }
      }
    }
    p[[ipair]] <- p[[ipair]] +
      xlim(minvalue[df_pairs[1, ipair]], maxvalue[df_pairs[1, ipair]]) +
      ylim(minvalue[df_pairs[2, ipair]], maxvalue[df_pairs[2, ipair]])
  }

  return(p)
}
