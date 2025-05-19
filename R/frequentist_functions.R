#' @title Summarizes results of frequentist methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by frequentist method wrappers
#'
#' @return Prints results of frequentist methods
#'
summary_frequentist <- function(optim_options, param_info, optim_results, out_dir) {
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  est_values <- optim_results$est_values
  ind_min_crit <- optim_results$ind_min_crit
  min_crit_value <- optim_results$min_crit_value

  cat(paste(
    "\nList of observed variables used:",
    paste(optim_results$obs_var_list, collapse = ", ")
  ))

  # Display of parameters values for the repetition which has the
  # smallest criterion
  for (ipar in 1:nb_params) {
    cat(paste(
      "\nEstimated value for", param_names[ipar], ": ",
      format(est_values[ind_min_crit, ipar],
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


#' @title Post-treat results of frequentist methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by frequentist method wrappers
#' @param crit_options List containing several arguments given to `estim_param`
#'  function: `param_names`, `obs_list`, `model_function`,
#'  `model_options`, `param_info`, `transform_obs`, `transform_sim`
#' that must be passed to main_crit function by the methods wrappers.
#'
#' @return Updated results of frequentist method
#'
post_treat_frequentist <- function(optim_options, param_info, optim_results,
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


#' @title Generate plots for frequentist methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by frequentist method wrappers
#'
#' @return Returns the list of plots + save them in a pdf file.
#'
#' @keywords internal
#'
plot_frequentist <- function(optim_options, param_info, optim_results, out_dir) {
  bounds <- get_params_bounds(param_info)
  init_values <- optim_results$init_values
  est_values <- optim_results$est_values
  crit_values <- optim_results$crit_values
  p_all <- list()

  # EstimatedVSinit plot

  tryCatch(
    {
      grDevices::pdf(
        file = file.path(out_dir, "EstimatedVSinit.pdf"),
        width = 9, height = 9
      )
    },
    error = function(cond) {
      filename <- paste0("EstimatedVSinit_new.pdf")
      warning(
        "Error trying to create ", out_dir,
        "/EstimatedVSinit.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
        filename
      )
      grDevices::pdf(
        file = file.path(out_dir, filename),
        width = 9, height = 9
      )
    }
  )

  tryCatch(
    {
      p <- plot_estimVSinit(
        init_values, est_values,
        crit_values, bounds$lb, bounds$ub
      )
    },
    error = function(cond) {
      warning(
        "Error trying to create EstimatedVSinit bubble graph file. \n
              Maybe linked with the values of the criterion to plot
              (size of the bubbles):",
        paste0(crit_values, collapse = ","),
        "\n Trying without the bubbles ..."
      )

      p <- plot_estimVSinit(init_values, est_values, crit_values,
        bounds$lb, bounds$ub,
        bubble = FALSE
      )
    }
  )

  for (plot in p) {
    print(plot)
  }
  grDevices::dev.off()
  p_all$estimVSinit <- p

  # ValuesVSit plot

  tryCatch(
    {
      grDevices::pdf(
        file = file.path(out_dir, "ValuesVSit.pdf"),
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

  if (!is.null(optim_results$params_and_crit)) {
    p <- plot_valuesVSit(optim_results$params_and_crit, param_info)
    for (plot in p) {
      print(plot)
    }
    grDevices::dev.off()
    p_all$valuesVSit <- p
  }

  # ValuesVSit_2D plot

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

  if (!is.null(optim_results$params_and_crit)) {
    p <- plot_valuesVSit_2D(optim_results$params_and_crit, param_info)
    for (plot in p) {
      print(plot)
    }
    grDevices::dev.off()
    p_all$valuesVSit_2D <- p
  }

  return(p_all)
}

#' @title Create plots of estimated versus initial values of the parameters
#'
#' @param init_values Data.frame containing initial values of the parameters
#' for each repetition
#' @param est_values Data.frame containing estimated values of the parameters
#' for each repetition
#' @param crit Vector containing the minimum value of the criterion for each
#' repetition of the minimization
#' @param lb Vector containing the lower bounds of the estimated parameters
#' @param ub Vector containing the upper bounds of the estimated parameters
#' @param bubble Logical indicating if bubbles of size proportional to the
#' minimum values of the criterion should be plot (TRUE, default value)
#' or not (FALSE).
#'
#' @return A named list containing one plot per parameter
#'
#' @details The number of the repetition that leads to the minimal value of the
#' criterion over all repetitions is written in white (if bubble is TRUE) or
#' in red (if bubble is false) while the other ones are written in black.
#'
#' @importFrom ggplot2 ggplot theme element_text geom_point geom_text
#' scale_size_binned scale_size labs xlim ylim
#' @importFrom dplyr filter
#'
#' @export
#'
plot_estimVSinit <- function(init_values, est_values, crit, lb, ub,
                             bubble = TRUE) {
  param_names <- colnames(init_values)
  nb_rep <- nrow(init_values)
  ind_min_crit <- which.min(crit)

  tmp <- rbind(lb, ub, est_values, init_values)
  tmp[tmp == Inf | tmp == -Inf] <- NA
  minvalue <- apply(tmp, 2, min, na.rm = TRUE)
  maxvalue <- apply(tmp, 2, max, na.rm = TRUE)
  minvalue <- minvalue - 0.05 * (maxvalue - minvalue)
  maxvalue <- maxvalue + 0.05 * (maxvalue - minvalue)

  p <- list()

  for (param_name in param_names) {
    df <- data.frame(
      init_values = init_values[, param_name],
      est_values = est_values[, param_name],
      crit = crit
    )
    row.names(df) <- paste0(seq(1:nb_rep))

    if (bubble) {
      tmp_aes <- aes(x = init_values, y = est_values, size = crit)
      color_best_rep <- "white"
    } else {
      tmp_aes <- aes(x = init_values, y = est_values)
      color_best_rep <- "red"
    }

    p[[param_name]] <- ggplot(df, tmp_aes) +
      labs(
        title = paste0(
          "Estimated vs Initial values of ", param_name,
          " \n for the different repetitions"
        ),
        y = paste("Estimated value for", param_name),
        x = paste("Initial value for", param_name),
        fill = "Criterion"
      ) +
      theme(plot.title = element_text(hjust = 0.5))

    if (bubble) {
      p[[param_name]] <- p[[param_name]] + geom_point(alpha = 0.5, color = "red")
    }

    p[[param_name]] <- p[[param_name]] +
      geom_text(
        label = rownames(df),
        nudge_x = 0, nudge_y = 0,
        check_overlap = T,
        show.legend = F,
        size = 4
      ) +
      geom_text(
        data = df[ind_min_crit, ],
        label = rownames(df[ind_min_crit, ]),
        nudge_x = 0, nudge_y = 0,
        check_overlap = T,
        show.legend = F,
        size = 4, color = color_best_rep
      ) +
      xlim(minvalue[param_name], maxvalue[param_name]) +
      ylim(minvalue[param_name], maxvalue[param_name])

    if (bubble) {
      if (length(unique(crit)) > 1) {
        p[[param_name]] <- p[[param_name]] + scale_size_binned(
          range = c(2, 20),
          name = "Final Value of \n minimized criteria"
        )
      } else {
        p[[param_name]] <- p[[param_name]] +
          scale_size(name = "Final Value of \n minimized criteria")
      }
    }
  }

  return(p)
}


#' @title Create plots of parameters and criterion values per iteration or
#' evaluation number
#'
#' @inheritParams estim_param
#' @param df Data.frame containing values of parameters (one column per
#' estimated parameter), criterion (crit column), repetition number (rep),
#' iteration number (iter) and evaluation number (eval)
#'  (similar to params_and_crit).
#' See Details section for comments about the difference between evaluations
#' and iterations.
#' @param iter_or_eval Values of the x axis: "iter" for iteration number,
#' "eval" for evaluation number
#' @param crit_log If TRUE, consider criterion values in log scale
#' @param rep_label Indicate if labels for the repetition number must be
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
plot_valuesVSit <- function(df, param_info, iter_or_eval = c("iter", "eval"),
                            crit_log = TRUE,
                            rep_label = c("begin_end", "begin", "end")) {
  param_names <- get_params_names(param_info)
  bounds <- get_params_bounds(param_info)

  lab <- "evaluations"
  if (iter_or_eval[1] == "iter") {
    df <- filter(df, !is.na(.data$iter))
    lab <- "iterations"
  }
  trans <- "identity"
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

  tmp <- rbind(bounds$lb, bounds$ub, select(df, all_of(param_names)))
  tmp[tmp == Inf | tmp == -Inf] <- NA
  minvalue <- apply(tmp, 2, min, na.rm = TRUE)
  maxvalue <- apply(tmp, 2, max, na.rm = TRUE)
  minvalue <- minvalue - 0.05 * (maxvalue - minvalue)
  maxvalue <- maxvalue + 0.05 * (maxvalue - minvalue)

  p <- list()

  for (param_name in param_names) {
    p[[param_name]] <- ggplot(df, aes_string(
      x = iter_or_eval[1], y = param_name,
      color = "crit"
    )) +
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
      geom_point(alpha = 0.5) +
      scale_color_gradient2(
        midpoint = mid, low = "blue", mid = "yellow",
        high = "red", space = "Lab", trans = trans
      )

    for (irep in unique(df$rep)) {
      p[[param_name]] <- p[[param_name]] +
        geom_line(data = filter(df, rep == irep))
      if (rep_label[1] == "begin_end" || rep_label[1] == "begin") {
        p[[param_name]] <- p[[param_name]] +
          geom_label(aes(label = rep),
            data = filter(df, rep == irep) %>% filter(eval == min(.data$eval)),
            size = 3
          )
      }
      if (rep_label[1] == "begin_end" || rep_label[1] == "end") {
        p[[param_name]] <- p[[param_name]] +
          geom_label(aes(label = rep),
            data = filter(df, rep == irep) %>% filter(eval == max(.data$eval)),
            size = 3
          )
      }
    }
    ylim(minvalue[param_name], maxvalue[param_name])
  }

  df$rep <- as.factor(df$rep)
  p[["criterion"]] <- ggplot(df, aes_string(
    x = iter_or_eval[1], y = "crit",
    color = "rep"
  )) +
    labs(
      title = paste0(
        "Evolution of the minimized criterion \n in function of the minimization ",
        lab
      ),
      y = "Minimized criterion",
      x = paste(lab, "number"),
      fill = "Repetition"
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(alpha = 0.5)

  for (irep in unique(df$rep)) {
    p[["criterion"]] <- p[["criterion"]] +
      geom_line(data = filter(df, rep == irep)) +
      geom_label(aes(label = rep),
        data = filter(df, rep == irep) %>% filter(eval == min(.data$eval)),
        size = 3
      ) +
      geom_label(aes(label = rep),
        data = filter(df, rep == irep) %>% filter(eval == max(.data$eval))
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
plot_valuesVSit_2D <- function(df, param_info, iter_or_eval = c("eval", "iter"),
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
  df$rep <- as.factor(df$rep)
  trans <- "identity"
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

  tmp <- rbind(bounds$lb, bounds$ub, select(df, all_of(param_names)))
  # -.data$ avoid NOTES on check ...
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

    if (fill[1] == "crit") {
      p[[ipair]] <- p[[ipair]] +
        scale_color_gradient2(
          midpoint = mid, low = "blue", mid = "yellow",
          high = "red", space = "Lab", trans = trans
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
    xlim(minvalue[df_pairs[1, ipair]], maxvalue[df_pairs[1, ipair]])
    ylim(minvalue[df_pairs[2, ipair]], maxvalue[df_pairs[2, ipair]])
  }

  return(p)
}
