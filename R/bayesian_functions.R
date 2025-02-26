#' @title Summarizes results of bayesian methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by bayesian method wrappers
#'
#' @return Prints results of bayesian methods
#' @keywords internal
#'
summary_bayesian <- function(optim_options, param_info, optim_results) {
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  path_results <- optim_options$path_results
  out <- optim_results$out

  cat(paste(
    "\nList of observed variables used:",
    paste(optim_results$obs_var_list, collapse = ", "), "\n"
  ))

  ## Print results
  codaObject <- getSample(out, start = 1, coda = TRUE)
  tmp <- summary(codaObject)
  if (nb_params >= 2) {
    summary(out)
  } else {
    print(tmp)
  }
  cat(paste(
    "Complementary graphs and results can be found in ",
    path_results, "\n\n"
  ))
}



#' @title Generate plots for bayesian methods
#'
#' @inheritParams estim_param
#'
#' @param optim_results Results list returned by bayesian method wrappers
#'
#' @return Save plots in optim_options$path_results.
#'
#' @keywords internal
#'
plot_bayesian <- function(optim_options, param_info, optim_results) {
  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)
  path_results <- optim_options$path_results
  out <- optim_results$out
  nb_chains <- length(out$chain)
  nb_iterations <- nrow(optim_results$post_sample) / nb_chains

  tryCatch(
    {
      grDevices::pdf(
        file = file.path(path_results, "iterAndDensityPlots.pdf"),
        width = 9, height = 9
      )
      graphics::plot(out)
      grDevices::dev.off()
    },
    error = function(cond) {
      filename <- paste0(
        "iterAndDensityPlots",
        format(Sys.time(), "%Y_%d_%H_%M_%S"), ".pdf"
      )
      warning(
        "Error trying to create ",
        path_results,
        "/iterAndDensityPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
        filename
      )
      utils::flush.console()
      grDevices::pdf(
        file = file.path(path_results, filename),
        width = 9, height = 9
      )
      graphics::plot(out)
      grDevices::dev.off()
    }
  )

  tryCatch(
    {
      grDevices::pdf(
        file = file.path(path_results, "marginalPlots.pdf"),
        width = 9, height = 9
      )
      marginalPlot(out)
      grDevices::dev.off()
    },
    error = function(cond) {
      filename <- paste0(
        "marginalPlots",
        format(Sys.time(), "%Y_%d_%H_%M_%S"), ".pdf"
      )
      warning(
        "Error trying to create ", path_results,
        "/marginalPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
        filename
      )
      utils::flush.console()
      grDevices::pdf(
        file = file.path(path_results, filename),
        width = 9, height = 9
      )
      marginalPlot(out)
      grDevices::dev.off()
    }
  )

  if (nb_params >= 2) {
    tryCatch(
      {
        grDevices::pdf(
          file = file.path(path_results, "correlationPlots.pdf"),
          width = 9, height = 9
        )
        correlationPlot(out)
        grDevices::dev.off()
      },
      error = function(cond) {
        filename <- paste0(
          "correlationPlots",
          format(Sys.time(), "%Y_%d_%H_%M_%S"), ".pdf"
        )
        warning(
          "Error trying to create ", path_results,
          "/correlationPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
          filename
        )
        utils::flush.console()
        grDevices::pdf(
          file = file.path(path_results, filename),
          width = 9, height = 9
        )
        correlationPlot(out)
        grDevices::dev.off()
      }
    )
  }

  if (nb_params >= 2) {
    # seems that it does not work for a single parameter
    # also, Nbiteration must be > thin+50 otherwise coda::gelman.plot end with
    # an error
    if (is.null(optim_options$thin)) optim_options$thin <- 1
    if (nb_iterations >= (optim_options$thin + 50)) {
      tryCatch(
        {
          grDevices::pdf(
            file = file.path(path_results, "gelmanDiagPlots.pdf"),
            width = 9, height = 9
          )
          gelmanDiagnostics(out, thin = optim_options$thin, log = "y", plot = T)
          grDevices::dev.off()
        },
        error = function(cond) {
          filename <- paste0(
            "gelmanDiagPlots",
            format(Sys.time(), "%Y_%d_%H_%M_%S"), ".pdf"
          )
          warning(
            "Error trying to create ", path_results,
            "/gelmanDiagPlots.pdf file. It is maybe opened in a pdf viewer and locked. It will be created under the name ",
            filename
          )
          utils::flush.console()
          grDevices::pdf(
            file = file.path(path_results, filename),
            width = 9, height = 9
          )
          gelmanDiagnostics(out, thin = optim_options$thin, log = "y", plot = T)
          grDevices::dev.off()
        }
      )
    } else {
      gelmanDiagnostics(out, thin = optim_options$thin, plot = F)
      warning(paste0(
        "Number of iterations after burnin phase is too low (",
        nb_iterations,
        ") to generate gelmanDiagPlots.pdf (should be superior to thin+50)"
      ))
    }
  }
}
