#' @title A wrapper for rgenoud package (single criterion)
#' @inheritParams optim_switch
#' @importFrom rgenoud genoud
#' @return A list containing:
#'   `final_values`: vector of estimated parameter values
#'   corresponding to the best solution.
#'   `init_values`: vector or matrix of initial parameter values.
#'   `est_values`: matrix of estimated values (final solution).
#'   `min_crit_value`: minimum value of the criterion.
#'   `trace_df`: data.frame containing parameters, criterion,
#'   iteration, evaluation and individual indices .
#'   `obs_var_list`: list of observed variables used by the criterion.
#'   `elapsed`: computation time.
#'   `genoud.pro`: an output file called located in the temporary
#'   directory provided by tempdir, containing solutions in each
#'   generation (criterion and parameter values )
#'
#' @keywords internal

wrap_rgenoud <- function(optim_options, param_info, crit_options) {
  if (nargs() == 1 && methods::hasArg(optim_options)) {
    init_TP <- if (!is.null(optim_options$pop.size))
      optim_options$pop.size
    else
      NA_integer_
    return(list(
      package = "rgenoud", family = "Global",
      method = "genoud", init_values_nb = init_TP
    ))
    }
  if (is.null(optim_options$pop.size)) {
    stop("optim_options$pop.size is mandatory, please define it (e.g., 10 * number of parameters)")
    }

  ranseed <- optim_options$ranseed
  optim_options$ranseed <- NULL

  param_names <- get_params_names(param_info)
  nb_params <- length(param_names)

  bounds <- get_params_bounds(param_info)
  init_values <- get_init_values(param_info)

  Domains <- cbind(bounds$lb, bounds$ub)

  # read final population from genoud.pro to construct est_value
  read_last_population_from_pro <- function(project_path, nb_params, param_names) {
    if (is.null(project_path) || !file.exists(project_path))
      return(NULL)
    L <- readLines(project_path, warn = FALSE)
    gen_idx <- grep("^Generation:\\s*[0-9]+", L)
    if (length(gen_idx) == 0)
      return(NULL)
    start <- gen_idx[length(gen_idx)]   # last generation
    block <- L[start:length(L)]
    rows <- grep("^\\s*[0-9]+\\s+", block, value = TRUE)
    if (length(rows) == 0)
      return(NULL)
    tok <- strsplit(trimws(rows), "\\s+")
    mat <- do.call(rbind, lapply(tok, function(x) as.numeric(x)))
    if (ncol(mat) < (2 + nb_params))  #(je vérifie si ce n est pas 3+nb_params)
      return(NULL)
    crit <- mat[,2]
    pars <- mat[, 3:(2+nb_params), drop = FALSE]  # ici aussi je dois vérifier!!
    colnames(pars) <- param_names
    list(pars = pars, crit = crit)
    }

  starting.values <- init_values
  if (is.vector(starting.values)) {
    starting.values <- matrix(starting.values, nrow = 1)
    }
  if (is.data.frame(starting.values)) {
    starting.values <- as.matrix(starting.values)
    }
  if (!is.null(ranseed)) {
    set.seed(ranseed)
    if (is.null(optim_options$unif.seed))
      optim_options$unif.seed <- sample.int(.Machine$integer.max, 1)
    if (is.null(optim_options$int.seed))
      optim_options$int.seed  <- sample.int(.Machine$integer.max, 1)
    }
  # project file
  if (is.null(optim_options$project.path)) {
    optim_options$project.path <- file.path(
      crit_options$out_dir,
      "genoud.pro"
    )
    }
  # tot_max_eval : genoud could do more (local search!!)
  crit_options$tot_max_eval <- optim_options$pop.size * optim_options$max.generations

  # trace env
  .trace_env <- new.env(parent = emptyenv())
  .trace_env$x_list <- list()
  .trace_env$crit_list <- list()
  .trace_env$k <- 0

  fn_gen <- function(x) {
    val <- main_crit(x, crit_options = crit_options)
    .trace_env$k <- .trace_env$k + 1
    .trace_env$x_list[[.trace_env$k]] <- as.numeric(x)
    .trace_env$crit_list[[.trace_env$k]] <- val
    val
    }

  start_time <- Sys.time()

  GENOUD <- tryCatch(
    do.call(
      rgenoud::genoud,
      c(
        list(
          fn = fn_gen,
          nvars = nb_params,
          max = FALSE,
          Domains = Domains,
          starting.values = starting.values
          ),
        optim_options
        )
      ),
    error = function(e) {
      warning(sprintf("genoud failed: %s", e$message))
      NULL
      }
    )

  elapsed <- Sys.time() - start_time

  if (is.null(GENOUD)) {
    return(NULL)
  }

  # final solution
  final_values <- as.numeric(GENOUD$par)
  names(final_values) <- param_names
  min_crit_value <- GENOUD$value

  # est_values : the best individual / final population en amont !!
  est_values <- matrix(final_values, nrow = 1, dimnames = list(NULL, param_names))
  # trace_df reconstruction from saved file
  trace_df <- NULL
  if (.trace_env$k > 0L) {
    X <- do.call(rbind, .trace_env$x_list)     # k x nb_params
    critv <- as.numeric(unlist(.trace_env$crit_list))
    eval <- seq_along(critv)
    popsize_used <- optim_options$pop.size


    # iter = batch (pas génération genoud ), ind = index dans le batch
    iter <- ((eval - 1L) %/% popsize_used) + 1L  #à ver
    ind  <- ((eval - 1L) %%  popsize_used) + 1L

    colnames(X) <- param_names
    trace_df <- data.frame(
      iter = as.integer(iter),
      ind  = as.integer(ind),
      as.data.frame(X),
      crit = critv,
      eval = as.integer(eval),
      rep = 1L,
      method = "rgenoud"
    )
    # est_values from genoud.pro (final gener)
    pro_pop <- read_last_population_from_pro(
      project_path = optim_options$project.path,
      nb_params = nb_params,
      param_names = param_names
      )
    if (!is.null(pro_pop)) {
      est_values <- as.matrix(pro_pop$pars)
      }
    }
  #obs_var_list
  info_final <- tryCatch(
    main_crit(
      param_values = final_values,
      crit_options = c(crit_options, return_detailed_info = TRUE)
    ),
    error = function(e) NULL
    )

  obs_var_list <- NULL
  if (!is.null(info_final)) {
    if (!is.null(info_final$obs_var_list))
      obs_var_list <- info_final$obs_var_list
    else if (!is.null(info_final$obs_intersect))
      obs_var_list <- names(info_final$obs_intersect)
    }
  # Stop info
  stop_reason <- "HARD MAXIMUM GENERATION LIMIT HIT"
  if (!is.null(GENOUD$hard.generation.limit) && isTRUE(GENOUD$hard.generation.limit)) {
    stop_reason <- "hard maximum generation limit hit"
    }
  if (!is.null(GENOUD$message) && nzchar(GENOUD$message)) {
    stop_reason <- GENOUD$message
    }
  generations_run <- NA_integer_
  if (!is.null(GENOUD$generations))
    generations_run <- as.integer(GENOUD$generations)

  res <- list(
    final_values = final_values,
    init_values = init_values,
    est_values = est_values,
    min_crit_value = min_crit_value,
    GENOUD = GENOUD,
    trace_df = trace_df,
    obs_var_list = obs_var_list,
    elapsed = elapsed,
    stop_reason = stop_reason,
    generations_run = generations_run,
    project_path = optim_options$project.path
  )

  return(res)
}
