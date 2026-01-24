#' @title Plot Bias² and MSE evolution by calibration step as diagnostic of the AgMIP Protocol
#'
#' @description
#' Create diagnostic plots showing the evolution of Bias² and MSE statistics
#' across calibration steps for one or several variables. For each variable,
#' the curve is drawn with attenuated color before the calibration step where
#' it is first used, then in normal color from this step onwards. Panels are
#' ordered by the step of first use of each variable.
#'
#' @param stats_per_step A data.frame containing at least the following columns:
#'   - `step` (character or factor): calibration step
#'   - `variable` (character): name of the variable
#'   - `Bias2` (numeric): Bias² statistic
#'   - `MSE` (numeric): MSE statistic
#' @param steps_by_var A named character vector associating each variable
#'   (`names(steps_by_var)`) to the name of the step in which it is (last) used
#'   (`steps_by_var[variable]`).
#' @param step_levels (optional) Character vector giving the global order
#'   of steps. If not provided, the order of appearance in `stats_per_step$step`
#'   is used.
#'
#' @return A ggplot object with one facet per variable, ordered according to
#'   their step of use.
#'
#' @importFrom dplyr mutate select distinct arrange group_by summarise ungroup filter all_of
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_segment facet_wrap scale_alpha_manual labs theme_bw theme element_text
#'
plot_stats_evolution <- function(stats_per_step, steps_by_var, step_levels = NULL) {
  # Determine order of steps
  if (is.null(step_levels)) {
    step_levels <- unique(stats_per_step$step)
  }

  stats_per_step <- stats_per_step %>%
    mutate(step = factor(.data$step, levels = step_levels, ordered = TRUE))

  # Data long + pre/post phase
  df_long <- stats_per_step %>%
    tidyr::pivot_longer(
      cols = all_of(c("Bias2", "MSE")),
      names_to = "stat",
      values_to = "value"
    ) %>%
    mutate(
      variable_chr = as.character(.data$variable),
      step_use = steps_by_var[.data$variable_chr],
      step_use_id = match(.data$step_use, step_levels),
      step_id = as.integer(.data$step),
      phase = ifelse(!is.na(.data$step_use_id) & .data$step_id < .data$step_use_id, "pre", "post")
    )

  # Order variables according to the step of use
  order_vars <- df_long %>%
    distinct(.data$variable, .data$step_use_id) %>%
    arrange(.data$step_use_id)

  df_long <- df_long %>%
    mutate(variable = factor(.data$variable,
                             levels = order_vars$variable,
                             ordered = TRUE
    ))

  # Connection segments between the two lines (pre/post phase)
  segments <- df_long %>%
    group_by(.data$variable, .data$stat) %>%
    arrange(.data$step_id) %>%
    filter(any(.data$phase == "pre") & any(.data$phase == "post")) %>%
    summarise(
      xend = min(.data$step[.data$step_id >= .data$step_use_id]),
      yend = .data$value[.data$step == .data$xend][1],
      x = max(.data$step[.data$step_id < .data$step_use_id]),
      y = .data$value[.data$step == .data$x][1],
      .groups = "drop"
    ) %>%
    mutate(phase = "pre")

  # Plot
  ggplot(df_long, aes(
    x = .data$step, y = .data$value,
    color = .data$stat, shape = .data$stat, linetype = .data$stat
  )) +
    geom_line(aes(alpha = .data$phase, group = interaction(.data$stat, .data$phase))) +
    geom_point(aes(alpha = .data$phase), size = 2) +
    geom_segment(
      data = segments,
      aes(
        x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
        color = .data$stat, linetype = .data$stat
      ),
      alpha = 0.25, inherit.aes = FALSE
    ) +
    facet_wrap(~.data$variable, scales = "free_y") +
    scale_alpha_manual(values = c(pre = 0.25, post = 1), guide = "none") +
    labs(x = "step", y = "bias^2 or MSE") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
