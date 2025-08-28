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
#' @param steps A named character vector associating each variable
#'   (`names(steps)`) to the name of the step in which it is (last) used
#'   (`steps[variable]`).
#' @param step_levels (optional) Character vector giving the global order
#'   of steps. If not provided, the order of appearance in `stats_per_step$step`
#'   is used.
#'
#' @return A ggplot object with one facet per variable, ordered according to
#'   their step of use.
#'
#' @importFrom dplyr mutate select distinct arrange group_by summarise ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_segment facet_wrap scale_alpha_manual labs theme_bw theme element_text
#'
plot_stats_evolution <- function(stats_per_step, steps, step_levels = NULL) {
  # Determine order of steps
  if (is.null(step_levels)) {
    step_levels <- unique(stats_per_step$step)
  }

  stats_per_step <- stats_per_step %>%
    mutate(step = factor(step, levels = step_levels, ordered = TRUE))

  # Data long + pre/post phase
  df_long <- stats_per_step %>%
    pivot_longer(c(Bias2, MSE), names_to = "stat", values_to = "value") %>%
    mutate(
      variable_chr = as.character(variable),
      step_use = steps[variable_chr],
      step_use_id = match(step_use, step_levels),
      step_id = as.integer(step),
      phase = ifelse(!is.na(step_use_id) & step_id < step_use_id, "pre", "post")
    )

  # Order variables according to the step of use
  order_vars <- df_long %>%
    distinct(variable, step_use_id) %>%
    arrange(step_use_id)

  df_long <- df_long %>%
    mutate(variable = factor(variable,
      levels = order_vars$variable,
      ordered = TRUE
    ))

  # Connection segments between the two lines (pre/post phase)
  segments <- df_long %>%
    group_by(variable, stat) %>%
    arrange(step_id) %>%
    filter(any(phase == "pre") & any(phase == "post")) %>%
    summarise(
      xend = min(step[step_id >= step_use_id]),
      yend = value[step == xend][1],
      x = max(step[step_id < step_use_id]),
      y = value[step == x][1],
      .groups = "drop"
    ) %>%
    mutate(phase = "pre")

  # Plot
  ggplot(df_long, aes(
    x = step, y = value,
    color = stat, shape = stat, linetype = stat
  )) +
    geom_line(aes(alpha = phase, group = interaction(stat, phase))) +
    geom_point(aes(alpha = phase), size = 2) +
    geom_segment(
      data = segments,
      aes(
        x = x, y = y, xend = xend, yend = yend,
        color = stat, linetype = stat
      ),
      alpha = 0.25, inherit.aes = FALSE
    ) +
    facet_wrap(~variable, scales = "free_y") +
    scale_alpha_manual(values = c(pre = 0.25, post = 1), guide = "none") +
    labs(x = "step", y = "bias^2 or MSE") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
