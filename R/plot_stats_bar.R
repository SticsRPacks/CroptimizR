#' @title Plot bar charts of rRMSE and EF statistics by step and variable
#'
#' @description
#' Creates bar charts for the statistics rRMSE and EF across calibration steps,
#' with one panel per statistic. Variables are displayed on the x-axis and bars
#' are colored according to the step.
#'
#' @param stats_per_steps A data.frame containing at least the columns:
#'   - `step`: the step name,
#'   - `variable`: the variable name,
#'   - `rRMSE`: root Relative Mean Squared Error,
#'   - `EF`: Efficiency Factor.
#'
#' @return A ggplot object displaying the bar charts.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#'
plot_stats_bars <- function(stats_per_steps) {
  # Convert wide to long for rRMSE and EF
  df_long <- stats_per_steps %>%
    tidyr::pivot_longer(
      cols = c("rRMSE", "EF"),
      names_to = "statistic",
      values_to = "value"
    )

  # Conserver l'ordre des steps tel qu'il apparait dans le data.frame
  df_long$step <- factor(df_long$step, levels = unique(stats_per_steps$step))

  # Conserver l'ordre des variables tel qu'il apparait
  df_long$variable <- factor(df_long$variable, levels = unique(stats_per_steps$variable))

  ggplot(df_long, aes(x = .data$variable, y = .data$value, fill = .data$step)) +
    geom_col(position = position_dodge(width = 0.8)) +
    facet_wrap(~statistic, ncol = 1, scales = "free_y") +
    labs(x = "Variable", y = "Value", fill = "Step") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
