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
#' @examples
#' # Example with fake data:
#' stats_per_steps <- data.frame(
#'   step = rep(c("Step1", "Step2"), each = 3),
#'   variable = rep(c("VarA", "VarB", "VarC"), 2),
#'   rRMSE = runif(6, 0, 0.5),
#'   EF = runif(6, 0, 1)
#' )
#' plot_stats_bars(stats_per_steps)
plot_stats_bars <- function(stats_per_steps) {
  # Convert wide to long for rRMSE and EF
  df_long <- stats_per_steps %>%
    pivot_longer(cols = c("rRMSE", "EF"), names_to = "statistic", values_to = "value")

  # Ensure variables are ordered by step
  df_long$variable <- factor(df_long$variable,
    levels = unique(df_long$variable[order(df_long$step)])
  )

  ggplot(df_long, aes(x = variable, y = value, fill = step)) +
    geom_col(position = position_dodge(width = 0.8)) +
    facet_wrap(~statistic, ncol = 1, scales = "free_y") +
    labs(x = "Variable", y = "Value", fill = "Step") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
