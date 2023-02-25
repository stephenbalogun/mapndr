#' Pyramid Plot of Age and Sex Data
#'
#' @param high_var the variable name in the data to be used to fill the bar (the lighter color)
#' @param low_var the variable name of the smaller values for the darker (shorter) fill
#' @inheritParams plot_pyramid
#'
#' @return population pyramid plot
#' @export
#'
#' @examples NULL
plot_pyramid2 <- function(
    .data,
    high_var,
    low_var,
    age_band = .data$age_group,
    sex = sex,
    label = TRUE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  my_cols <- c("F" = "#f5c1c1", "M" = "#c1f5f5")

  df <- .data |>
    dplyr::mutate(
      {{ high_var }} := ifelse({{ sex }} == "F", {{ high_var }} * -1, {{ high_var }}),
      {{ low_var }} := ifelse({{ sex }} == "F", {{ low_var }} * -1, {{ low_var }}),
      lab = round({{ low_var }} / {{ high_var }}, digits = 2),
      lab = ifelse({{ sex }} == "F", .data$lab * -1, .data$lab),
      {{ age_band }} := factor(
        {{ age_band }},
        levels = c(
          "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
          "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
        )
      )
    )

  plot <- df |>
    ggplot2::ggplot(
      ggplot2::aes(y = {{ age_band }}, fill = {{ sex }})
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(x = {{ high_var }}),
      stat = "identity",
      color = "#777777",
      alpha = 0.3
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(x = {{ low_var }}),
      stat = "identity",
      color = "#777777"
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_continuous(
      labels = scales::label_dollar(prefix = "", style_negative = "parens")
    ) +
    ggplot2::scale_fill_manual(values = cols %||% my_cols)

  if (label) {
    plot <- plot +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(
          x = {{ high_var }},
          label = scales::percent(abs(.data$lab))
        ),
        hjust = "outward",
        size = size %||% 3,
        check_overlap = TRUE
      )
  }

  if (interactive) {
    plotly::ggplotly(plot)
  } else {
    plot
  }
}
