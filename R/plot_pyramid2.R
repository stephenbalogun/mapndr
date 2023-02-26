#' Pyramid Plot of Age and Sex Data
#'
#' @param light_fill the variable name in the data to be used to fill the bar (the lighter color)
#' @param dark_fill the variable name of the smaller values for the darker (shorter) fill
#' @inheritParams plot_pyramid
#'
#' @return population pyramid plot
#' @export
#'
#' @examples
#'
#' ## plot 2022 spectrum estimate of Zamfara by sex and `incidence_x`
#'
#' pop_data <- spectrum(year = 2022, state = "Zamfara") |>
#'   dplyr::mutate(
#'     age_group = forcats::fct_collapse(age_group, "65+" = c("65-69", "70-74", "75-79", "80+"))
#'   ) |>
#'   dplyr::group_by(state, sex, age_group) |>
#'   dplyr::summarise(
#'     estimate = sum(estimate, na.rm = TRUE),
#'     .groups = "drop"
#'   ) |>
#'   dplyr::mutate(
#'     incidence_x = round(estimate * runif(1, max = 0.3))
#'   )
#'
#' plot_pyramid2(pop_data, light_fill = estimate, dark_fill = incidence_x)
#'
plot_pyramid2 <- function(
    .data,
    light_fill,
    dark_fill,
    age_group = age_group,
    sex = sex,
    label = TRUE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {

  my_cols <- c("F" = "#f5c1c1", "M" = "#c1f5f5")

  validate_pyramid(label, interactive, size, cols)


  df <- .data |>
    dplyr::mutate(
      {{ light_fill }} := ifelse({{ sex }} == "F", {{ light_fill }} * -1, {{ light_fill }}),
      {{ dark_fill }} := ifelse({{ sex }} == "F", {{ dark_fill }} * -1, {{ dark_fill }}),
      lab = round({{ dark_fill }} / {{ light_fill }}, digits = 2),
      lab = ifelse({{ sex }} == "F", .data$lab * -1, .data$lab),
      {{ age_group }} := factor(
        {{ age_group }},
        levels = c(
          "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
          "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
        )
      )
    )

  plot <- df |>
    ggplot2::ggplot(
      ggplot2::aes(y = {{ age_group }}, fill = {{ sex }})
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(x = {{ light_fill }}),
      stat = "identity",
      color = "#777777",
      alpha = 0.3
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(x = {{ dark_fill }}),
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
          x = {{ light_fill }},
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
