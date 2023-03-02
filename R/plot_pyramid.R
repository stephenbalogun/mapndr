#' Pyramid Plot of Age and Sex Data
#'
#' @param fill the variable name in the data to be used to plot the pyramid
#' @param age_group the variable name in the data that contains the different age-band
#' @param age_bands a vector of age-bands contained in the data to be used for age-categorization, if different from
#' c( "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
#' @param border logical (boolean). It determines if the individual bars should have a border or not
#' @param border_color if the `border` argument is set to `TRUE`, the user can supplied a border color using any of the standard color formats
#' @param sex the variable name that contains the sex (gender) in the supplied data. The `sex` should be in the "M" and "F" format as commonly seen on the NDR
#' @inheritParams map_lgas
#'
#' @return population pyramid plot
#' @export
#'
#' @examples
#'
#'
#' ## plot 2022 spectrum estimate of Zamfara by sex
#'
#' pop_data <- spectrum(year = 2022, state = "Zamfara") |>
#'   dplyr::mutate(
#'     age_group = forcats::fct_collapse(age_group, "65+" = c("65-69", "70-74", "75-79", "80+"))
#'   ) |>
#'   dplyr::group_by(state, sex, age_group) |>
#'   dplyr::summarise(
#'     estimate = sum(estimate, na.rm = TRUE),
#'     .groups = "drop"
#'   )
#'
#' plot_pyramid(pop_data, fill = estimate)
plot_pyramid <- function(
    .data,
    fill,
    age_group = age_group,
    sex = sex,
    age_bands = NULL,
    label = TRUE,
    cols = NULL,
    size = NULL,
    border = TRUE,
    border_color = NULL,
    interactive = FALSE) {
  my_cols <- c("F" = "#f5c1c1", "M" = "#c1f5f5")

  age_cat <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
  )

  data_age_group <- dplyr::distinct(.data, {{ age_group }}) |> dplyr::pull({{ age_group }})


  validate_pyramid(label, interactive, size, cols, border, border_color)


  if (!is.null(age_bands) && !all(age_bands %in% data_age_group)) {
    rlang::abort("The age_bands supplied is not the same as the unique entries in the data provided")
  }

  plot <- .data |>
    dplyr::mutate(
      {{ fill }} := ifelse({{ sex }} == "F", {{ fill }} * -1, {{ fill }}),
      {{ age_group }} := factor(
        {{ age_group }},
        levels = age_bands %||% age_cat
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        y = {{ age_group }},
        fill = {{ sex }}
      )
    )

  if (border) {
    plot <- plot +
      ggplot2::geom_bar(
        ggplot2::aes(x = {{ fill }}),
        stat = "identity",
        color = border_color %||% "#777777"
      )
  } else {
    plot <- plot +
      ggplot2::geom_bar(
        ggplot2::aes(x = {{ fill }}),
        stat = "identity"
      )
  }


  if (label) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = {{ fill }},
          label = abs({{ fill }})
        ),
        hjust = "outward",
        size = size %||% 3,
        check_overlap = TRUE
      )
  }


  if (interactive) {
    plotly::ggplotly(
      plot +
        ggplot2::theme_classic() +
        ggplot2::scale_x_continuous(
          labels = scales::label_dollar(prefix = "", style_negative = "parens")
        ) +
        ggplot2::scale_fill_manual(values = cols %||% my_cols)
    )
  } else {
    plot +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(
        labels = scales::label_dollar(prefix = "", style_negative = "parens")
      ) +
      ggplot2::scale_fill_manual(values = cols %||% my_cols)
  }
}
