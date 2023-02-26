#' Pyramid Plot of Age and Sex Data
#'
#' @param var the variable name in the data to be used to plot the pyramid
#' @param age_group the variable name in the data that contains the different age-band
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
#' plot_pyramid(pop_data, var = estimate)
plot_pyramid <- function(
    .data,
    var,
    age_group = age_group,
    sex = sex,
    label = TRUE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {

  if (!is.logical(label)) {
    rlang::abort("The label value is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!is.logical(interactive)) {
    rlang::abort("The interactive value supplied is not a logical vector. Did you forget to write the word in capital letters?")
  }

  if (!is.null(size) && !is.numeric(size)) {
    rlang::abort("`size` value must be in numbers")
  }

  my_cols <- c("F" = "#f5c1c1", "M" = "#c1f5f5")

  if (!is.null(cols) && length(cols) != length(my_cols)) {
    rlang::abort("The values supplied to `col` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  plot <- .data |>
    dplyr::mutate(
      {{ var }} := ifelse({{ sex }} == "F", {{ var }} * -1, {{ var }}),
      {{ age_group }} := factor(
        {{ age_group }},
        levels = c(
          "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
          "40-44", "45-49", "50-54", "55-59", "60-64", "65+"
        )
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        y = {{ age_group }},
        fill = {{ sex }}
      )
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(x = {{ var }}),
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
        ggplot2::aes(
          x = {{ var }},
          label = abs({{ var }})
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
