#' Plot LGA-level Map
#'
#' @param .data a tabular (rectangular) data containing LGA and the variables to be plotted
#' @param fill the variable (categorical or continuous) to be used to fill (color) the LGAs
#' @param state the variable name of the column representing states in the data
#' @param lga the variable name of the column representing LGAs in the data
#' @param label_lga logical (boolean), indicating if the LGAs should be labelled or not
#' @param label_fill logical (boolean), indicating if the values for the `fill` data should be displayed
#' @param label_lga_color when supplied, this color will be used as font-color for the `lga` label
#' @param label_fill_color when supplied, this color will be used as font-color for the `fill` label
#' @param fill_colors a string of colors equal to the number of categories contained in the `fill` variable. When this is supplied, it overides the default colors and allow the user to choose specific colors
#' @param size_lga the font-size for the `lga` labels in whole numbers.
#' @param size_fill the font-size for the `fill` labels in whole numbers.
#' @param border_color the color to be used for the map boundaries. The default is black
#' @param border_width the thickness of the border line numbers
#' @param interactive logical (boolean), indicating whether the map should allow some level of interactivity
#' @param gradient when map is filled by a continuous variable, a gradient color can be supplied. The options are "A" to "H". The default is "E"
#' @param grad_direction, the gradient direction can be reversed by negating the current value. The default is `-1`. The reverse will take a value of `1`
#' @param na_fill the fill color to be used for locations with missing value. The default is `pink`
#'
#' @return LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data accompanying the package, filling by `prev_x`
#'
#' map_lgas(lga_data, fill = prev_x, label_lga = TRUE)
map_lgas <- function(
    .data,
    fill,
    state = state,
    lga = lga,
    label_lga = TRUE,
    label_fill = FALSE,
    label_lga_color = NULL,
    label_fill_color = NULL,
    fill_colors = NULL,
    size_lga = NULL,
    size_fill = NULL,
    border_color = NULL,
    border_width = NULL,
    gradient = NULL,
    grad_direction = NULL,
    na_fill = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  noise <- stats::runif(1, min = 0.01, max = 0.02)

  validate_lga_maps(label_lga, label_fill, size_lga, size_fill, interactive)

  if (!is.null(fill_colors) && length(fill_colors) > 1 && length(fill_colors) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `fill_colors` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  df <- ndr_lgas(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(
        state == {{ state }},
        lga == {{ lga }}
      ),
      multiple = "all"
    )

  lab_data <- df |>
    dplyr::group_by(.data$state, .data$lga) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric), mean
      ),
      .groups = "drop"
    )

  if (!is.numeric(fill_vec)) {
    lab_data <- df |>
      dplyr::distinct(.data$state, .data$lga, {{ fill }}) |>
      dplyr::left_join(
        lab_data,
        dplyr::join_by(
          state == {{ state }},
          lga == {{ lga }}
        )
      )
  }


  if (!is.null(fill_colors) && length(fill_colors) == 1) {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga)
      ) +
      ggplot2::geom_polygon(
        fill = fill_colors,
        color = border_color %||% "black",
        linewidth = border_width %||% 0.5,
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void()
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = border_color %||% "black",
        linewidth = border_width %||% 0.5
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void()
  }

  if (label_lga) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = .data$lga),
        size = size_lga %||% 2,
        color = label_lga_color %||% "black",
        check_overlap = TRUE
      )
  }

  if (label_fill) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long + 1.5 * noise, .data$lat + 1.5 * noise, label = {{ fill }}),
        size = size_fill %||% 2,
        color = label_fill_color %||% "black",
        check_overlap = TRUE
      )
  }

  if (is.character(fill_vec) | is.factor(fill_vec)) {
    col_select <- my_cols(length(unique(fill_vec)))

    col_select <- stats::setNames(col_select, sort(unique(fill_vec)))

    p <- p +
      ggplot2::scale_fill_manual(
        values = fill_colors %||% col_select
      )
  } else if (is.numeric(fill_vec)) {
    p <- p + ggplot2::scale_fill_viridis_c(
      alpha = 0.5,
      option = gradient %||% "E",
      na.value = na_fill %||% "pink",
      direction = grad_direction %||% -1
    )
  }

  if (interactive) {
    plotly::ggplotly(p)
  } else {
    p
  }
}
