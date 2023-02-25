#' Plot LGA-level Map
#'
#' @param .data a tabular (rectangular) data containing LGA and the variables to be plotted
#' @param fill the variable (categorical or continuous) to be used to fill (color) the LGAs
#' @param state the variable name of the column representing states in the data
#' @param lga the variable name of the column representing LGAs in the data
#' @param label logical (boolean), indicating if the LGAs should be labelled or not
#' @param cols a string of colors equal to the number of categories contained in the `fill` variable. When this is supplied, it overides the default colors and allow the user to choose specific colors
#' @param size the text size of the labels in whole numbers. The default is 2
#' @param interactive logical (boolean), indicating whether the map should allow some level of interactivity
#'
#' @return LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data accompanying the package, filling by `prev_x`
#'
#' map_lgas(lga_data, fill = prev_x, label = TRUE)
map_lgas <- function(
    .data,
    fill,
    state = state,
    lga = lga,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  df <- ndr_lgas(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(
        {{ state }} == {{ state }},
        {{ lga }} == {{ lga }}
      ),
      multiple = "all"
    )

  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(.data$long, .data$lat, group = .data$lga)
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = {{ fill }}),
      color = "black"
    ) +
    ggplot2::coord_map() +
    ggplot2::theme_void()

  if (label) {
    lab_data <- df |>
      dplyr::group_by(.data$state, .data$lga) |>
      dplyr::summarise(
        dplyr::across(
          tidyselect::where(is.numeric), mean
        ),
        .groups = "drop"
      )

    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = .data$lga),
        size = size %||% 2,
        check_overlap = TRUE
      )
  }

  if (is.character(fill_vec) | is.factor(fill_vec)) {
    col_select <- my_cols(length(unique(fill_vec)))

    col_select <- stats::setNames(col_select, sort(unique(fill_vec)))

    p <- p +
      ggplot2::scale_fill_manual(
        values = col_select
      )
  } else if (is.numeric(fill_vec)) {
    p <- p + ggplot2::scale_fill_viridis_c(alpha = 0.5)
  }

  if (interactive) {
    plotly::ggplotly(p)
  } else {
    p
  }
}
