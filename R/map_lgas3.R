#' Plot LGA-level Map with Two Bubbles
#'
#' @inheritParams map_lgas
#' @param light_b the variable name of the column to be used for the lighter (bigger) bubble
#' @param dark_b the variable name of the column to be used for the darker (smaller) bubble
#'
#' @return a three-dimensional LGA-level map
#' @export
#'
#' @examples NULL
map_lgas3 <- function(
    .data,
    fill,
    light_b,
    dark_b,
    st = .data$state,
    lga = lga,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ st }}) |> dplyr::pull({{ st }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  df <- ndr_lgas(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(
        .data$state == {{ st }},
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
      {{ dark_b }} := dplyr::first({{ dark_b }}),
      {{ light_b }} := dplyr::first({{ light_b }}),
      .groups = "drop"
    )

  noise <- stats::runif(1, max = 0.03)

  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(.data$long, .data$lat, group = .data$lga)
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = {{ fill }}),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = lab_data,
      ggplot2::aes((.data$long + noise), (.data$lat + noise), size = {{ light_b }}),
      color = "#f5c1c1",
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = lab_data,
      ggplot2::aes(
        (.data$long + noise), (.data$lat + noise),
        size = {{ dark_b }}
      ),
      color = "#ae2234",
      show.legend = FALSE
    ) +
    ggplot2::coord_map() +
    ggplot2::theme_void()


  if (label) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = .data$lga),
        size = size %||% 2
      ) +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(
          .data$long + (3 * noise), .data$lat + (3 * noise),
          label = round({{ dark_b }})
        ),
        size = size %||% 2,
        check_overlap = TRUE
      )
  }

  if (is.character(fill_vec) | is.factor(fill_vec)) {
    col_select <- my_cols(length(unique(fill_vec)))

    col_select <- stats::setNames(col_select, sort(unique(fill_vec)))

    p <- p +
      ggplot2::scale_fill_manual(
        values = cols %||% col_select
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
