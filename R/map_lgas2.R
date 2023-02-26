#' Plot LGA-level Map with Bubbles
#'
#' @inheritParams map_lgas
#' @param bubble the variable name of the column to be used for the bubble (point) plot
#'
#' @return a two-dimensional LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data accompanying the package, filling by `prev_x` and bubbles by `incidence_x`
#'
#' map_lgas2(lga_data, fill = prev_x, bubble = incidence_x, label = TRUE)
#'
map_lgas2 <- function(
    .data,
    fill,
    bubble,
    state = state,
    lga = lga,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  noise <- stats::runif(1, min = 0.01, max = 0.02)

  df <- ndr_lgas(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(
        {{ state }} == {{ state }},
        {{ lga }} == {{ lga }}
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
    ) |>
    dplyr::mutate(
      long2 = .data$long + (2 * noise),
      lat2 = .data$lat + (2 * noise)
    )

  if (!is.null(cols) && length(cols) == 1) {
    p <- df |>
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga),
        fill = cols,
        color = "black",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        alpha = 0.4
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(5, 15))
  } else {
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
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        alpha = 0.4
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(5, 15))
  }


  if (label) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2, .data$lat2, label = .data$lga),
        size = size %||% 2,
        check_overlap = TRUE
      ) +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = {{ bubble }}),
        size = size %||% 2,
        color = "#ffffff",
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
