#' Plot State-level Map with Bubbles
#'
#' @inheritParams map_lgas2
#'
#' @return a two-dimensional state-level map
#' @export
#'
#' @examples NULL
map_states2 <- function(
    .data,
    fill,
    bubble,
    st = .data$state,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ st }}) |> dplyr::pull({{ st }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  df <- ndr_states(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(.data$state == {{ st }}),
      multiple = "all"
    )

  lab_data <- df |>
    dplyr::group_by(.data$state) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric), mean
      ),
      .groups = "drop"
    )

  noise <- stats::runif(1, max = 0.1)

  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(.data$long, .data$lat, group = .data$group)
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = {{ fill }}),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = lab_data,
      ggplot2::aes((.data$long + noise), (.data$lat + noise), size = {{ bubble }}),
      alpha = 0.5
    ) +
    ggplot2::coord_map() +
    ggplot2::theme_void()


  if (label) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = .data$state),
        size = size %||% 3
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
