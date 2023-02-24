#' Plot State-level Map with Two Bubbles
#'
#' @inheritParams map_lgas3
#'
#' @return a three-dimensional state-level map
#' @export
#'
#' @examples NULL
map_states3 <- function(
    .data,
    fill,
    light_b,
    dark_b,
    st = state,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ st }}) |> dplyr::pull({{ st }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  df <- ndr_states(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(state == {{ st }}),
      multiple = "all"
    )

  lab_data <- df |>
    dplyr::group_by(state) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric), mean
      ),
      {{ dark_b }} := dplyr::first({{ dark_b }}),
      {{ light_b }} := dplyr::first({{ light_b }}),
      .groups = "drop"
    )

  noise <- stats::runif(1, max = 0.1)

  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(long, lat, group = group)
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = {{ fill }}),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = lab_data,
      ggplot2::aes((long + noise), (lat + noise), size = {{ light_b }}),
      color = "#f5c1c1",
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = lab_data,
      ggplot2::aes((long + noise), (lat + noise), size = {{ dark_b }}),
      color = "#ae2234",
      show.legend = FALSE
    ) +
    ggplot2::coord_map() +
    ggplot2::theme_void()


  if (label) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(long, lat, label = state),
        size = size %||% 3
      ) +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(long + (3 * noise), lat + (3 * noise), label = round({{ dark_b }})), size = size %||% 3
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
