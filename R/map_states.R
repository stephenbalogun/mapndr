#' Plot State-level Map
#'
#' @inheritParams map_lgas
#'
#' @return state-level map
#' @export
#'
#' @examples NULL
map_states <- function(
    .data,
    fill,
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

  p <- df |>
    ggplot2::ggplot(
      ggplot2::aes(long, lat, group = group)
    ) +
    ggplot2::geom_polygon(
      ggplot2::aes(fill = {{ fill }}),
      color = "black"
    ) +
    ggplot2::coord_map() +
    ggplot2::theme_void()

  if (label) {
    lab_data <- df |>
      dplyr::group_by(state) |>
      dplyr::summarise(
        dplyr::across(
          tidyselect::where(is.numeric), mean
        ),
        .groups = "drop"
      )

    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(long, lat, label = state),
        size = size %||% 3
      )
  }

  if (is.character(fill_vec) | is.factor(fill_vec)) {
    col_select <- my_cols(length(unique(fill_vec)))

    col_select <- stats::setNames(col_select, sort(unique(fill_vec)))

    p <- p +
      ggplot2::scale_fill_manual(
        values = cols %||% cols_select
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
