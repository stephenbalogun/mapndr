#' Plot State-level Map with Two Bubbles
#'
#' @inheritParams map_lgas3
#' @inheritParams map_states
#'
#' @return a three-dimensional state-level map
#' @export
#'
#' @examples
#'
#' ## map 2022 spectrum estimate for "Ondo", "Oyo", "Osun" and "Ogun" states interactively
#'
#' state_data <- spectrum(state = c("Ondo", "Oyo", "Osun", "Ogun")) |>
#'   dplyr::count(state, wt = estimate, name = "estimate") |>
#'   dplyr::mutate(estimate_scaled = estimate * 0.8)
#'
#' map_states3(state_data, fill = estimate, light_bubble = estimate, dark_bubble = estimate_scaled)
#'
map_states3 <- function(
    .data,
    fill,
    light_bubble,
    dark_bubble,
    state = state,
    label_state = TRUE,
    label_fill = FALSE,
    label_dark_bubble = FALSE,
    label_state_color = NULL,
    label_fill_color = NULL,
    label_dark_bubble_color = NULL,
    fill_colors = NULL,
    light_bubble_color = NULL,
    dark_bubble_color = NULL,
    size_state = NULL,
    size_fill = NULL,
    size_dark_bubble = NULL,
    border_color = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  noise <- stats::runif(1, min = 0.05, max = 0.1)


  validate_state_maps(label_state, label_fill, size_state, size_fill, interactive)


  if (!is.null(fill_colors) && length(fill_colors) > 1 && length(fill_colors) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `fill_colors` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  df <- ndr_states(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(state == {{ state }}),
      multiple = "all"
    )

  lab_data <- df |>
    dplyr::group_by(.data$state) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric), mean
      ),
      {{ dark_bubble }} := dplyr::first({{ dark_bubble }}),
      {{ light_bubble }} := dplyr::first({{ light_bubble }}),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      long2 = .data$long + (2 * noise),
      lat2 = .data$lat + (2 * noise)
    )


  if (!is.numeric(fill_vec)) {
    lab_data <- df |>
      dplyr::distinct(.data$state, {{ fill }}) |>
      dplyr::left_join(
        lab_data,
        dplyr::join_by(state == {{ state }})
      )
  }


  if (!is.null(fill_colors) && length(fill_colors) == 1) {
    p <- df |>
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(.data$long, .data$lat, group = .data$group),
        fill = fill_colors,
        color = border_color %||% "black",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ light_bubble }}),
        color = light_bubble_color %||% "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ dark_bubble }}),
        color = dark_bubble_color %||% "#ae2234",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(0, 15))
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$group)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = border_color %||% "black",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ light_bubble }}),
        color = light_bubble_color %||% "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ dark_bubble }}),
        color = dark_bubble_color %||% "#ae2234",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(0, 15))
  }


  if (label_state) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2, .data$lat2, label = .data$state),
        size = size_state %||% 3,
        color = label_state_color %||% "black",
        check_overlap = TRUE
      )
  }

  if (label_dark_bubble) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat, label = scales::comma({{ dark_bubble }})
        ),
        size = size_dark_bubble %||% 2,
        color = label_dark_bubble_color %||% "#ffffff",
        check_overlap = TRUE
      )
  }

  if (label_fill) {

    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(
          .data$long2 + noise,
          .data$lat2 + noise,
          label = {{ fill }}
        ),
        size = size_fill %||% 3,
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
    p <- p + ggplot2::scale_fill_viridis_c(alpha = 0.5)
  }

  if (interactive) {
    plotly::ggplotly(p)
  } else {
    p
  }
}
