#' Plot State-level Map with Bubbles
#'
#' @inheritParams map_states
#' @inheritParams map_lgas2
#'
#' @return a two-dimensional state-level map
#' @export
#'
#' @examples
#'
#' ## map 2022 spectrum estimate for "Ondo", "Oyo", "Osun" and "Ogun" states interactively
#'
#' state_data <- spectrum(state = c("Ondo", "Oyo", "Osun", "Ogun")) |>
#'   dplyr::count(state, wt = estimate, name = "estimate")
#'
#' map_states2(state_data, fill = estimate, bubble = estimate)
#'
map_states2 <- function(
    .data,
    fill,
    bubble,
    state = state,
    label_state = TRUE,
    label_fill = FALSE,
    label_bubble = FALSE,
    label_state_color = NULL,
    label_fill_color = NULL,
    label_bubble_color = NULL,
    fill_colors = NULL,
    bubble_color = NULL,
    size_state = NULL,
    size_fill = NULL,
    size_bubble = NULL,
    border_color = NULL,
    border_width = NULL,
    gradient = NULL,
    grad_direction = NULL,
    na_fill = NULL,
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
      .groups = "drop"
    ) |>
    dplyr::mutate(
      long2 = .data$long + (2 * noise),
      lat2 = .data$lat + (2 * noise)
    )

  if (!is.numeric(fill_vec)) {
    lab_data <- df |>
      dplyr::distinct(.data$state, .data$state, {{ fill }}) |>
      dplyr::left_join(
        lab_data,
        dplyr::join_by(state)
      )
  }


  if (!is.null(fill_colors) && length(fill_colors) == 1) {
    p <- df |>
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(.data$long, .data$lat, group = .data$group),
        fill = fill_colors,
        color = border_color %||% "black",
        linewidth = border_width %||% 0.5
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        alpha = 0.5
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size_area(max_size = 10)
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$group)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = border_color %||% "black",
        linewidth = border_width %||% 0.5
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        color = bubble_color %||% "black",
        alpha = 0.5
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size_area(max_size = 10)
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


  if (label_bubble) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = {{ bubble }}),
        size = size_bubble %||% 3,
        color = label_bubble_color %||% "#ffffff",
        check_overlap = TRUE
      )
  }


  if (label_fill) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2 + noise, .data$lat2 + noise, label = ({{ fill }})),
        size = size_fill %||% 3,
        color = label_fill_color %||% "#ffffff",
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
    p <- p +
      ggplot2::scale_fill_viridis_c(
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
