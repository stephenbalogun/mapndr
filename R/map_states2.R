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
    label_bubble = TRUE,
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
    range = NULL,
    all_regions = FALSE,
    interactive = FALSE) {
  states <- dplyr::pull(.data, {{ state }}) |> unique()

  fill_vec <- dplyr::pull(.data, {{ fill }}) |> unique()


  noise <- stats::runif(1, min = 0.05, max = 0.1)

  validate_state_maps(label_state, label_fill, size_state, size_fill, all_regions, interactive)

  if (!is.null(fill_colors) && length(fill_colors) > 1 && length(fill_colors) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `fill_colors` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  if (all(unique(naijR::lgas_nigeria$state) %in% states) || all_regions) {
    df <- ndr_states() |>
      dplyr::left_join(
        .data,
        dplyr::join_by(state == {{ state }}),
        multiple = "all"
      )
  } else {
    df <- ndr_states(states) |>
      dplyr::left_join(
        .data,
        dplyr::join_by(state == {{ state }}, ),
        multiple = "all"
      )
  }


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
        color = border_color %||% border_grey(),
        linewidth = border_width %||% 0.3
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        color = bubble_color %||% border_grey()
      )
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$group)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = border_color %||% border_grey(),
        linewidth = border_width %||% 0.3
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        color = bubble_color %||% border_grey()
      )
  }

  if (label_state) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2, .data$lat2, label = .data$state),
        size = size_state %||% 3,
        color = label_state_color %||% label_grey(),
        check_overlap = TRUE
      )
  }


  if (label_bubble) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = ifelse(any({{ bubble }} < 1), {{ bubble }}, scales::comma({{ bubble }}))),
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
        color = label_fill_color %||% label_grey(),
        check_overlap = TRUE
      )
  }

  if (is.character(fill_vec) | is.factor(fill_vec)) {
    col_select <- my_cols(length(fill_vec))

    col_select <- stats::setNames(col_select, sort(fill_vec))

    p <- p +
      ggplot2::scale_fill_manual(
        values = fill_colors %||% col_select,
        na.value = na_fill %||% off_white()
      )
  } else if (is.numeric(fill_vec)) {
    p <- p +
      ggplot2::scale_fill_viridis_c(
        alpha = 0.5,
        option = gradient %||% "E",
        na.value = na_fill %||% off_white(),
        direction = grad_direction %||% -1,
        limits = range
      )
  }

  if (interactive) {
    plotly::ggplotly(
      p +
        ggplot2::coord_sf() +
        ggplot2::theme_void() +
        ggplot2::scale_size_area(max_size = 15, guide = "none")
    )
  } else {
    p +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size_area(max_size = 15, guide = "none")
  }
}
