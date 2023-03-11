#' Plot State-level Map
#'
#' @inheritParams map_lgas
#' @param label_state logical (boolean), indicating if the state should be labelled or not
#' @param label_state_color when supplied, this color will be used as font-color for the `state` label
#' @param size_state the font-size for the `state` labels in whole numbers.
#'
#' @return state-level map
#' @export
#'
#' @examples
#'
#' ## map 2022 spectrum estimate for "Ondo", "Oyo", "Osun" and "Ogun" states interactively
#'
#' state_data <- spectrum(state = c("Ondo", "Oyo", "Osun", "Ogun")) |>
#'   dplyr::count(state, wt = estimate, name = "estimate")
#'
#' map_states(state_data, fill = estimate, interactive = TRUE)
map_states <- function(
    .data,
    fill,
    state = state,
    label_state = TRUE,
    label_fill = FALSE,
    label_state_color = NULL,
    label_fill_color = NULL,
    fill_colors = NULL,
    size_state = NULL,
    size_fill = NULL,
    border_color = NULL,
    border_width = NULL,
    gradient = NULL,
    grad_direction = NULL,
    na_fill = NULL,
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
    )

  if (!is.numeric(fill_vec)) {
    lab_data <- df |>
      dplyr::distinct(.data$state, {{ fill }}) |>
      dplyr::left_join(
        lab_data,
        dplyr::join_by(state)
      )
  }

  if (!is.null(fill_colors) && length(fill_colors) == 1) {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$group)
      ) +
      ggplot2::geom_polygon(
        fill = fill_colors,
        color = border_color %||% border_grey(),
        linewidth = border_width %||% 0.3
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
      )
  }

  if (label_state) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = .data$state),
        size = size_state %||% 3,
        color = label_state_color %||% label_grey(),
        check_overlap = TRUE
      )
  }

  if (label_fill) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long + 1.5 * noise, .data$lat + 1.5 * noise, label = {{ fill }}),
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
    p <- p + ggplot2::scale_fill_viridis_c(
      alpha = 0.5,
      option = gradient %||% "E",
      na.value = na_fill %||% off_white(),
      direction = grad_direction %||% -1
    )
  }

  if (interactive) {
    plotly::ggplotly(
      p +
        ggplot2::coord_sf() +
        ggplot2::theme_void()
    )
  } else {
    p +
      ggplot2::coord_sf() +
      ggplot2::theme_void()
  }
}
