#' Plot State-level Map with Two Bubbles
#'
#' @inheritParams map_lgas3
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
#' map_states3(state_data, fill = estimate, light_b = estimate, dark_b = estimate_scaled, label = TRUE)
#'
map_states3 <- function(
    .data,
    fill,
    light_b,
    dark_b,
    state = state,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  if (!is.logical(label)) {
    rlang::abort("The label value is not a logical vector. Logical vectors in R are written in capital letters and unquoted. Did you forget to write the word in capital letters or did you add quotes?")
  }

  if (!is.logical(interactive)) {
    rlang::abort("The interactive value supplied is not a logical vector. Did you forget to write the word in capital letters?")
  }

  if (!is.null(size) && !is.numeric(size)) {
    rlang::abort("`size` value must be in numbers")
  }

  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  if (!is.null(cols) && length(cols) > 1 && length(cols) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `col` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  noise <- stats::runif(1, min = 0.05, max = 0.1)

  df <- ndr_states(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by({{ state }} == {{ state }}),
      multiple = "all"
    )

  lab_data <- df |>
    dplyr::group_by(.data$state) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric), mean
      ),
      {{ dark_b }} := dplyr::first({{ dark_b }}),
      {{ light_b }} := dplyr::first({{ light_b }}),
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
        ggplot2::aes(.data$long, .data$lat, group = .data$group),
        fill = cols,
        color = "black",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ light_b }}),
        color = "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(5, 15))
  } else {
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
        ggplot2::aes(.data$long, .data$lat, size = {{ light_b }}),
        color = "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ dark_b }}),
        color = "#ae2234",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(5, 15))
  }


  if (label) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2, .data$lat2, label = .data$state),
        size = size %||% 3,
        check_overlap = TRUE
      ) +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = round({{ dark_b }})), size = size %||% 3,
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
