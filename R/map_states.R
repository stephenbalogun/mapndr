#' Plot State-level Map
#'
#' @inheritParams map_lgas
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
#' map_states(state_data, fill = estimate, label = TRUE, interactive = TRUE)
map_states <- function(
    .data,
    fill,
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

  df <- ndr_states(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by({{ state }} == {{ state }}),
      multiple = "all"
    )

  if (!is.null(cols) && length(cols) == 1) {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$group)
      ) +
      ggplot2::geom_polygon(
        fill = cols,
        color = "black"
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void()
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$group)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = "black"
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void()
  }

  if (label) {
    lab_data <- df |>
      dplyr::group_by(.data$state) |>
      dplyr::summarise(
        dplyr::across(
          tidyselect::where(is.numeric), mean
        ),
        .groups = "drop"
      )

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
