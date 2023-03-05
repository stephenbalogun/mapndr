#' Plot LGA-level Map with Bubbles
#'
#' @inheritParams map_lgas
#' @param bubble the variable name of the column to be used for the bubble (point) plot
#' @param label_bubble logical (boolean), indicating if the values for the `bubble` data should be displayed
#' @param label_bubble_color when supplied, this color will be used as font-color for the `bubble` label
#' @param bubble_color a string of color. When this is supplied, it overides the default color
#' @param size_bubble the font-size for the `bubble` labels in whole numbers. The default is 2
#' @return a two-dimensional LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data accompanying the package, filling by `prev_x` and bubbles by `incidence_x`
#'
#' map_lgas2(lga_data, fill = prev_x, bubble = incidence_x, label_lga = TRUE)
#'
map_lgas2 <- function(
    .data,
    fill,
    bubble,
    state = state,
    lga = lga,
    label_lga = TRUE,
    label_fill = FALSE,
    label_bubble = FALSE,
    label_lga_color = NULL,
    label_fill_color = NULL,
    label_bubble_color = NULL,
    fill_colors = NULL,
    bubble_color = NULL,
    size_lga = NULL,
    size_fill = NULL,
    size_bubble = NULL,
    border_color = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  noise <- stats::runif(1, min = 0.01, max = 0.02)

  validate_lga_maps(label_lga, label_fill, size_lga, size_fill, interactive)


  if (!is.null(fill_colors) && length(fill_colors) > 1 && length(fill_colors) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `fill_colors` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  df <- ndr_lgas(states) |>
    dplyr::left_join(
      .data,
      dplyr::join_by(
        state == {{ state }},
        lga == {{ lga }}
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

  if (!is.numeric(fill_vec)) {
    lab_data <- df |>
      dplyr::distinct(.data$state, .data$lga, {{ fill }}) |>
      dplyr::left_join(
        lab_data,
        dplyr::join_by(
          state == {{ state }},
          lga == {{ lga }}
        )
      )
  }

  if (!is.null(fill_colors) && length(fill_colors) == 1) {
    p <- df |>
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga),
        fill = fill_colors,
        color = border_color %||% "black",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        color = bubble_color %||% "black",
        alpha = 0.4
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(0, 15))
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = border_color %||% "black"
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ bubble }}),
        color = bubble_color %||% "black",
        alpha = 0.4
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(0, 15))
  }


  if (label_lga) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2, .data$lat2, label = .data$lga),
        size = size_lga %||% 2,
        color = label_lga_color %||% "black",
        check_overlap = TRUE
      )
  }

  if (label_bubble) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, label = scales::comma({{ bubble }})),
        size = size_bubble %||% 2,
        color = label_bubble_color %||% "#ffffff",
        check_overlap = TRUE
      )
  }


  if (label_fill) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2 + noise, .data$lat2 + noise, label = {{ fill }}),
        size = size_fill %||% 2,
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
