#' Plot LGA-level Map with Two Bubbles
#'
#' @inheritParams map_lgas
#' @inheritParams map_lgas2
#' @param light_bubble the variable name of the column to be used for the lighter (bigger) bubble
#' @param dark_bubble the variable name of the column to be used for the darker (smaller) bubble
#' @param light_bubble_color a string of color. When this is supplied, it overides the default color for the `light_bubble` plots
#' @param dark_bubble_color a string of color. When this is supplied, it overides the default color for the `dark_bubble` plots
#' @param label_dark_bubble logical (boolean), indicating if the values for the `dark_bubble` data should be displayed
#' @param label_dark_bubble_color when supplied, this color will be used as font-color for the `dark_bubble` label
#' @param size_dark_bubble the font-size for the `dark_bubble` labels in whole numbers. The default is 2
#'
#' @return a three-dimensional LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data, filling by `prev_x` and light_b by `population` and dark_b by `incidence_x`
#'
#' map_lgas3(
#' lga_data,
#' fill = prev_x,
#' light_bubble = population,
#' dark_bubble = incidence_x,
#' label_dark_bubble = TRUE
#' )
#'
map_lgas3 <- function(
    .data,
    fill,
    light_bubble,
    dark_bubble,
    state = state,
    lga = lga,
    label_lga = TRUE,
    label_fill = FALSE,
    label_dark_bubble = FALSE,
    label_lga_color = NULL,
    label_fill_color = NULL,
    label_dark_bubble_color = NULL,
    fill_colors = NULL,
    light_bubble_color = NULL,
    dark_bubble_color = NULL,
    size_lga = NULL,
    size_fill = NULL,
    size_dark_bubble = NULL,
    border_color = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  validate_lga_maps(label_lga, label_fill, size_lga, size_fill, interactive)

  if (!is.null(fill_colors) && length(fill_colors) > 1 && length(fill_colors) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `fill_colors` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }

  noise <- stats::runif(1, min = 0.01, max = 0.02)


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
        color = border_color %||% "black"
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ light_bubble }}),
        color = light_bubble_color %||% "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat,
          size = {{ dark_bubble }}
        ),
        color = dark_bubble_color %||% "#ae2234",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size_area(max_size = 10)
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
        ggplot2::aes(.data$long, .data$lat, size = {{ light_bubble }}),
        color = light_bubble_color %||% "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat,
          size = {{ dark_bubble }}
        ),
        color = dark_bubble_color %||% "#ae2234",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size_area(max_size = 10)
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


  if (label_dark_bubble) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat,
          label = scales::comma({{ dark_bubble }})
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
