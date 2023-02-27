#' Plot LGA-level Map with Two Bubbles
#'
#' @inheritParams map_lgas
#' @param light_b the variable name of the column to be used for the lighter (bigger) bubble
#' @param dark_b the variable name of the column to be used for the darker (smaller) bubble
#'
#' @return a three-dimensional LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data, filling by `prev_x` and light_b by `population` and dark_b by `incidence_x`
#'
#' map_lgas3(lga_data, fill = prev_x, light_b = population, dark_b = incidence_x, label = TRUE)
#'
map_lgas3 <- function(
    .data,
    fill,
    light_b,
    dark_b,
    state = state,
    lga = lga,
    label = FALSE,
    cols = NULL,
    size = NULL,
    interactive = FALSE) {
  states <- dplyr::distinct(.data, {{ state }}) |> dplyr::pull({{ state }})

  fill_vec <- dplyr::select(.data, {{ fill }}) |> dplyr::pull({{ fill }})

  validate_maps(label, interactive, size, cols)

  if (!is.null(cols) && length(cols) > 1 && length(cols) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `col` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
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
        ggplot2::aes(.data$long, .data$lat, group = .data$lga),
        fill = cols,
        color = "black"
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ light_b }}),
        color = "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat,
          size = {{ dark_b }}
        ),
        color = "#ae2234",
        show.legend = FALSE
      ) +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size(range = c(5, 15))
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = "black"
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = {{ light_b }}),
        color = "#f5c1c1",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat,
          size = {{ dark_b }}
        ),
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
        ggplot2::aes(.data$long2, .data$lat2, label = .data$lga),
        size = size %||% 2,
        check_overlap = TRUE
      ) +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(
          .data$long, .data$lat,
          label = round({{ dark_b }})
        ),
        size = size %||% 2,
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
