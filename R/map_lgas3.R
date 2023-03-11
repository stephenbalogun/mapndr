#' Plot LGA-level Map with Two Bubbles
#'
#' @inheritParams map_lgas
#' @inheritParams map_lgas2
#' @param big_bubble the variable name of the column to be used for the lighter (bigger) bubble
#' @param small_bubble the variable name of the column to be used for the darker (smaller) bubble
#' @param big_bubble_color a string of color. When this is supplied, it overides the default color for the `big_bubble` plots
#' @param small_bubble_color a string of color. When this is supplied, it overides the default color for the `small_bubble` plots
#' @param label_small_bubble logical (boolean), indicating if the values for the `small_bubble` data should be displayed
#' @param label_small_bubble_color when supplied, this color will be used as font-color for the `small_bubble` label
#' @param size_small_bubble the font-size for the `small_bubble` labels in whole numbers. The default is 2
#' @param bubble_transparency the alpha transparency of the bubbles. Value ranges from 0 (transparent) to 1(solid)
#'
#' @return a three-dimensional LGA-level map
#' @export
#'
#' @examples
#'
#' ## map the lga_data, filling by `prev_x` and light_b by `population` and dark_b by `incidence_x`
#'
#' map_lgas3(
#'   lga_data,
#'   fill = prev_x,
#'   big_bubble = population,
#'   small_bubble = incidence_x,
#'   label_small_bubble = TRUE
#' )
#'
map_lgas3 <- function(
    .data,
    fill,
    big_bubble,
    small_bubble,
    state = state,
    lga = lga,
    label_lga = TRUE,
    label_fill = FALSE,
    label_small_bubble = TRUE,
    label_lga_color = NULL,
    label_fill_color = NULL,
    label_small_bubble_color = NULL,
    fill_colors = NULL,
    big_bubble_color = NULL,
    small_bubble_color = NULL,
    bubble_transparency = NULL,
    size_lga = NULL,
    size_fill = NULL,
    size_small_bubble = NULL,
    border_color = NULL,
    border_width = NULL,
    gradient = NULL,
    grad_direction = NULL,
    na_fill = NULL,
    all_regions = FALSE,
    interactive = FALSE) {
  states <- dplyr::pull(.data, {{ state }}) |> unique()

  fill_vec <- dplyr::pull(.data, {{ fill }}) |> unique()

  noise <- stats::runif(1, min = 0.01, max = 0.02)

  validate_lga_maps(label_lga, label_fill, size_lga, size_fill, all_regions, interactive)

  if (!is.null(fill_colors) && length(fill_colors) > 1 && length(fill_colors) != length(unique(fill_vec))) {
    rlang::abort("The values supplied to `fill_colors` argument must be colors of length equal to the unique entries in the `fill` variable! Did you supply discrete colors to a continuous `fill` variable?")
  }


  if (all(unique(naijR::lgas_nigeria$state) %in% states) || all_regions) {
    df <- ndr_lgas() |>
      dplyr::left_join(
        .data,
        dplyr::join_by(
          state == {{ state }},
          lga == {{ lga }}
        ),
        multiple = "all"
      )
  } else {
    df <- ndr_lgas(states) |>
      dplyr::left_join(
        .data,
        dplyr::join_by(
          state == {{ state }},
          lga == {{ lga }}
        ),
        multiple = "all"
      )
  }


  lab_data <- df |>
    dplyr::group_by(.data$state, .data$lga) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric), mean
      ),
      {{ small_bubble }} := dplyr::first({{ small_bubble }}),
      {{ big_bubble }} := dplyr::first({{ big_bubble }}),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      long2 = .data$long + (2 * noise),
      lat2 = .data$lat + (2 * noise)
    ) |>
    tidyr::pivot_longer(
      cols = c({{ big_bubble }}, {{ small_bubble }}),
      names_to = "indicator",
      values_to = "value"
    )


  if (!is.numeric(fill_vec)) {
    lab_data <- df |>
      dplyr::distinct(.data$state, .data$lga, {{ fill }}) |>
      dplyr::left_join(
        lab_data,
        dplyr::join_by(
          state,
          lga
        ),
        multiple = "all"
      )
  }

  if (!is.null(fill_colors) && length(fill_colors) == 1) {
    p <- df |>
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga),
        fill = fill_colors,
        color = border_color %||% border_grey(),
        linewidth = border_width %||% 0.2
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = .data$value, color = .data$indicator),
        alpha = bubble_transparency %||% 0.5,
        show.legend = c("indicator" = TRUE)
      )
  } else {
    p <- df |>
      ggplot2::ggplot(
        ggplot2::aes(.data$long, .data$lat, group = .data$lga)
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(fill = {{ fill }}),
        color = border_color %||% border_grey(),
        linewidth = border_width %||% 0.2
      ) +
      ggplot2::geom_point(
        data = lab_data,
        ggplot2::aes(.data$long, .data$lat, size = .data$value, color = .data$indicator),
        alpha = bubble_transparency %||% 0.5,
        show.legend = c("indicator" = TRUE)
      )
  }


  if (label_lga) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2, .data$lat2, label = .data$lga),
        size = size_lga %||% 2,
        color = label_lga_color %||% label_grey(),
        check_overlap = TRUE
      )
  }


  if (label_small_bubble) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data |> dplyr::filter(.data$indicator == unique(lab_data$indicator)[[2]]),
        ggplot2::aes(
          .data$long,
          .data$lat,
          label = ifelse(any(.data$value < 1), .data$value, scales::comma(.data$value))
        ),
        size = size_small_bubble %||% 2,
        color = label_small_bubble_color %||% "#ffffff",
        check_overlap = TRUE
      )
  }

  if (label_fill) {
    p <- p +
      ggplot2::geom_text(
        data = lab_data,
        ggplot2::aes(.data$long2 + noise, .data$lat2 + noise, label = {{ fill }}),
        size = size_fill %||% 2,
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
        direction = grad_direction %||% -1
      )
  }

  if (interactive) {
    plotly::ggplotly(
      p +
        ggplot2::coord_sf() +
        ggplot2::theme_void() +
        ggplot2::scale_size_area(max_size = 15, guide = "none") +
        ggplot2::scale_color_viridis_d() +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))
    )
  } else {
    p +
      ggplot2::coord_sf() +
      ggplot2::theme_void() +
      ggplot2::scale_size_area(max_size = 15, guide = "none") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))
  }
}
