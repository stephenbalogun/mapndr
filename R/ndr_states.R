#' Coordinates of Nigeria States
#'
#' @inheritParams ndr_lgas
#'
#' @return coordinates of selected states
#' @export
#'
#' @examples NULL
ndr_states <- function(state) {
  state <- stringr::str_replace_all(state, "FCT", "Federal Capital Territory")

  df <- ggplot2::map_data(
    naijR::map_ng(
      naijR::states(state)
    )
  ) |>
    dplyr::rename(state = .data$region)

  if (any(state %in% "Federal Capital Territory")) {
    df <- df |>
      dplyr::mutate(state = ifelse(state == "Federal Capital Territory", "FCT", state))
  }

  df[, 1:5]
}
