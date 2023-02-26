#' Coordinates of Nigeria States
#'
#' @inheritParams ndr_lgas
#'
#' @return coordinates of selected states
#' @export
#'
#' @examples
#'
#' ## get the coordinates of Zamfara state
#'
#' zam_coord <- ndr_states("Zamfara")
#'
#' ## get the coordinates of Bayelsa and Ebonyi states
#' bay_ebo_coords <- ndr_states(c("Bayelsa", "Ebonyi"))
ndr_states <- function(state) {


  if (all(!state %in% c(naijR::states(), "FCT"))) {
    rlang::abort("state must be any or a combination of the recognized Nigeria states based on NDR format")
  }


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
