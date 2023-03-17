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
ndr_states <- function(state = NULL) {
  if (!is.null(state) && !all(state %in% c(naijR::states(), "FCT"))) {
    rlang::abort("state must be any or a combination of the recognized Nigeria states based on NDR format")
  }

  if (!is.null(state)) {
    state <- stringr::str_replace_all(state, "FCT", "Federal Capital Territory")
  }

  ggplot2::map_data(
    naijR::map_ng(
      naijR::states(state), plot = FALSE
    )
  ) |>
    dplyr::rename(state = .data$region) |>
    dplyr::select(-tidyselect::last_col()) |>
    dplyr::mutate(
      state = ifelse(state == "Federal Capital Territory", "FCT", state)
    )
}
