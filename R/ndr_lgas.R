#' Coordinates of Nigeria LGAs
#'
#' @param state string (character). The names of state (or LGA)
#'
#' @return coordinates of selected LGAs
#' @export
#'
#' @examples NULL


ndr_lgas <- function(state) {

  diff <- c(
    "Girei" = "Girie", "Lamurde" = "Larmurde", "Oturkpo" = "Otukpo",
    "Warri South West" = "warri south west", "Igueben" = "igueben", "Ado Ekiti" = "Ado-Ekiti",
    "Ido Osi" = "Ido-Osi", "Irepodun/Ifelodun" = "Irepodun-Ifelodun", "Isi Uzo" = "isi uzo",
    "Abuja Municipal Area Council" = "AMAC", "Aboh Mbaise" = "aboh mbaise", "Kiyawa" = "kiyawa",
    "Ibeju-Lekki" = "ibeju-lekki", "Ifako-Ijaiye" = "ifako-ijaiye", "Ado-Odo/Ota" = "Ado-Odo Ota",
    "Yewa North" = "Yelwa North", "Ijebu Ode" = "Ijebu ode", "Imeko Afon" = "Imeko-Afon",
    "Ogun Waterside" = "Ogun waterside", "Shagamu" = "Sagamu", "Akoko North-East" = "Akoko North East",
    "Akoko North-West" = "Akoko North West", "Akoko South-East" = "Akoko South East",
    "Akoko South-West" = "Akoko South West", "Ile Oluji/Okeigbo" = "Ile Oluji Okeigbo",
    "Ibadan South-East" = "ibadan south-east", "Ona Ara" = "Ona ara"
  )

  state <- stringr::str_replace_all(state, "FCT", "Federal Capital Territory")

  st <- naijR::lgas_nigeria |>
    dplyr::filter(.data$state %in% state) |>
    dplyr::select(.data$lga, .data$state)

  purrr::map_df(
    state,
    ~ ggplot2::map_data(
      naijR::map_ng(naijR::lgas(.))
    )
  ) |>
    dplyr::rename(lga = .data$region) |>
    dplyr::select(-tidyselect::last_col()) |>
    dplyr::left_join(st, dplyr::join_by(.data$lga), multiple = "all") |>
    dplyr::mutate(
      lga = stringr::str_replace_all(.data$lga, diff),
      state = ifelse(state == "Federal Capital Territory", "FCT", state)
    )
}
