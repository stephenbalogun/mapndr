#' Coordinates of Nigeria LGAs
#'
#' @param state string (character). The names of state (or LGA)
#'
#' @return coordinates of selected LGAs
#' @export
#'
#' @examples
#'
#' ## get LGA coordinates for Gombe state
#'
#' gom_lga_coord <- ndr_lgas("Gombe")
#'
#' ## get LGA coordinates for Anambra and Kogi
#' ana_kog_coord <- ndr_lgas(c("Anambra", "Kogi"))
#'
ndr_lgas <- function(state = NULL) {
  if (!is.null(state) && !all(state %in% c(naijR::states(), "FCT"))) {
    rlang::abort("state must be any or a combination of the recognized Nigeria states based on NDR format")
  }

  if (!is.null(state)) {
    state <- stringr::str_replace_all(state, "FCT", "Federal Capital Territory")
  }


  if (is.null(state) || all(unique(naijR::lgas_nigeria$state) %in% state)) {
    st <- naijR::lgas_nigeria |>
      dplyr::select(.data$lga, .data$state)

    ndr_lgas <- ggplot2::map_data(
      naijR::map_ng(naijR::lgas())
    )
  } else {
    st <- naijR::lgas_nigeria |>
      dplyr::filter(.data$state %in% state) |>
      dplyr::select(.data$lga, .data$state)

    ndr_lgas <- purrr::map(
      state,
      \(state) ggplot2::map_data(
        naijR::map_ng(naijR::lgas(state))
      )
    ) |>
      purrr::list_rbind()
  }

  ndr_lgas |>
    dplyr::rename(lga = .data$region) |>
    dplyr::select(-tidyselect::last_col()) |>
    dplyr::left_join(st, by = "lga", multiple = "all") |>
    dplyr::mutate(
      lga = stringr::str_replace_all(.data$lga, diff()),
      state = ifelse(state == "Federal Capital Territory", "FCT", state)
    )
}



diff <- function() {
  c(
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
}
