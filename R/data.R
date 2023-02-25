#' The 2022 population projection of LGAs in Abia state
#'
#' A minimal 2022 population estimate of LGAs in Abia state, Nigeria, as obtained from the
#' [City Population website]("https://www.citypopulation.de/en/nigeria/admin/"). The data has been pre-cleaned and two
#'  additional columns `prev_x` and `incidence_x` added based on random calculations.
#'
#' @format A data frame with 17 rows and 5 variables:
#' \describe{
#'   \item{state}{State of data}
#'   \item{lga}{Local Government Areas of the listed state}
#'   \item{population}{the 2022 population estimate per LGA}
#'   \item{prev_x}{random prevalence of a condition "x" created by the authors}
#'    \item{incidence_x}{random annual incidence of a condition "x" created by the authors}
#'   }
#' @note for more information, kindly visit \url{https://www.citypopulation.de/en/nigeria/admin/}
"lga_data"
