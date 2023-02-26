#' Selected Colors for Charts
#'
#' @param n number of colors. This should be equal to the number of categories contained in the data
#'
#' @return selected colors
#'
#' @keywords internal
#'
#' @examples NULL
my_cols <- function(n) {
  cols <- c("#ff6666", "#f5c1c1", "#ffe6e6", "#ffcc99", "#e6e600", "#cae8cb", "#c1f5f5", "#1ac6ff")

  if (is.numeric(n) && n < 2) {
    rlang::abort("Too little categories. Choose `n` between 2 and 7")
  }

  if (is.numeric(n) && n > 7) {
    rlang::abort("Too many categories in the data. Choose `n` between 2 and 7 categories")
  }

  if (!is.numeric(n) || nchar(n) != 1) {
    rlang::abort("`n` must be an integer between 2 and 7")
  }

  switch(n,
    "Too little. Choose between 2 and 7 categories",
    c(cols[2], cols[6]),
    c(cols[2], cols[5], cols[7]),
    c(cols[1], cols[3], cols[5], cols[7]),
    c(cols[1], cols[3], cols[5], cols[6], cols[8]),
    c(cols[1], cols[2], cols[4], cols[5], cols[6], cols[8]),
    c(cols[1], cols[2], cols[4], cols[5], cols[6], cols[7], cols[8]),
    "Too many categories in the data. Choose between 2 and 7 categories"
  )

}

# switch(n,
#        `1` = "Too little. Choose between 2 and 7 categories",
#        `2` = c(cols[2], cols[6]),
#        `3` = c(cols[2], cols[5], cols[7]),
#        `4` = c(cols[1], cols[3], cols[5], cols[7]),
#        `5` = c(cols[1], cols[3], cols[5], cols[6], cols[8]),
#        `6` = c(cols[1], cols[2], cols[4], cols[5], cols[6], cols[8]),
#        `7` = c(cols[1], cols[2], cols[4], cols[5], cols[6], cols[7], cols[8]),
#        "Too many categories in the data. Choose between 2 and 7 categories"
#        )
