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

  switch(n,
    "Too little. Choose between 2 and 7 categories",
    c(cols[c(2, 6)]),
    c(cols[c(2, 5, 7)]),
    c(cols[c(1, 3, 5, 7)]),
    c(cols[c(1, 3, 5, 6, 8)]),
    c(cols[c(1, 2, 4, 5, 6, 8)]),
    c(cols[c(1, 2, 4, 5, 6, 7, 8)]),
    "Invalid entry. Choose a number between 2 and 7 categories"
  )
}


off_white <- function() {
  "#fcfcfc"
}

border_grey <- function() {
  "#787778"
}

label_grey <- function() {
  "#403f40"
}
