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

  n_col <- sort(sample(length(cols), n))

  cols[n_col]
}
