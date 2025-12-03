#' The Birthday Function
#'
#' @param x the number of people within the group
#'
#' @returns a probability scalar
#' @export
#'
#' @examples
#' birthday(x = 20:25)
birthday <- function(x) {
  1-exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}

