#' Multiply two numbers (vectorised)
#'
#' @description
#' Simple, fast multiplication of two numeric vectors (element-wise). Recycles
#' following R's standard recycling rules.
#'
#' @param a,b Numeric vectors.
#'
#' @return A numeric vector with `a * b`.
#' @examples
#' multiply(2, 3)          # 6
#' multiply(1:3, 10)       # 10 20 30
#' multiply(c(0.5, 2), 4)  # 2 8
#'
#' @export
multiply <- function(a, b) {
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("Both `a` and `b` must be numeric.")
  }
  a * b
}
