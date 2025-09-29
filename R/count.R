#' Count elements or occurrences in a vector
#'
#' @description
#' Versatile counting helper:
#' - If `value` is `NULL` (default), returns the number of **non-missing** elements.
#' - If `value` is provided, returns how many times `value` occurs in `x`.
#'
#' @param x An atomic vector (logical, integer, double, character, etc.).
#' @param value A single value to count in `x`. If `NULL`, counts non-missing elements.
#' @param na.rm Logical. If `TRUE` and `value` is supplied, missing values are ignored in the comparison.
#'
#' @return An integer scalar with the count.
#' @examples
#' # Count non-missing elements
#' count(c(1, 2, NA, 4))           # 3
#'
#' # Count occurrences of a value
#' count(c("a", "b", "a"), value = "a")  # 2
#'
#' # Count logicals
#' count(c(TRUE, FALSE, TRUE), value = TRUE)  # 2
#'
#' @export
count <- function(x, value = NULL, na.rm = TRUE) {
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector (logical, integer, double, character, etc.).")
  }
  if (is.null(value)) {
    return(sum(!is.na(x)))
  }
  # Ensure length-1 value
  if (length(value) != 1L) stop("`value` must be length 1.")
  # Compare safely
  if (na.rm) {
    return(sum(x[!is.na(x)] == value))
  } else {
    return(sum(x == value, na.rm = TRUE))
  }
}
