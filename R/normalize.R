#' Normalize an r2bb object
#'
#' This function normalizes an r2bb object. This takes a list, potentially
#' read from a YAML file, and converts it to the canonical format used by the
#' package. This also performs validation of the object. This just dispatches
#' to the appropriate normalize_*() function based on the `$type` field of the
#' object.
#'
#' @param x The object to normalize
#' @param ... Additional arguments passed to methods
#' @return A normalized r2bb object
#' @seealso [normalize_pool()], [normalize_test()], [normalize_question()]
#' @export
normalize <- function(x, ...) {
  if (x$type == 'pool') {
    normalize_pool(x, ...)
  } else if (x$type == 'test') {
    normalize_test(x, ...)
  } else if (x$type == 'question') {
    normalize_question(x, ...)
  } else {
    stop('Unknown object type')
  }
}
