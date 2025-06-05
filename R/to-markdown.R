#' Convert an object to Markdown
#'
#' This function converts an object to Markdown format.
#' It is a generic function that dispatches based on the class of the object.
#'
#' @param x The object to convert to Markdown
#' @param ... Additional arguments passed to methods
#' @return A character string containing the Markdown
#' @seealso [to_markdown.r2bb_question_matching()]
#' @export
to_markdown <- function(x, ...) {
  UseMethod('to_markdown')
}
