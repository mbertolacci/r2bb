#' Print a question in a human-readable format
#' 
#' This function is used to print a question in a human-readable format.
#'
#' @param x The question to print
#' @param format The format to print the question in; markdown or yaml
#' @param ... Additional arguments to pass to the print method
#' @export
print.r2bb_question <- function(x, format = c('markdown', 'yaml'), ...) {
  format <- match.arg(format)
  if (format == 'markdown') {
    cat(to_markdown(x, ...))
  } else {
    cat(yaml::as.yaml(x, ...))
  }
}
