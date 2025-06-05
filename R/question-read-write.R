#' Read a question from a file
#'
#' This function reads a question or a list of questions from a file. The file
#' can be a YAML file, or an RYaml file (in which case [render_ryaml()] is used
#' to render it).
#'
#' @param path The file to read the question from. Either a YAML file, or an
#' RYaml file (in which case [render_ryaml()] is used to render it).
#' @param allow_multiple Whether to allow multiple questions in the file.
#' These must be separate by the YAML document boundary, `---`. If this
#' is true, the return value is a list of questions; otherwise, the return
#' value is a single question.
#' @param normalize Whether to normalize the question. This will convert the
#' question to the canonical format used by the package, which also performs
#' validation of the question.
#' @param normalise Synonym for `normalize`.
#' @param envir The environment to use for RYaml files.
#' @param ... Additional arguments to pass to [render_ryaml()], if it's needed.
#' @return A question or a list of questions
#' @export
read_question <- function(
  path,
  allow_multiple = FALSE,
  normalize = TRUE,
  normalise = normalize,
  envir = parent.frame(),
  ...
) {
  output <- .read_ryaml_or_yaml(path, allow_multiple = allow_multiple, envir = envir, ...)
  if (normalise) {
    if (allow_multiple) {
      lapply(output, normalize_question)
    } else {
      normalize_question(output)
    }
  } else {
    output
  }
}

#' Write a question to a file
#'
#' This function writes a question to a file. The file can be a YAML file, or an
#' RYaml file (in which case [render_ryaml()] is used to render it).
#'
#' @param x The question or questions to write to the file.
#' @param path The path to the file to write the question to.
#' @param ... Additional arguments to pass to [render_ryaml()], if it's needed.
#' @export
write_question <- function(x, path, ...) {
  if (inherits(x, 'r2bb_question')) {
    x <- list(x)
  } else {
    for (question in x) {
      if (!inherits(question, 'r2bb_question')) {
        stop('All questions must be of class "r2bb_question"')
      }
    }
  }
  .write_yaml_multiple(x, path, ...)
}
