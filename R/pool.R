#' Read a question pool from a file
#'
#' This function reads a question pool from a file. The file can be a YAML
#' file, or an RYaml file (in which case [render_ryaml()] is used to render it).
#'
#' @param file The file to read the question pool from. Either a YAML file, or an
#' RYaml file (in which case [render_ryaml()] is used to render it).
#' @param allow_multiple Whether to allow multiple pools in the file.
#' These must be separate by the YAML document boundary, `---`. If this
#' is true, the return value is a list of pools; otherwise, the return
#' value is a single pool.
#' @param normalize Whether to normalize the pool. This will convert the
#' pool to the canonical format used by the package, which also performs
#' validation of the pool.
#' @param normalise Synonym for `normalize`.
#' @param envir The environment to use for RYaml files.
#' @param ... Additional arguments to pass to [render_ryaml()], if it's needed.
#' @return a question pool or a list of question pools
#' @export
read_pool <- function(
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
      lapply(output, normalize_pool)
    } else {
      normalize_pool(output)
    }
  } else {
    output
  }
}

#' Normalize a question pool
#'
#' This function normalizes a question pool, which means that it will convert
#' the pool to the canonical format used by the package. This also performs
#' validation of the pool.
#'
#' @param pool The question pool to normalize.
#' @return The normalized question pool
#' @seealso [normalize()]
#' @export
normalize_pool <- function(pool) {
  if (inherits(pool, 'r2bb_pool')) {
    return(pool)
  }

  if (is.null(pool$type)) {
    pool$type <- 'pool'
  }
  stopifnot(pool$type == 'pool')

  pool$title <- .normalize_string(pool$title)
  pool$description <- .normalize_string(pool$description)
  pool$instructions <- .normalize_string(pool$instructions)
  pool$questions <- lapply(pool$questions, normalize_question)
  class(pool) <- 'r2bb_pool'
  pool
}

#' @title Convert a question pool to Blackboard XML
#' @description This function converts a question pool to Blackboard XML format.
#' @param x The question pool to convert
#' @param convert_rich_text Method to convert rich text to HTML. By default,
#' `md_to_html_pandoc` is used, which converts Markdown to HTML using
#' [pandoc](https://pandoc.org/). Can be FALSE to prevent any conversion.
#' @param convert_rich_text_options Additional arguments to pass to the
#' `convert_rich_text` function.
#' @param ... Additional arguments passed to methods
#' @return A character string containing the Blackboard XML
#' @seealso [to_bbxml()]
#' @export
to_bbxml.r2bb_pool <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  template_data <- list(
    id = uuid::UUIDgenerate(),
    section_id = uuid::UUIDgenerate(),
    title = x$title,
    instructions = convert_rich_text(x$instructions),
    description = convert_rich_text(x$description),
    items = sapply(
      x$questions,
      to_bbxml,
      convert_rich_text = convert_rich_text,
      convert_rich_text_options = NULL
    )
  )
  .fill_template('pool.xml.mustache', template_data)
}

#' Convert from Blackboard XML to an R2BB pool
#'
#' This function converts an object from Blackboard XML format to an R2BB pool.
#'
#' Generally you will not call this directly; instead, call [from_bbxml_package()]
#' on an exported zip package file.
#'
#' By default, rich text fields in HTML are converted to Markdown using pandoc. This
#' can work quite well, but is not perfect; results are not guaranteed to be
#' identical to the original.
#'
#' @param x An xml2 node or nodeset
#' @param convert_html_to_markdown Whether to convert HTML to Markdown.
#' @seealso [from_bbxml_package()], [from_bbxml_question()], [from_bbxml_test()]
#' @return An R2BB pool
from_bbxml_pool <- function(x, convert_html_to_markdown = TRUE) {
  if (!inherits(x, c('xml_node', 'xml_document'))) {
    stop('x must be an xml2 node or document')
  }

  convert_rich_text <- function(x) {
    if (convert_html_to_markdown) {
      html_to_md_pandoc(x)
    } else {
      x
    }
  }

  assessment <- xml2::xml_find_first(x, '//questestinterop/assessment')
  assessment_type <- assessment |>
    xml2::xml_find_first('.//assessmentmetadata/bbmd_assessmenttype') |>
    xml2::xml_text()
  stopifnot(assessment_type == 'Pool')

  output <- list(
    type = 'pool',
    title = xml2::xml_attr(assessment, 'title'),
    description = assessment |>
      xml2::xml_find_first('.//presentation_material/flow_mat/material/mat_extension/mat_formattedtext') |>
      xml2::xml_text() |>
      convert_rich_text(),
    instructions = assessment |>
      xml2::xml_find_first('.//rubric/flow_mat/material/mat_extension/mat_formattedtext') |>
      xml2::xml_text() |>
      convert_rich_text()
  )
  
  question_nodes <- xml2::xml_find_all(assessment, './/section/item')
  output$questions <- from_bbxml_question(
    question_nodes, 
    convert_html_to_markdown = convert_html_to_markdown
  )
  
  class(output) <- 'r2bb_pool'
  output
}

#' Print a question pool
#'
#' This function prints a question pool to the console; it will be converted
#' to Markdown using [to_markdown.r2bb_pool()].
#'
#' @param x The question pool to print
#' @param ... Additional arguments passed to methods
#' @return The question pool
#' @seealso [print.r2bb_question()]
#' @export
print.r2bb_pool <- function(x, ...) {
  cat(to_markdown(x), '\n')
}

#' Convert a question pool to Markdown
#'
#' This function converts a question pool to Markdown format.
#'
#' @param x The question pool to convert
#' @param ... Additional arguments passed to methods
#' @return A character string containing the Markdown
#' @seealso [to_markdown.r2bb_question()]
#' @export
to_markdown.r2bb_pool <- function(x, ...) {
  questions_text <- paste(sapply(x$questions, function(question) {
    stringr::str_replace_all(to_markdown(question), '(?m)^#', '###')
  }), collapse = '\n')

  .trim_with_newline(glue::glue('
# {x$title}

## Description

{x$description}

## Instructions

{x$instructions}

## Questions

{questions_text}
  '))
}