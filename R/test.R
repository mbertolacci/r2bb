#' Read a test from a file
#'
#' This function reads a test from a file. The file can be a YAML
#' file, or an RYaml file (in which case [render_ryaml()] is used to render it).
#'
#' @param path The file to read the test from. Either a YAML file, or an
#' RYaml file (in which case [render_ryaml()] is used to render it).
#' @param allow_multiple Whether to allow multiple tests in the file.
#' These must be separate by the YAML document boundary, `---`. If this
#' is true, the return value is a list of tests; otherwise, the return
#' value is a single test.
#' @param normalize Whether to normalize the test. This will convert the
#' test to the canonical format used by the package, which also performs
#' validation of the test.
#' @param normalise Synonym for `normalize`.
#' @param envir The environment to use for RYaml files.
#' @param ... Additional arguments to pass to [render_ryaml()], if it's needed.
#' @return A test or a list of tests
#' @export
read_test <- function(
  path,
  allow_multiple = FALSE,
  normalize = TRUE,
  normalise = normalize,
  envir = parent.frame(),
  ...
) {
  output <- read_ryaml(path, allow_multiple = allow_multiple, envir = envir, ...)
  if (normalise) {
    if (allow_multiple) {
      lapply(output, normalize_test)
    } else {
      normalize_test(output)
    }
  } else {
    output
  }
}

#' Normalize a test
#'
#' This function normalizes a test, which means that it will convert
#' the test to the canonical format used by the package. This also performs
#' validation of the test.
#'
#' @param test The test to normalize.
#' @return The normalized test
#' @seealso [normalize()]
#' @export
normalize_test <- function(test) {
  if (inherits(test, 'r2bb_test')) {
    return(test)
  }

  if (is.null(test$type)) {
    test$type <- 'test'
  }
  stopifnot(test$type == 'test')

  test$title <- .normalize_string(test$title)
  test$description <- .normalize_string(test$description)
  test$instructions <- .normalize_string(test$instructions)
  if (is.null(test$contents)) {
    test$contents <- list()
  }
  for (i in seq_along(test$contents)) {
    if (is.null(test$contents[[i]])) {
      stop('Content type is null')
    }
    if (test$contents[[i]]$type == 'random_block') {
      test$contents[[i]]$pool <- normalize_pool(test$contents[[i]]$pool)
      test$contents[[i]]$pool$questions_to_display <- .normalize_integer(
        test$contents[[i]]$questions_to_display,
        1
      )
      test$contents[[i]]$pool$points_per_question <- .normalize_numeric(
        test$contents[[i]]$points_per_question,
        1
      )
    } else if (test$contents[[i]]$type == 'question') {
      test$contents[[i]]$points <- .normalize_numeric(
        test$contents[[i]]$points,
        1
      )
      test$contents[[i]]$question <- normalize_question(test$contents[[i]]$question)
    }
  }
  class(test) <- 'r2bb_test'
  test
}

#' @title Convert a test to Blackboard XML
#' @description This function converts a test to Blackboard XML format.
#' This is mostly used as part of [to_bbxml_package()], but can be called
#' directly if you want the XML for a single test.
#' @param x The test to convert
#' @param convert_rich_text Method to convert rich text to HTML. By default,
#' `md_to_html_pandoc` is used, which converts Markdown to HTML using
#' [pandoc](https://pandoc.org/). Can be FALSE to prevent any conversion.
#' @param convert_rich_text_options Additional arguments to pass to the
#' `convert_rich_text` function.
#' @param ... Additional arguments passed to methods
#' @return A character string containing the Blackboard XML
#' @seealso [to_bbxml()]
#' @export
to_bbxml.r2bb_test <- function(
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
    contents = list()
  )
  pool_resource_names <- character(0)
  n_pools <- 0L
  max_score <- 0
  for (i in seq_along(x$contents)) {
    if (x$contents[[i]]$type == 'random_block') {
      questions_to_display <- x$contents[[i]]$pool$questions_to_display
      points_per_question <- x$contents[[i]]$pool$points_per_question
      n_pools <- n_pools + 1L
      pool_resource_name <- sprintf('res%05d', n_pools)
      pool_resource_names <- c(pool_resource_names, pool_resource_name)
      template_data$contents[[i]] <- list(
        id = uuid::UUIDgenerate(),
        is_random_block = TRUE,
        max_score = questions_to_display * points_per_question,
        weighting = points_per_question,
        questions_to_display = questions_to_display,
        pool_resource_name = pool_resource_name
      )
      max_score <- max_score + questions_to_display * points_per_question
    } else if (x$contents[[i]]$type == 'question') {
      template_data$contents[[i]] <- list(
        is_question = TRUE,
        item = to_bbxml(
          x$contents[[i]]$question,
          convert_rich_text = convert_rich_text,
          convert_rich_text_options = NULL
        )
      )
      max_score <- max_score + x$contents[[i]]$question$max_score
    }
  }
  template_data$max_score <- max_score

  output <- .fill_template('test.xml.mustache', template_data)
  attr(output, 'pool_resource_names') <- pool_resource_names
  output
}

#' Convert from Blackboard XML to an R2BB test
#'
#' This function converts an object from Blackboard XML format to an R2BB test.
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
#' @param pools_by_name A named list of pools, by resource name; used to fill in
#' the pool details for random blocks.
#' @seealso [from_bbxml_package()], [from_bbxml_question()], [from_bbxml_pool()]
#' @return An R2BB test
from_bbxml_test <- function(x, convert_html_to_markdown = TRUE, pools_by_name) {
  stopifnot(inherits(x, 'xml_document') || inherits(x, 'xml_node'))

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
  stopifnot(assessment_type == 'Test')

  output <- list(
    type = 'test',
    title = xml2::xml_attr(assessment, 'title'),
    description = assessment |>
      xml2::xml_find_first('.//presentation_material/flow_mat/material/mat_extension/mat_formattedtext') |>
      xml2::xml_text() |>
      convert_rich_text(),
    instructions = assessment |>
      xml2::xml_find_first('.//rubric/flow_mat/material/mat_extension/mat_formattedtext') |>
      xml2::xml_text() |>
      convert_rich_text(),
    contents = list()
  )

  section_children <- xml2::xml_find_all(assessment, './section') |>
    xml2::xml_children()
  
  for (section_child in section_children) {
    section_child_name <- xml2::xml_name(section_child)
    if (section_child_name == 'item') {
      question <- from_bbxml_question(section_child, convert_html_to_markdown = convert_html_to_markdown)
      output$contents[[length(output$contents) + 1]] <- list(
        type = 'question',
        points = question$max_score,
        question = question
      )
    } else if (section_child_name == 'section') {
      section_type <- section_child |>
        xml2::xml_find_first('.//sectionmetadata/bbmd_sectiontype') |>
        xml2::xml_text()

      if (section_type != 'Random Block') {
        stop('Unknown section type: ', section_type)
      }

      weighting <- section_child |>
        xml2::xml_find_first('.//sectionmetadata/qmd_weighting') |>
        xml2::xml_text() |>
        as.numeric()

      questions_to_display <- section_child |>
        xml2::xml_find_first('.//selection_number') |>
        xml2::xml_text() |>
        as.integer()

      resource_name <- section_child |>
        xml2::xml_find_first('.//sourcebank_ref') |>
        xml2::xml_text()

      pool <- if (!missing(pools_by_name)) {
        if (!(resource_name %in% names(pools_by_name))) {
          stop('Pool not found: ', resource_name)
        }
        pools_by_name[[resource_name]]
      } else {
        resource_name
      }

      output$contents[[length(output$contents) + 1]] <- list(
        type = 'random_block',
        points_per_question = weighting,
        questions_to_display = questions_to_display,
        pool = pool
      )
    }
  }
  
  class(output) <- 'r2bb_test'
  output
}
