.question_to_bbxml_common <- function(x, convert_rich_text) {
  list(
    id = uuid::UUIDgenerate(),
    title = x$title,
    question_text = convert_rich_text(x$question_text),
    positive_feedback = convert_rich_text(x$positive_feedback),
    negative_feedback = convert_rich_text(x$negative_feedback),
    instructor_notes = x$instructor_notes,  # For some reason, this is plain text
    partial_credit = if (x$partial_credit) 'true' else 'false',
    is_partial_credit = x$partial_credit,
    max_score = x$max_score
  )
}

#' @title Convert a question to Blackboard XML
#' @description This function converts a question to Blackboard XML format.
#' @param x The question to convert
#' @param convert_rich_text Method to convert rich text to HTML. By default,
#' `md_to_html_pandoc` is used, which converts Markdown to HTML using
#' [pandoc](https://pandoc.org/). Can be FALSE to prevent any conversion.
#' @param convert_rich_text_options Additional arguments to pass to the
#' `convert_rich_text` function.
#' @param ... Additional arguments passed to methods
#' @return A character string containing the Blackboard XML
#' @seealso [to_bbxml()]
#' @export
to_bbxml.r2bb_question_matching <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  template_data <- .question_to_bbxml_common(x, convert_rich_text)

  n_questions <- length(x$questions)
  n_answers <- length(x$answers)
  template_data$responses <- NULL
  for (i in seq_along(x$questions)) {
    choices <- lapply(seq_len(n_answers), function(x) list(id = uuid::UUIDgenerate()))
    template_data$responses <- c(
      template_data$responses,
      list(list(
        id = uuid::UUIDgenerate(),
        correct_choice_id = choices[[x$questions[[i]]$answer_index]]$id,
        choices = choices,
        text = convert_rich_text(x$questions[[i]]$text),
        partial_credit_percentage = x$questions[[i]]$partial_credit_percentage
      ))
    )
  }
  template_data$answers <- lapply(x$answers, convert_rich_text)
  .fill_template('item-matching.xml.mustache', template_data)
}

#' Convert a single blank question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_single_blank <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  template_data <- .question_to_bbxml_common(x, convert_rich_text)
  template_data$responses <- lapply(x$answers, function(answer) {
    list(
      id = uuid::UUIDgenerate(),
      text = answer$text,
      case = if (answer$case_sensitive) 'Yes' else 'No',
      evaluation_type = if (answer$type == 'exact') {
        'EXACT'
      } else if (answer$type == 'contains') {
        'CONTAINS'
      } else if (answer$type == 'pattern') {
        'MATCHES'
      }
    )
  })

  .fill_template('item-single-blank.xml.mustache', template_data)
}

#' Convert a multiple blanks question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_multiple_blanks <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  template_data <- .question_to_bbxml_common(x, convert_rich_text)
  template_data$responses <- NULL
  for (i in seq_along(x$answers)) {
    response_i <- list(
      name = names(x$answers)[i]
    )
    response_i$answers <- lapply(x$answers[[i]], function(answer) {
      list(
        text = answer$text,
        case = if (answer$case_sensitive) 'Yes' else 'No',
        is_exact = answer$type == 'exact',
        is_contains = answer$type == 'contains',
        is_matches = answer$type == 'pattern'
      )
    })
    template_data$responses <- c(template_data$responses, list(response_i))
  }
  .fill_template('item-multiple-blanks.xml.mustache', template_data)
}

#' Convert a multiple choice question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_multiple_choice <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  template_data <- .question_to_bbxml_common(x, convert_rich_text)

  template_data$answer_numbering <- x$answer_numbering
  template_data$answer_orientation <- x$answer_orientation
  template_data$shuffle <- if (x$random_order) 'Yes' else 'No'
  template_data$responses <- NULL
  for (i in seq_along(x$answers)) {
    response_id <- uuid::UUIDgenerate()
    if (x$answers[[i]]$correct) {
      template_data$correct_response_id <- response_id
    }
    template_data$responses <- c(
      template_data$responses,
      list(list(
        id = response_id,
        text = convert_rich_text(x$answers[[i]]$text),
        partial_credit_percentage = x$answers[[i]]$partial_credit_percentage
      ))
    )
  }
  .fill_template('item-multiple-choice.xml.mustache', template_data)
}

#' Convert a multiple answer question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_multiple_answer <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  template_data <- .question_to_bbxml_common(x, convert_rich_text)
  template_data$responses <- NULL
  template_data$answer_numbering <- x$answer_numbering
  template_data$answer_orientation <- x$answer_orientation
  template_data$shuffle <- if (x$random_order) 'Yes' else 'No'
  template_data$responses <- NULL
  for (i in seq_along(x$answers)) {
    response_id <- uuid::UUIDgenerate()
    template_data$responses <- c(
      template_data$responses,
      list(list(
        id = response_id,
        text = convert_rich_text(x$answers[[i]]$text),
        is_correct = x$answers[[i]]$correct,
        partial_credit_percentage = x$answers[[i]]$partial_credit_percentage
      ))
    )
  }
  .fill_template('item-multiple-answer.xml.mustache', template_data)
}

#' Convert a numeric question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_numeric <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)
  template_data <- .question_to_bbxml_common(x, convert_rich_text)
  template_data$response_id <- uuid::UUIDgenerate()
  template_data$answer <- x$answer
  template_data$lower_bound <- x$answer - x$tolerance
  template_data$upper_bound <- x$answer + x$tolerance
  .fill_template('item-numeric.xml.mustache', template_data)
}

#' Convert a file upload question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_file_upload <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)
  template_data <- .question_to_bbxml_common(x, convert_rich_text)
  .fill_template('item-file-upload.xml.mustache', template_data)
}

#' Convert a short answer question to Blackboard XML
#' @rdname to_bbxml.r2bb_question_matching
#' @export
to_bbxml.r2bb_question_short_answer <- function(
  x,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(options = '--mathml'),
  ...
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)
  template_data <- .question_to_bbxml_common(x, convert_rich_text)
  template_data$answer <- convert_rich_text(x$answer)
  template_data$answer_rows <- x$answer_rows
  .fill_template('item-short-answer.xml.mustache', template_data)
}
