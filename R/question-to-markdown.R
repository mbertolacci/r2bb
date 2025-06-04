#' Convert a question to Markdown
#'
#' This function is used to convert a question to Markdown format.
#'
#' @param x The question to convert
#' @param ... Additional arguments to pass to the conversion method
#' @export
to_markdown.r2bb_question <- function(x, ...) {
  stop('Unknown question type')
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_matching <- function(x, ...) {
  questions_text <- paste(sapply(seq_along(x$questions), function(i) {
    question <- x$questions[[i]]
    glue::glue('[A{question$answer_index}] (Q{i}) {.indent_lines(question$text, 12, TRUE)}')
  }), collapse = '\n')
  answers_text <- paste(sapply(seq_along(x$answers), function(i) {
    answer <- x$answers[[i]]
    glue::glue('(A{i}) {.indent_lines(answer, 5, TRUE)}')
  }), collapse = '\n')

  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Questions

{questions_text}

## Answers

{answers_text}

{.feedback_text(x)}

{.instructor_notes_text(x)}
'))
}

.format_blank_answers <- function(x) {
  paste(sapply(x, function(answer) {
    glue::glue('- {answer$text} [{answer$type}, case {if (answer$case_sensitive) "sensitive" else "insensitive"}]')
  }), collapse = '\n')
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_single_blank <- function(x, ...) {
  answers_text <- .format_blank_answers(x$answers)
  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Answers

{answers_text}

{.feedback_text(x)}

{.instructor_notes_text(x)}
'))
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_multiple_blanks <- function(x, ...) {
  answers_text <- paste(sapply(names(x$answers), function(name) {
    glue::glue('{name}:\n{.format_blank_answers(x$answers[[name]])}')
  }), collapse = '\n\n')

  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Answers

{answers_text}

{.feedback_text(x)}

{.instructor_notes_text(x)}
'))
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_numeric <- function(x, ...) {
  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Answer

{x$answer} [tolerance {x$tolerance}]

{.feedback_text(x)}

{.instructor_notes_text(x)}
'))
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_file_upload <- function(x, ...) {
  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Answer

(File upload)

{.feedback_text(x)}

{.instructor_notes_text(x)}
'))
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_short_answer <- function(x, ...) {
  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Answer

(Short answer)

{.feedback_text(x)}

{.instructor_notes_text(x)}'))
}

.print_multiple_choice_answer <- function(x) {
  if (x$random_order) {
    x$answers <- x$answers[sample(seq_along(x$answers))]
  }

  answers_text <- paste(sapply(x$answers, function(answer) {
    correct_annotation <- if (answer$correct) {
      'x'
    } else if (answer$partial_credit_percentage > 0) {
      'p'
    } else {
      ' '
    }
    glue::glue('- [{correct_annotation}] {.indent_lines(answer$text, 10, TRUE)}')
  }), collapse = '\n')
  .trim_with_newline(glue::glue('
# {x$title}

## Question

{x$question_text}

## Answers

{answers_text}

[x] = correct answer
[p] = partial credit given

{.feedback_text(x)}

{.instructor_notes_text(x)}
  '))
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_multiple_choice <- function(x, ...) {
  .print_multiple_choice_answer(x)
}

#' @rdname to_markdown.r2bb_question
#' @export
to_markdown.r2bb_question_multiple_answer <- function(x, ...) {
  .print_multiple_choice_answer(x)
}

.indent_lines <- function(text, depth, omit_first = FALSE) {
  indent <- paste(rep(' ', depth), collapse = '')
  lines <- strsplit(text, '\n')[[1]]
  paste(sapply(seq_along(lines), function(i) {
    if (i == 1 && omit_first) {
      return(lines[i])
    }
    paste(indent, lines[i], sep = '')
  }), collapse = '\n')
}


.feedback_text <- function(x) {
  if (x$positive_feedback == '' && x$negative_feedback == '') {
    return('')
  } else if (x$positive_feedback == x$negative_feedback) {
    just_one <- TRUE
    title <- 'Feedback'
    text <- x$positive_feedback
  } else if (x$positive_feedback == '') {
    just_one <- TRUE
    title <- 'Negative feedback'
    text <- x$negative_feedback
  } else if (x$negative_feedback == '') {
    just_one <- TRUE
    title <- 'Positive feedback'
    text <- x$positive_feedback
  } else {
    just_one <- FALSE
  }

  if (just_one) {
    glue::glue('
# {title}

{text}
')
  } else {
    glue::glue('
# Positive feedback

{x$positive_feedback}

# Negative feedback

{x$negative_feedback}
')
  }
}

.instructor_notes_text <- function(x) {
  if (x$instructor_notes == '') {
    return('')
  }
  glue::glue('
# Instructor notes

{x$instructor_notes}
')
}
