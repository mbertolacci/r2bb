#' Normalize a question
#'
#' This function normalizes a question to the canonical format used by the
#' package, which also performs validation of the question.
#'
#' @param question The question to normalize
#' @return The normalized question
#' @seealso [normalize()]
#' @export
normalize_question <- function(question) {
  if (inherits(question, 'r2bb_question')) {
    # Already normalized
    return(question)
  }

  if (is.null(question$type)) {
    question$type <- 'question'
  }
  stopifnot(question$type == 'question')

  question$max_score <- .normalize_numeric(question$max_score, 1)
  question$partial_credit <- .normalize_logical(question$partial_credit, FALSE)
  question$question_text <- .normalize_string(question$question_text)
  if (is.null(question$positive_feedback)) {
    question$positive_feedback <- question$feedback
  }
  if (is.null(question$negative_feedback)) {
    question$negative_feedback <- question$feedback
  }
  question$positive_feedback <- .normalize_string(question$positive_feedback)
  question$negative_feedback <- .normalize_string(question$negative_feedback)
  question$feedback <- NULL
  question$instructor_notes <- .normalize_string(question$instructor_notes)

  output <- if (question$question_type == 'matching') {
    .normalize_question_matching(question)
  } else if (question$question_type == 'single_blank') {
    .normalize_question_single_blank(question)
  } else if (question$question_type == 'multiple_blanks') {
    .normalize_question_multiple_blanks(question)
  } else if (question$question_type == 'multiple_choice') {
    .normalize_question_multiple_choice(question)
  } else if (question$question_type == 'multiple_answer') {
    .normalize_question_multiple_answer(question)
  } else if (question$question_type == 'numeric') {
    .normalize_question_numeric(question)
  } else if (question$question_type == 'file_upload') {
    .normalize_question_file_upload(question)
  } else if (question$question_type == 'short_answer') {
    .normalize_question_short_answer(question)
  } else {
    stop('Unknown question type: ', question$question_type)
  }
  class(output) <- c(
    sprintf('r2bb_question_%s', question$question_type),
    'r2bb_question',
    class(output)
  )
  output
}

.fill_partial_credit_percentage <- function(i, partial_credit_percentage, partial_credit, n_cases) {
  base <- round(100 / n_cases)
  if (is.null(partial_credit_percentage)) {
    if (partial_credit) {
      partial_credit_percentage <- if (i < n_cases) {
        base
      } else {
        round(100 - (n_cases - 1) * base)
      }
    } else {
      partial_credit_percentage <- 0
    }
  }
  partial_credit_percentage <- as.numeric(partial_credit_percentage)
  if (partial_credit_percentage < 0 || partial_credit_percentage > 100) {
    stop('Partial credit percentage must be between 0 and 100')
  }
  partial_credit_percentage
}

.normalize_question_matching <- function(question) {
  n_questions <- length(question$questions)
  n_answers <- length(question$answers)

  partial_credit_percentage_total <- 0
  for (i in seq_len(n_questions)) {
    question$questions[[i]]$text <- .normalize_string(question$questions[[i]]$text)
    question$questions[[i]]$answer_index <- .normalize_integer(question$questions[[i]]$answer_index, i)
    question$questions[[i]]$partial_credit_percentage <- .fill_partial_credit_percentage(
      i,
      question$questions[[i]]$partial_credit_percentage,
      question$partial_credit,
      n_questions
    )
    partial_credit_percentage_total <- partial_credit_percentage_total + question$questions[[i]]$partial_credit_percentage
  }
  if (question$partial_credit && partial_credit_percentage_total != 100) {
    print(partial_credit_percentage_total)
    stop('Total partial credit percentage is not 100')
  }

  for (i in seq_len(n_answers)) {
    question$answers[[i]] <- .normalize_string(question$answers[[i]])
  }

  question
}

.transform_answers <- function(answers) {
  lapply(answers, function(answer) {
    if (!is.list(answer)) {
      answer <- list(text = answer)
    }
    answer$type <- .normalize_string(answer$type, 'exact')
    answer$case_sensitive <- .normalize_logical(answer$case_sensitive, FALSE)
    answer$text <- .normalize_string(answer$text)
    answer$type <- match.arg(answer$type, c('exact', 'contains', 'pattern'))
    answer
  })
}

.normalize_question_single_blank <- function(question) {
  question$answers <- .transform_answers(question$answers)
  question
}

.normalize_question_multiple_blanks <- function(question) {
  question$answers <- lapply(question$answers, .transform_answers)
  question
}

.normalize_question_multiple_choice <- function(question) {
  question$random_order <- .normalize_logical(question$random_order, FALSE)
  question$answer_numbering <- .normalize_string(question$answer_numbering, 'none')
  question$answer_orientation <- .normalize_string(question$answer_orientation, 'vertical')

  found_correct_answer <- FALSE
  for (i in seq_along(question$answers)) {
    if (!is.list(question$answers[[i]])) {
      question$answers[[i]] <- list(text = question$answers[[i]])
    }
    question$answers[[i]]$text <- .normalize_string(question$answers[[i]]$text)
    question$answers[[i]]$correct <- .normalize_logical(question$answers[[i]]$correct, FALSE)
    question$answers[[i]]$partial_credit_percentage <- .normalize_numeric(
      question$answers[[i]]$partial_credit_percentage,
      if (question$answers[[i]]$correct) 100 else 0
    )

    if (question$answers[[i]]$correct) {
      if (found_correct_answer) {
        stop('Multiple correct answers')
      }
      if (question$answers[[i]]$partial_credit_percentage != 100) {
        stop('Partial credit percentage must be 100 for correct answers')
      }
      found_correct_answer <- TRUE
    }

    if (question$answers[[i]]$partial_credit_percentage < 0 || question$answers[[i]]$partial_credit_percentage > 100) {
      stop('Partial credit percentage must be between 0 and 100')
    }
  }
  if (!found_correct_answer) {
    stop('No correct answer')
  }
  question
}

.normalize_question_multiple_answer <- function(question) {
  question$random_order <- .normalize_logical(question$random_order, FALSE)
  question$answer_numbering <- .normalize_string(question$answer_numbering, 'none')
  question$answer_orientation <- .normalize_string(question$answer_orientation, 'vertical')

  found_correct_answer <- FALSE
  for (i in seq_along(question$answers)) {
    if (!is.list(question$answers[[i]])) {
      question$answers[[i]] <- list(text = question$answers[[i]])
    }

    question$answers[[i]]$text <- .normalize_string(question$answers[[i]]$text)
    question$answers[[i]]$correct <- .normalize_logical(question$answers[[i]]$correct, FALSE)
    if (question$answers[[i]]$correct) {
      found_correct_answer <- TRUE
    }
  }

  n_correct_answers <- sum(sapply(question$answers, function(x) x$correct))
  total_partial_credit_percentage <- 0
  index_correct <- 1L
  for (i in seq_along(question$answers)) {
    if (question$answers[[i]]$correct) {
      partial_credit_percentage <- .fill_partial_credit_percentage(
        index_correct,
        question$answers[[i]]$partial_credit_percentage,
        question$partial_credit,
        n_correct_answers
      )
      index_correct <- index_correct + 1L
      # NOTE: the total rule only applies to correct answers
      total_partial_credit_percentage <- total_partial_credit_percentage + partial_credit_percentage
    } else {
      partial_credit_percentage <- .normalize_numeric(
        question$answers[[i]]$partial_credit_percentage,
        0
      )
    }
    if (partial_credit_percentage < 0 || partial_credit_percentage > 100) {
      stop('Partial credit percentage must be between 0 and 100')
    }
    question$answers[[i]]$partial_credit_percentage <- partial_credit_percentage
  }
  if (!found_correct_answer) {
    stop('No correct answer')
  }
  if (question$partial_credit && total_partial_credit_percentage != 100) {
    stop('Total partial credit percentage is not 100')
  }
  question
}

.normalize_question_numeric <- function(question) {
  question$answer <- .normalize_numeric(question$answer)
  question$tolerance <- .normalize_numeric(question$tolerance)
  question
}

.normalize_question_file_upload <- function(question) {
  question
}

.normalize_question_short_answer <- function(question) {
  question$answer <- .normalize_string(question$answer)
  question$answer_rows <- .normalize_integer(question$answer_rows, 3L)
  question
}
