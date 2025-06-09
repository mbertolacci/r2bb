
test_that("normalize_question works for all supported types", {
  # multiple_choice
  q_mc <- normalize_question(list(
    title = "MC",
    question_type = "multiple_choice",
    question_text = "Choose one",
    answers = list(
      list(text = "A", correct = TRUE),
      list(text = "B", correct = FALSE)
    )
  ))
  expect_s3_class(q_mc, "r2bb_question_multiple_choice")

  # multiple_answer
  q_ma <- normalize_question(list(
    title = "MA",
    question_type = "multiple_answer",
    question_text = "Select all",
    answers = list(
      list(text = "A", correct = TRUE),
      list(text = "B", correct = TRUE),
      list(text = "C", correct = FALSE)
    )
  ))
  expect_s3_class(q_ma, "r2bb_question_multiple_answer")

  # single_blank
  q_sb <- normalize_question(list(
    title = "SB",
    question_type = "single_blank",
    question_text = "Fill in",
    answers = list("answer")
  ))
  expect_s3_class(q_sb, "r2bb_question_single_blank")

  # multiple_blanks
  q_mb <- normalize_question(list(
    title = "MB",
    question_type = "multiple_blanks",
    question_text = "[A] and [B]",
    answers = list(A = list("a"), B = list("b"))
  ))
  expect_s3_class(q_mb, "r2bb_question_multiple_blanks")

  # numeric
  q_num <- normalize_question(list(
    title = "NUM",
    question_type = "numeric",
    question_text = "2+2",
    answer = 4
  ))
  expect_s3_class(q_num, "r2bb_question_numeric")

  # file_upload
  q_fu <- normalize_question(list(
    title = "FU",
    question_type = "file_upload",
    question_text = "Upload"
  ))
  expect_s3_class(q_fu, "r2bb_question_file_upload")

  # short_answer
  q_sa <- normalize_question(list(
    title = "SA",
    question_type = "short_answer",
    question_text = "Explain",
    answer = "Because"
  ))
  expect_s3_class(q_sa, "r2bb_question_short_answer")

  # matching
  q_match <- normalize_question(list(
    title = "MATCH",
    question_type = "matching",
    question_text = "Match",
    questions = list(list(text = "1", answer_index = 1)),
    answers = list("one")
  ))
  expect_s3_class(q_match, "r2bb_question_matching")
})


test_that("normalize_question errors on unknown type", {
  expect_error(
    normalize_question(list(
      title = "Bad",
      question_type = "unknown_type",
      question_text = "?"
    )),
    "Unknown question type"
  )
})

