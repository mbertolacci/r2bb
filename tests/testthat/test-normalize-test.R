
test_that("normalize_test works with all question types", {
  q_mc <- normalize_question(list(
    title = "MC",
    question_type = "multiple_choice",
    question_text = "Choose one",
    answers = list(
      list(text = "A", correct = TRUE),
      list(text = "B", correct = FALSE)
    )
  ))
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
  q_sb <- normalize_question(list(
    title = "SB",
    question_type = "single_blank",
    question_text = "Fill in",
    answers = list("answer")
  ))
  q_mb <- normalize_question(list(
    title = "MB",
    question_type = "multiple_blanks",
    question_text = "[A] and [B]",
    answers = list(A = list("a"), B = list("b"))
  ))
  q_num <- normalize_question(list(
    title = "NUM",
    question_type = "numeric",
    question_text = "2+2",
    answer = 4
  ))
  q_fu <- normalize_question(list(
    title = "FU",
    question_type = "file_upload",
    question_text = "Upload"
  ))
  q_sa <- normalize_question(list(
    title = "SA",
    question_type = "short_answer",
    question_text = "Explain",
    answer = "Because"
  ))
  q_match <- normalize_question(list(
    title = "MATCH",
    question_type = "matching",
    question_text = "Match",
    questions = list(list(text = "1", answer_index = 1)),
    answers = list("one")
  ))

  t <- normalize_test(list(
    title = "All Qs",
    description = "desc",
    instructions = "inst",
    contents = list(
      list(type = "question", points = 1, question = q_mc),
      list(type = "question", points = 1, question = q_ma),
      list(type = "question", points = 1, question = q_sb),
      list(type = "question", points = 1, question = q_mb),
      list(type = "question", points = 1, question = q_num),
      list(type = "question", points = 1, question = q_fu),
      list(type = "question", points = 1, question = q_sa),
      list(type = "question", points = 1, question = q_match)
    )
  ))
  expect_s3_class(t, "r2bb_test")
  expect_length(t$contents, 8)
})

