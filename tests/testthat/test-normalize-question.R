test_that("multiple choice questions normalize", {
  q <- normalize_question(list(
    title = "Capital of France",
    question_type = "multiple_choice",
    question_text = "What is the capital of France?",
    answers = list(
      list(text = "Paris", correct = TRUE),
      list(text = "London", correct = FALSE)
    )
  ))
  expect_equal(q$title, "Capital of France")
  expect_s3_class(q, "r2bb_question_multiple_choice")
  expect_s3_class(q, "r2bb_question")
})

test_that("multiple answer questions normalize", {
  q <- normalize_question(list(
    title = "Primes",
    question_type = "multiple_answer",
    question_text = "Select all prime numbers",
    answers = list(
      list(text = "2", correct = TRUE),
      list(text = "3", correct = TRUE),
      list(text = "4", correct = FALSE)
    )
  ))
  expect_equal(q$title, "Primes")
  expect_s3_class(q, "r2bb_question_multiple_answer")
})

test_that("single blank questions normalize", {
  q <- normalize_question(list(
    title = "Color",
    question_type = "single_blank",
    question_text = "The sky is ____.",
    answers = list("blue")
  ))
  expect_equal(q$title, "Color")
  expect_s3_class(q, "r2bb_question_single_blank")
})

test_that("multiple blanks questions normalize", {
  q <- normalize_question(list(
    title = "Numbers",
    question_type = "multiple_blanks",
    question_text = "A=[A], B=[B]",
    answers = list(A = "1", B = "2")
  ))
  expect_equal(q$title, "Numbers")
  expect_s3_class(q, "r2bb_question_multiple_blanks")
})

test_that("numeric questions normalize", {
  q <- normalize_question(list(
    title = "Simple sum",
    question_type = "numeric",
    question_text = "2 + 2 = ?",
    answer = 4
  ))
  expect_equal(q$title, "Simple sum")
  expect_s3_class(q, "r2bb_question_numeric")
})

test_that("file upload questions normalize", {
  q <- normalize_question(list(
    title = "Upload",
    question_type = "file_upload",
    question_text = "Upload a file"
  ))
  expect_equal(q$title, "Upload")
  expect_s3_class(q, "r2bb_question_file_upload")
})

test_that("short answer questions normalize", {
  q <- normalize_question(list(
    title = "Why",
    question_type = "short_answer",
    question_text = "Explain why 2 + 2 = 4",
    answer = "Because it does"
  ))
  expect_equal(q$title, "Why")
  expect_s3_class(q, "r2bb_question_short_answer")
})

test_that("matching questions normalize", {
  q <- normalize_question(list(
    title = "Match numbers",
    question_type = "matching",
    question_text = "Match numbers to words",
    questions = list(
      list(text = "1", answer_index = 1),
      list(text = "2", answer_index = 2)
    ),
    answers = list("one", "two")
  ))
  expect_equal(q$title, "Match numbers")
  expect_s3_class(q, "r2bb_question_matching")
})

test_that("unknown question types fail", {
  expect_error(
    normalize_question(list(
      title = "Bad",
      question_type = "does_not_exist",
      question_text = "??"
    )),
    "Unknown question type"
  )
})
