test_that("normalize_test works", {
  q <- normalize_question(list(
    title = "Capital of France",
    question_type = "multiple_choice",
    question_text = "What is the capital of France?",
    feedback = "Paris is the capital of France.",
    random_order = FALSE,
    answers = list(
      list(text = "Paris", correct = TRUE),
      list(text = "London", correct = FALSE),
      list(text = "Berlin", correct = FALSE),
      list(text = "Madrid", correct = FALSE)
    )
  ))
  t <- normalize_test(list(
    title = "Capitals Test",
    description = "Test about country capitals.",
    instructions = "Answer the questions.",
    contents = list(
      list(type = "question", points = 1, question = q)
    )
  ))
  expect_equal(t$title, "Capitals Test")
  expect_s3_class(t, "r2bb_test")
})
