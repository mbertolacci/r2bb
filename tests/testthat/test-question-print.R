test_that("print.r2bb_question outputs markdown format by default", {
  # Create a sample question object (normalized)
  question <- normalize_question(list(
    title = "Test Question",
    question_type = "multiple_choice",
    question_text = "What is 2 + 2?",
    max_score = 1.0,
    answers = list(
      list(text = "4", correct = TRUE),
      list(text = "3", correct = FALSE),
      list(text = "5", correct = FALSE)
    )
  ))
  
  # Capture print output with default format (markdown)
  output <- capture.output(print(question))
  
  # Should contain markdown-formatted content
  expect_true(any(grepl("Test Question", output)))
  expect_true(any(grepl("What is 2 \\+ 2\\?", output)))
})

test_that("print.r2bb_question outputs markdown format when explicitly specified", {
  question <- normalize_question(list(
    title = "Sample Question",
    question_type = "short_answer",
    question_text = "Explain photosynthesis.",
    max_score = 5.0
  ))
  
  output <- capture.output(print(question, format = "markdown"))
  
  # Should contain the question content
  expect_true(any(grepl("Sample Question", output)))
  expect_true(any(grepl("photosynthesis", output)))
})

test_that("print.r2bb_question outputs yaml format when specified", {
  question <- normalize_question(list(
    title = "YAML Test",
    question_type = "numeric",
    question_text = "What is pi rounded to 2 decimal places?",
    max_score = 2.0,
    tolerance = 0.01,
    answers = list(3.14)
  ))
  
  output <- capture.output(print(question, format = "yaml"))
  
  # Should contain YAML-formatted content
  expect_true(any(grepl("title: YAML Test", output)))
  expect_true(any(grepl("question_type: numeric", output)))
  expect_true(any(grepl("max_score: 2", output)))
  expect_true(any(grepl("tolerance: 0.01", output)))
})

test_that("print.r2bb_question handles invalid format argument", {
  question <- normalize_question(list(
    title = "Test", 
    question_type = "multiple_choice",
    question_text = "Test?",
    answers = list(list(text = "A", correct = TRUE))
  ))
  
  expect_error(print(question, format = "invalid_format"))
})

test_that("print.r2bb_question passes additional arguments to underlying functions", {
  question <- normalize_question(list(
    title = "Test with Args",
    question_type = "multiple_choice",
    question_text = "Test question",
    answers = list(
      list(text = "Answer 1", correct = TRUE),
      list(text = "Answer 2", correct = FALSE)
    )
  ))
  
  # Test that additional arguments are passed (this tests the ... parameter)
  # For YAML format, we can pass yaml-specific arguments
  expect_no_error(capture.output(print(question, format = "yaml", indent = 4)))
})

test_that("print.r2bb_question works with complex question structures", {
  complex_question <- normalize_question(list(
    title = "Complex Question",
    question_type = "multiple_blanks",
    question_text = "Fill in the blanks: [blank1] and [blank2]",
    max_score = 10.0,
    feedback = "Good job!",
    random_order = TRUE,
    answers = list(blank1 = list("hello", "hi"), blank2 = list("world"))
  ))
  
  # Should handle complex nested structures in both formats
  expect_no_error(capture.output(print(complex_question, format = "markdown")))
  expect_no_error(capture.output(print(complex_question, format = "yaml")))
  
  yaml_output <- capture.output(print(complex_question, format = "yaml"))
  expect_true(any(grepl("question_type: multiple_blanks", yaml_output)))
  expect_true(any(grepl("random_order: yes", yaml_output)))
})

test_that("print.r2bb_question handles minimal question objects", {
  minimal_question <- normalize_question(list(
    title = "Minimal",
    question_type = "short_answer",
    question_text = "Answer this."
  ))
  
  # Should work with minimal required fields
  expect_no_error(capture.output(print(minimal_question, format = "markdown")))
  expect_no_error(capture.output(print(minimal_question, format = "yaml")))
  
  yaml_output <- capture.output(print(minimal_question, format = "yaml"))
  expect_true(any(grepl("title: Minimal", yaml_output)))
  expect_true(any(grepl("question_type: short_answer", yaml_output)))
})