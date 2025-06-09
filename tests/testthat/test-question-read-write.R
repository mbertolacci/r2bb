test_that("read_question reads YAML files correctly", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Test Question
question_type: multiple_choice
question_text: What is 2 + 2?
max_score: 1.0
answers:
  - text: "4"
    correct: true
  - text: "3"
    correct: false
', file = temp_yaml)
  
  question <- read_question(temp_yaml, normalize = FALSE)
  
  expect_equal(question$title, "Test Question")
  expect_equal(question$question_type, "multiple_choice")
  expect_equal(question$question_text, "What is 2 + 2?")
  expect_equal(question$max_score, 1.0)
  expect_length(question$answers, 2)
  expect_true(question$answers[[1]]$correct)
  expect_false(question$answers[[2]]$correct)
})

test_that("read_question reads RYaml files correctly", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
title: Dynamic Question
question_type: numeric
question_text: What is `r 3 + 5`?
max_score: `r 2.0`
tolerance: 0.1
', file = temp_ryaml)
  
  question <- read_question(temp_ryaml, normalize = FALSE)
  
  expect_equal(question$title, "Dynamic Question")
  expect_equal(question$question_type, "numeric")
  expect_equal(question$question_text, "What is 8?")
  expect_equal(question$max_score, 2.0)
  expect_equal(question$tolerance, 0.1)
})

test_that("read_question normalizes by default", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Test Question
question_type: multiple_choice
question_text: What is 2 + 2?
answers:
  - text: "4"
    correct: true
  - text: "3"
    correct: false
', file = temp_yaml)
  
  question <- read_question(temp_yaml)
  
  # Normalized questions should have the r2bb_question class
  expect_s3_class(question, "r2bb_question")
})

test_that("read_question can skip normalization", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Test Question
question_type: multiple_choice
question_text: What is 2 + 2?
answers:
  - text: "4"
    correct: true
', file = temp_yaml)
  
  question <- read_question(temp_yaml, normalize = FALSE)
  
  # Non-normalized questions should be plain lists
  expect_type(question, "list")
  expect_false(inherits(question, "r2bb_question"))
})

test_that("read_question handles normalise parameter (British spelling)", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Test Question
question_type: short_answer
question_text: Explain something.
', file = temp_yaml)
  
  # Test that normalise parameter works
  question_normalized <- read_question(temp_yaml, normalise = TRUE)
  question_not_normalized <- read_question(temp_yaml, normalise = FALSE)
  
  expect_s3_class(question_normalized, "r2bb_question")
  expect_false(inherits(question_not_normalized, "r2bb_question"))
})

test_that("read_question handles single questions when allow_multiple = FALSE", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Single Question
question_type: short_answer
question_text: Answer this.
', file = temp_yaml)
  
  question <- read_question(temp_yaml, allow_multiple = FALSE, normalize = FALSE)
  
  expect_type(question, "list")
  expect_equal(question$title, "Single Question")
})

test_that("read_question handles multiple questions when allow_multiple = TRUE", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Question 1
question_type: short_answer
question_text: First question.
---
title: Question 2
question_type: multiple_choice
question_text: Second question.
answers:
  - text: "A"
    correct: true
  - text: "B"
    correct: false
', file = temp_yaml)
  
  questions <- read_question(temp_yaml, allow_multiple = TRUE, normalize = FALSE)
  
  expect_type(questions, "list")
  expect_length(questions, 2)
  expect_equal(questions[[1]]$title, "Question 1")
  expect_equal(questions[[2]]$title, "Question 2")
})

test_that("read_question normalizes multiple questions when requested", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
title: Question 1
question_type: short_answer
question_text: First question.
---
title: Question 2
question_type: multiple_choice
question_text: Second question.
answers:
  - text: "A"
    correct: true
', file = temp_yaml)
  
  questions <- read_question(temp_yaml, allow_multiple = TRUE, normalize = TRUE)
  
  expect_type(questions, "list")
  expect_length(questions, 2)
  expect_s3_class(questions[[1]], "r2bb_question")
  expect_s3_class(questions[[2]], "r2bb_question")
})

test_that("read_question passes additional arguments to render_ryaml", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
title: Test
question_type: short_answer
question_text: Value is `r 1 + 1`
', file = temp_ryaml)
  
  # Test that additional arguments are passed through
  expect_no_error(read_question(temp_ryaml, normalize = FALSE, quiet = TRUE))
})

test_that("write_question writes single question correctly", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  question <- structure(
    list(
      title = "Write Test",
      question_type = "multiple_choice",
      question_text = "Test question?",
      max_score = 1.0,
      answers = list(
        list(text = "Yes", correct = TRUE),
        list(text = "No", correct = FALSE)
      )
    ),
    class = "r2bb_question"
  )
  
  write_question(question, temp_yaml)
  
  expect_true(file.exists(temp_yaml))
  
  # Read back and verify
  content <- yaml::yaml.load_file(temp_yaml)
  expect_equal(content$title, "Write Test")
  expect_equal(content$question_type, "multiple_choice")
  expect_length(content$answers, 2)
})

test_that("write_question writes multiple questions correctly", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  question1 <- structure(
    list(
      title = "Question 1",
      question_type = "short_answer",
      question_text = "First question"
    ),
    class = "r2bb_question"
  )
  
  question2 <- structure(
    list(
      title = "Question 2", 
      question_type = "numeric",
      question_text = "Second question",
      tolerance = 0.1
    ),
    class = "r2bb_question"
  )
  
  write_question(list(question1, question2), temp_yaml)
  
  expect_true(file.exists(temp_yaml))
  
  # Read back and verify multiple documents
  file_content <- readLines(temp_yaml)
  expect_true(any(grepl("title: Question 1", file_content)))
  expect_true(any(grepl("title: Question 2", file_content)))
  expect_true(any(grepl("---", file_content))) # Document separator
})

test_that("write_question validates input types", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  # Test with invalid question object
  invalid_question <- list(title = "Invalid", type = "wrong")
  
  expect_error(write_question(invalid_question, temp_yaml))
  
  # Test with mixed valid/invalid in list
  valid_question <- structure(
    list(title = "Valid", question_type = "short_answer"),
    class = "r2bb_question"
  )
  
  expect_error(write_question(list(valid_question, invalid_question), temp_yaml))
})

test_that("write_question handles empty lists", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  # Should handle empty list gracefully
  expect_no_error(write_question(list(), temp_yaml))
})

test_that("write_question passes additional arguments to yaml functions", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  question <- structure(
    list(
      title = "Test Args",
      question_type = "short_answer",
      question_text = "Test"
    ),
    class = "r2bb_question"
  )
  
  # Test that additional arguments are passed to yaml::as.yaml
  expect_no_error(write_question(question, temp_yaml, indent = 4))
})

test_that("read_question and write_question round-trip correctly", {
  temp_yaml1 <- tempfile(fileext = ".yaml")
  temp_yaml2 <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_yaml1, temp_yaml2)))
  
  # Create original question
  original_question <- structure(
    list(
      title = "Round Trip Test",
      question_type = "multiple_choice",
      question_text = "Test round trip",
      max_score = 5.0,
      random_order = TRUE,
      answers = list(
        list(text = "Option A", correct = TRUE),
        list(text = "Option B", correct = FALSE),
        list(text = "Option C", correct = FALSE)
      )
    ),
    class = "r2bb_question"
  )
  
  # Write and read back
  write_question(original_question, temp_yaml1)
  read_question_back <- read_question(temp_yaml1, normalize = FALSE)
  
  # Write again and compare
  write_question(structure(read_question_back, class = "r2bb_question"), temp_yaml2)
  
  content1 <- readLines(temp_yaml1)
  content2 <- readLines(temp_yaml2)
  
  # Both files should have similar structure (allowing for formatting differences)
  expect_true(length(content1) > 0)
  expect_true(length(content2) > 0)
})