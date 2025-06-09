
test_that("read_test reads YAML files correctly", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: test
title: Test Assessment
description: A test assessment for testing
instructions: Follow the instructions carefully
contents:
  - type: question
    points: 2
    question:
      title: Question 1
      question_type: multiple_choice
      question_text: What is 2 + 2?
      answers:
        - text: "4"
          correct: true
        - text: "3"
          correct: false
  - type: question
    points: 5
    question:
      title: Question 2
      question_type: short_answer
      question_text: Explain something
', file = temp_yaml)
  
  test <- read_test(temp_yaml, normalize = FALSE)
  
  expect_equal(test$type, "test")
  expect_equal(test$title, "Test Assessment")
  expect_equal(test$description, "A test assessment for testing")
  expect_equal(test$instructions, "Follow the instructions carefully")
  expect_length(test$contents, 2)
  expect_equal(test$contents[[1]]$type, "question")
  expect_equal(test$contents[[1]]$points, 2)
  expect_equal(test$contents[[2]]$type, "question")
  expect_equal(test$contents[[2]]$points, 5)
})

test_that("read_test reads RYaml files correctly", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
type: test
title: Dynamic Test `r 1 + 1`
description: Test with R code
instructions: Dynamic instructions
contents:
  - type: question
    points: `r 2 + 1`
    question:
      title: Question `r 1`
      question_type: numeric
      question_text: What is `r 3 + 5`?
      tolerance: 0.1
      answers:
        - `r 3 + 5`
', file = temp_ryaml)
  
  test <- read_test(temp_ryaml, normalize = FALSE)
  
  expect_equal(test$type, "test")
  expect_equal(test$title, "Dynamic Test 2")
  expect_equal(test$contents[[1]]$points, 3)
  expect_equal(test$contents[[1]]$question$title, "Question 1")
  expect_equal(test$contents[[1]]$question$question_text, "What is 8?")
})

test_that("read_test normalizes by default", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: test
title: Normalize Test
description: Test normalization
contents:
  - type: question
    points: 1
    question:
      title: Test Question
      question_type: short_answer
      question_text: Test?
', file = temp_yaml)
  
  test <- read_test(temp_yaml)
  
  expect_s3_class(test, "r2bb_test")
  expect_s3_class(test$contents[[1]]$question, "r2bb_question")
})

test_that("read_test can skip normalization", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: test
title: No Normalize Test
description: Test no normalization
contents:
  - type: question
    points: 1
    question:
      title: Test Question
      question_type: short_answer
      question_text: Test?
', file = temp_yaml)
  
  test <- read_test(temp_yaml, normalize = FALSE)
  
  expect_type(test, "list")
  expect_false(inherits(test, "r2bb_test"))
  expect_false(inherits(test$contents[[1]]$question, "r2bb_question"))
})

test_that("read_test handles normalise parameter (British spelling)", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: test
title: British Spelling Test
description: Test normalise
contents: []
', file = temp_yaml)
  
  test_normalized <- read_test(temp_yaml, normalise = TRUE)
  test_not_normalized <- read_test(temp_yaml, normalise = FALSE)
  
  expect_s3_class(test_normalized, "r2bb_test")
  expect_false(inherits(test_not_normalized, "r2bb_test"))
})

test_that("read_test handles multiple tests when allow_multiple = TRUE", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: test
title: Test 1
description: First test
contents: []
---
type: test
title: Test 2
description: Second test
contents: []
', file = temp_yaml)
  
  tests <- read_test(temp_yaml, allow_multiple = TRUE, normalize = FALSE)
  
  expect_type(tests, "list")
  expect_length(tests, 2)
  expect_equal(tests[[1]]$title, "Test 1")
  expect_equal(tests[[2]]$title, "Test 2")
})

test_that("read_test passes additional arguments to render_ryaml", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
type: test
title: Args Test
description: Test arguments
contents: []
', file = temp_ryaml)
  
  expect_no_error(read_test(temp_ryaml, normalize = FALSE, quiet = TRUE))
})

test_that("normalize_test works correctly", {
  test_data <- list(
    type = "test",
    title = "Normalization Test",
    description = "Test normalization",
    instructions = "Follow these instructions",
    contents = list(
      list(
        type = "question",
        points = 2,
        question = list(
          title = "Question 1",
          question_type = "multiple_choice",
          question_text = "Test question",
          answers = list(
            list(text = "Answer", correct = TRUE)
          )
        )
      )
    )
  )
  
  normalized_test <- normalize_test(test_data)
  
  expect_s3_class(normalized_test, "r2bb_test")
  expect_equal(normalized_test$title, "Normalization Test")
  expect_equal(normalized_test$description, "Test normalization")
  expect_equal(normalized_test$instructions, "Follow these instructions")
  expect_s3_class(normalized_test$contents[[1]]$question, "r2bb_question")
  expect_equal(normalized_test$contents[[1]]$points, 2)
})

test_that("normalize_test handles already normalized tests", {
  test_data <- list(
    type = "test",
    title = "Already Normalized",
    description = "Test",
    instructions = "Test",
    contents = list()
  )
  class(test_data) <- "r2bb_test"
  
  result <- normalize_test(test_data)
  
  expect_identical(result, test_data)
})

test_that("normalize_test validates test type", {
  invalid_test <- list(type = "not_test", title = "Invalid")
  
  expect_error(normalize_test(invalid_test))
})

test_that("normalize_test handles missing type", {
  test_data <- list(
    title = "Missing Type",
    description = "Test",
    contents = list()
  )
  
  normalized_test <- normalize_test(test_data)
  
  expect_equal(normalized_test$type, "test")
  expect_s3_class(normalized_test, "r2bb_test")
})

test_that("normalize_test handles random_block content", {
  pool_data <- list(
    type = "pool",
    title = "Test Pool",
    description = "Pool for testing",
    questions = list(
      list(
        title = "Pool Question",
        question_type = "short_answer",
        question_text = "Pool question"
      )
    )
  )
  
  test_data <- list(
    type = "test",
    title = "Random Block Test",
    description = "Test random blocks",
    contents = list(
      list(
        type = "random_block",
        pool = pool_data,
        questions_to_display = 2,
        points_per_question = 3
      )
    )
  )
  
  normalized_test <- normalize_test(test_data)
  
  expect_s3_class(normalized_test, "r2bb_test")
  expect_s3_class(normalized_test$contents[[1]]$pool, "r2bb_pool")
  expect_equal(normalized_test$contents[[1]]$pool$questions_to_display, 2)
  expect_equal(normalized_test$contents[[1]]$pool$points_per_question, 3)
})

test_that("normalize_test handles missing content fields", {
  test_data <- list(
    type = "test",
    title = "Missing Contents",
    description = "Test"
  )
  
  normalized_test <- normalize_test(test_data)
  
  expect_equal(length(normalized_test$contents), 0)
})

test_that("normalize_test handles null content", {
  test_data <- list(
    type = "test",
    title = "Null Content",
    description = "Test",
    contents = list(NULL)
  )
  
  expect_error(normalize_test(test_data), "Content type is null")
})

test_that("normalize_test normalizes string fields", {
  test_data <- list(
    type = "test",
    title = "  Trimmed Title  ",
    description = NULL,
    instructions = "  Trimmed Instructions  ",
    contents = list()
  )
  
  normalized_test <- normalize_test(test_data)
  
  expect_equal(normalized_test$title, "Trimmed Title")
  expect_equal(normalized_test$description, "")
  expect_equal(normalized_test$instructions, "Trimmed Instructions")
})

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

test_that("to_bbxml.r2bb_test returns valid XML string", {
  test <- normalize_test(list(
    title = "XML Test",
    description = "Test for **XML** generation",
    instructions = "Follow the *instructions*",
    contents = list(
      list(
        type = "question",
        points = 2,
        question = list(
          title = "Test Question",
          question_type = "multiple_choice",
          question_text = "What is 2 + 2?",
          answers = list(
            list(text = "4", correct = TRUE),
            list(text = "3", correct = FALSE)
          )
        )
      )
    )
  ))
  
  xml_output <- to_bbxml(test)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("XML Test", xml_output))
  expect_true(grepl("<strong>XML</strong>", xml_output))
  expect_true(grepl("<em>instructions</em>", xml_output))
})

test_that("to_bbxml.r2bb_test handles random_block content", {
  pool <- normalize_pool(list(
    title = "Test Pool",
    description = "Pool for testing",
    questions = list(
      list(
        title = "Pool Question",
        question_type = "short_answer",
        question_text = "Pool question"
      )
    )
  ))
  
  test <- normalize_test(list(
    title = "Random Block Test",
    description = "Test with random block",
    contents = list(
      list(
        type = "random_block",
        pool = pool,
        questions_to_display = 2,
        points_per_question = 3
      )
    )
  ))
  
  xml_output <- to_bbxml(test)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Random Block Test", xml_output))
  
  # Check that pool resource names are attached as attributes
  pool_resource_names <- attr(xml_output, "pool_resource_names")
  expect_length(pool_resource_names, 1)
  expect_true(grepl("res\\d+", pool_resource_names[1]))
})

test_that("to_bbxml.r2bb_test handles mixed content types", {
  pool <- normalize_pool(list(
    title = "Mixed Pool",
    description = "Pool",
    questions = list(
      list(
        title = "Pool Question",
        question_type = "short_answer",
        question_text = "Pool question"
      )
    )
  ))
  
  test <- normalize_test(list(
    title = "Mixed Content Test",
    description = "Test with mixed content",
    contents = list(
      list(
        type = "question",
        points = 1,
        question = list(
          title = "Direct Question",
          question_type = "short_answer",
          question_text = "Direct question"
        )
      ),
      list(
        type = "random_block",
        pool = pool,
        questions_to_display = 1,
        points_per_question = 2
      )
    )
  ))
  
  xml_output <- to_bbxml(test)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Mixed Content Test", xml_output))
  
  # Should have one pool resource name for the random block
  pool_resource_names <- attr(xml_output, "pool_resource_names")
  expect_length(pool_resource_names, 1)
})

test_that("to_bbxml.r2bb_test handles convert_rich_text parameter", {
  test <- normalize_test(list(
    title = "Rich Text Test",
    description = "Test **bold** text",
    instructions = "Test *italic* text",
    contents = list(
      list(
        type = "question",
        points = 1,
        question = list(
          title = "Question",
          question_type = "short_answer",
          question_text = "Test"
        )
      )
    )
  ))
  
  # Test with default markdown conversion
  xml_default <- to_bbxml(test)
  expect_true(grepl("<strong>bold</strong>", xml_default))
  expect_true(grepl("<em>italic</em>", xml_default))
  
  # Test with no conversion
  xml_no_convert <- to_bbxml(test, convert_rich_text = FALSE)
  expect_true(grepl("\\*\\*bold\\*\\*", xml_no_convert))
  expect_true(grepl("\\*italic\\*", xml_no_convert))
})

test_that("to_bbxml.r2bb_test generates unique IDs", {
  test <- normalize_test(list(
    title = "ID Test",
    description = "Test IDs",
    contents = list(
      list(
        type = "question",
        points = 1,
        question = list(
          title = "Question",
          question_type = "short_answer",
          question_text = "Test"
        )
      )
    )
  ))
  
  xml1 <- to_bbxml(test)
  xml2 <- to_bbxml(test)
  
  expect_type(xml1, "character")
  expect_type(xml2, "character")
  expect_false(identical(xml1, xml2))
})

test_that("to_bbxml.r2bb_test calculates max_score correctly", {
  test <- normalize_test(list(
    title = "Score Test",
    description = "Test score calculation",
    contents = list(
      list(
        type = "question",
        points = 2,
        question = list(
          title = "Question 1",
          question_type = "short_answer",
          question_text = "Test",
          max_score = 2
        )
      ),
      list(
        type = "question",
        points = 3,
        question = list(
          title = "Question 2",
          question_type = "short_answer",
          question_text = "Test",
          max_score = 3
        )
      )
    )
  ))
  
  xml_output <- to_bbxml(test)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  # The max score should be 5 (2 + 3)
  expect_true(grepl("5", xml_output))
})

test_that("test functions handle special characters", {
  test <- normalize_test(list(
    title = "Special & Characters < Test >",
    description = "Test with <tags> & symbols",
    instructions = "Instructions with \"quotes\"",
    contents = list(
      list(
        type = "question",
        points = 1,
        question = list(
          title = "Question with & symbols",
          question_type = "short_answer",
          question_text = "Test <question>?"
        )
      )
    )
  ))
  
  # Should handle special characters without errors
  expect_no_error(to_bbxml(test))
})

test_that("test functions pass additional arguments", {
  test <- normalize_test(list(
    title = "Args Test",
    description = "Test",
    instructions = "Test",
    contents = list(
      list(
        type = "question",
        points = 1,
        question = list(
          title = "Question",
          question_type = "short_answer",
          question_text = "Test"
        )
      )
    )
  ))
  
  # Test that additional arguments don't cause errors
  expect_no_error(to_bbxml(test, some_extra_arg = "value"))
})

