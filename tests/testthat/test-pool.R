test_that("read_pool reads YAML files correctly", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: Test Pool
description: A test pool for testing
instructions: Follow the instructions
questions:
  - title: Question 1
    question_type: multiple_choice
    question_text: What is 2 + 2?
    max_score: 1.0
    answers:
      - text: "4"
        correct: true
      - text: "3"
        correct: false
  - title: Question 2
    question_type: short_answer
    question_text: Explain something
    max_score: 5.0
', file = temp_yaml)
  
  pool <- read_pool(temp_yaml, normalize = FALSE)
  
  expect_equal(pool$type, "pool")
  expect_equal(pool$title, "Test Pool")
  expect_equal(pool$description, "A test pool for testing")
  expect_equal(pool$instructions, "Follow the instructions")
  expect_length(pool$questions, 2)
  expect_equal(pool$questions[[1]]$title, "Question 1")
  expect_equal(pool$questions[[2]]$title, "Question 2")
})

test_that("read_pool reads RYaml files correctly", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
type: pool
title: Dynamic Pool
description: Pool with R code
instructions: Dynamic instructions
questions:
  - title: Question `r 1 + 1`
    question_type: numeric
    question_text: What is `r 3 + 5`?
    max_score: `r 2.0`
    tolerance: 0.1
    answers:
      - `r 3 + 5`
', file = temp_ryaml)
  
  pool <- read_pool(temp_ryaml, normalize = FALSE)
  
  expect_equal(pool$type, "pool")
  expect_equal(pool$title, "Dynamic Pool")
  expect_equal(pool$questions[[1]]$title, "Question 2")
  expect_equal(pool$questions[[1]]$question_text, "What is 8?")
  expect_equal(pool$questions[[1]]$max_score, 2.0)
})

test_that("read_pool normalizes by default", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: Normalize Test
description: Test normalization
questions:
  - title: Test Question
    question_type: multiple_choice
    question_text: Test?
    answers:
      - text: "Yes"
        correct: true
', file = temp_yaml)
  
  pool <- read_pool(temp_yaml)
  
  expect_s3_class(pool, "r2bb_pool")
  expect_s3_class(pool$questions[[1]], "r2bb_question")
})

test_that("read_pool can skip normalization", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: No Normalize Test
description: Test no normalization
questions:
  - title: Test Question
    question_type: short_answer
    question_text: Test?
', file = temp_yaml)
  
  pool <- read_pool(temp_yaml, normalize = FALSE)
  
  expect_type(pool, "list")
  expect_false(inherits(pool, "r2bb_pool"))
  expect_false(inherits(pool$questions[[1]], "r2bb_question"))
})

test_that("read_pool handles normalise parameter (British spelling)", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: British Spelling Test
description: Test normalise
questions:
  - title: Test Question
    question_type: short_answer
    question_text: Test?
', file = temp_yaml)
  
  pool_normalized <- read_pool(temp_yaml, normalise = TRUE)
  pool_not_normalized <- read_pool(temp_yaml, normalise = FALSE)
  
  expect_s3_class(pool_normalized, "r2bb_pool")
  expect_false(inherits(pool_not_normalized, "r2bb_pool"))
})

test_that("read_pool handles single pools when allow_multiple = FALSE", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: Single Pool
description: One pool only
questions:
  - title: Question
    question_type: short_answer
    question_text: Answer this
', file = temp_yaml)
  
  pool <- read_pool(temp_yaml, allow_multiple = FALSE, normalize = FALSE)
  
  expect_type(pool, "list")
  expect_equal(pool$title, "Single Pool")
})

test_that("read_pool handles multiple pools when allow_multiple = TRUE", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: Pool 1
description: First pool
questions:
  - title: Question 1
    question_type: short_answer
    question_text: First question
---
type: pool
title: Pool 2
description: Second pool
questions:
  - title: Question 2
    question_type: multiple_choice
    question_text: Second question
    answers:
      - text: "A"
        correct: true
', file = temp_yaml)
  
  pools <- read_pool(temp_yaml, allow_multiple = TRUE, normalize = FALSE)
  
  expect_type(pools, "list")
  expect_length(pools, 2)
  expect_equal(pools[[1]]$title, "Pool 1")
  expect_equal(pools[[2]]$title, "Pool 2")
})

test_that("read_pool normalizes multiple pools when requested", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
type: pool
title: Pool 1
description: First pool
questions:
  - title: Question 1
    question_type: short_answer
    question_text: First question
---
type: pool
title: Pool 2
description: Second pool
questions:
  - title: Question 2
    question_type: short_answer
    question_text: Second question
', file = temp_yaml)
  
  pools <- read_pool(temp_yaml, allow_multiple = TRUE, normalize = TRUE)
  
  expect_type(pools, "list")
  expect_length(pools, 2)
  expect_s3_class(pools[[1]], "r2bb_pool")
  expect_s3_class(pools[[2]], "r2bb_pool")
})

test_that("read_pool passes additional arguments to render_ryaml", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
type: pool
title: Args Test
description: Test arguments
questions:
  - title: Question
    question_type: short_answer
    question_text: Value is `r 1 + 1`
', file = temp_ryaml)
  
  expect_no_error(read_pool(temp_ryaml, normalize = FALSE, quiet = TRUE))
})

test_that("normalize_pool works correctly", {
  pool_data <- list(
    type = "pool",
    title = "Normalization Test",
    description = "Test pool normalization",
    instructions = "Follow these instructions",
    questions = list(
      list(
        title = "Question 1",
        question_type = "multiple_choice",
        question_text = "Test question",
        answers = list(
          list(text = "Answer", correct = TRUE)
        )
      )
    )
  )
  
  normalized_pool <- normalize_pool(pool_data)
  
  expect_s3_class(normalized_pool, "r2bb_pool")
  expect_equal(normalized_pool$title, "Normalization Test")
  expect_equal(normalized_pool$description, "Test pool normalization")
  expect_equal(normalized_pool$instructions, "Follow these instructions")
  expect_s3_class(normalized_pool$questions[[1]], "r2bb_question")
})

test_that("normalize_pool handles already normalized pools", {
  pool_data <- list(
    type = "pool",
    title = "Already Normalized",
    description = "Test",
    instructions = "Test",
    questions = list()
  )
  class(pool_data) <- "r2bb_pool"
  
  result <- normalize_pool(pool_data)
  
  expect_identical(result, pool_data)
})

test_that("normalize_pool validates pool type", {
  invalid_pool <- list(type = "not_pool", title = "Invalid")
  
  expect_error(normalize_pool(invalid_pool))
})

test_that("normalize_pool handles missing type", {
  pool_data <- list(
    title = "Missing Type",
    description = "Test",
    questions = list()
  )
  
  normalized_pool <- normalize_pool(pool_data)
  
  expect_equal(normalized_pool$type, "pool")
  expect_s3_class(normalized_pool, "r2bb_pool")
})

test_that("normalize_pool normalizes string fields", {
  pool_data <- list(
    type = "pool",
    title = "  Trimmed Title  ",
    description = NULL,
    instructions = "  Trimmed Instructions  ",
    questions = list()
  )
  
  normalized_pool <- normalize_pool(pool_data)
  
  expect_equal(normalized_pool$title, "Trimmed Title")
  expect_equal(normalized_pool$description, "")
  expect_equal(normalized_pool$instructions, "Trimmed Instructions")
})

test_that("to_bbxml.r2bb_pool returns valid XML string", {
  pool <- normalize_pool(list(
    title = "XML Test Pool",
    description = "Test pool for XML generation",
    instructions = "Follow the **instructions**",
    questions = list(
      list(
        title = "Test Question",
        question_type = "multiple_choice",
        question_text = "What is 2 + 2?",
        answers = list(
          list(text = "4", correct = TRUE),
          list(text = "3", correct = FALSE)
        )
      )
    )
  ))
  
  xml_output <- to_bbxml(pool)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("XML Test Pool", xml_output))
  expect_true(grepl("Test pool for XML generation", xml_output))
  expect_true(grepl("<strong>instructions</strong>", xml_output))
})

test_that("to_bbxml.r2bb_pool handles convert_rich_text parameter", {
  pool <- normalize_pool(list(
    title = "Rich Text Test",
    description = "Test **bold** text",
    instructions = "Test *italic* text",
    questions = list(
      list(
        title = "Test Question",
        question_type = "short_answer",
        question_text = "Test question"
      )
    )
  ))
  
  # Test with default markdown conversion
  xml_default <- to_bbxml(pool)
  expect_true(grepl("<strong>bold</strong>", xml_default))
  expect_true(grepl("<em>italic</em>", xml_default))
  
  # Test with no conversion
  xml_no_convert <- to_bbxml(pool, convert_rich_text = FALSE)
  expect_true(grepl("\\*\\*bold\\*\\*", xml_no_convert))
  expect_true(grepl("\\*italic\\*", xml_no_convert))
})

test_that("to_bbxml.r2bb_pool handles convert_rich_text_options", {
  pool <- normalize_pool(list(
    title = "Options Test",
    description = "Test $x^2$",
    instructions = "Math: $y = mx + b$",
    questions = list(
      list(
        title = "Math Question",
        question_type = "short_answer",
        question_text = "Test math"
      )
    )
  ))
  
  xml_output <- to_bbxml(pool, convert_rich_text_options = list(options = "--mathml"))
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
})

test_that("to_bbxml.r2bb_pool generates unique IDs", {
  pool <- normalize_pool(list(
    title = "ID Test",
    description = "Test IDs",
    instructions = "Test",
    questions = list(
      list(
        title = "Question",
        question_type = "short_answer",
        question_text = "Test"
      )
    )
  ))
  
  xml1 <- to_bbxml(pool)
  xml2 <- to_bbxml(pool)
  
  expect_type(xml1, "character")
  expect_type(xml2, "character")
  expect_false(identical(xml1, xml2))
})

test_that("print.r2bb_pool outputs to console", {
  pool <- normalize_pool(list(
    title = "Print Test",
    description = "Test printing",
    instructions = "Test instructions",
    questions = list(
      list(
        title = "Question",
        question_type = "short_answer",
        question_text = "Test question"
      )
    )
  ))
  
  output <- capture.output(print(pool))
  
  expect_true(length(output) > 0)
  expect_true(any(grepl("Print Test", output)))
  expect_true(any(grepl("Test printing", output)))
})

test_that("to_markdown.r2bb_pool returns formatted markdown", {
  pool <- normalize_pool(list(
    title = "Markdown Test Pool",
    description = "Test pool for **markdown** generation",
    instructions = "Follow these *instructions*",
    questions = list(
      list(
        title = "Question 1",
        question_type = "multiple_choice",
        question_text = "What is 2 + 2?",
        answers = list(
          list(text = "4", correct = TRUE),
          list(text = "3", correct = FALSE)
        )
      ),
      list(
        title = "Question 2",
        question_type = "short_answer",
        question_text = "Explain something"
      )
    )
  ))
  
  md_output <- to_markdown(pool)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Markdown Test Pool", md_output))
  expect_true(grepl("## Description", md_output))
  expect_true(grepl("\\*\\*markdown\\*\\*", md_output))
  expect_true(grepl("## Instructions", md_output))
  expect_true(grepl("\\*instructions\\*", md_output))
  expect_true(grepl("## Questions", md_output))
  expect_true(grepl("### Question 1", md_output))
  expect_true(grepl("### Question 2", md_output))
})

test_that("to_markdown.r2bb_pool handles empty fields", {
  pool <- normalize_pool(list(
    title = "Empty Fields Test",
    description = "",
    instructions = "",
    questions = list()
  ))
  
  md_output <- to_markdown(pool)
  
  expect_type(md_output, "character")
  expect_true(grepl("# Empty Fields Test", md_output))
  expect_true(grepl("## Description", md_output))
  expect_true(grepl("## Instructions", md_output))
  expect_true(grepl("## Questions", md_output))
})

test_that("to_markdown.r2bb_pool adjusts question header levels", {
  pool <- normalize_pool(list(
    title = "Header Test",
    description = "Test header adjustment",
    instructions = "Test",
    questions = list(
      list(
        title = "Question with Headers",
        question_type = "short_answer",
        question_text = "Test question",
        positive_feedback = "Good job",
        instructor_notes = "Important notes"
      )
    )
  ))
  
  md_output <- to_markdown(pool)
  
  # Pool should have # headers (with potential leading/trailing whitespace)
  expect_true(grepl("# Header Test", md_output))
  expect_true(grepl("## Description", md_output))
  expect_true(grepl("## Instructions", md_output))
  expect_true(grepl("## Questions", md_output))
  
  # Questions should have ### headers (adjusted from #)
  expect_true(grepl("### Question with Headers", md_output))
  expect_true(grepl("### Positive feedback", md_output))
  expect_true(grepl("### Instructor notes", md_output))
})

test_that("pool functions handle special characters", {
  pool <- normalize_pool(list(
    title = "Special & Characters < Test >",
    description = "Test with <tags> & symbols",
    instructions = "Instructions with \"quotes\"",
    questions = list(
      list(
        title = "Question with & symbols",
        question_type = "short_answer",
        question_text = "Test <question>?"
      )
    )
  ))
  
  # Should handle special characters without errors
  expect_no_error(to_bbxml(pool))
  expect_no_error(to_markdown(pool))
  expect_no_error(capture.output(print(pool)))
})

test_that("pool functions pass additional arguments", {
  pool <- normalize_pool(list(
    title = "Args Test",
    description = "Test",
    instructions = "Test",
    questions = list(
      list(
        title = "Question",
        question_type = "short_answer",
        question_text = "Test"
      )
    )
  ))
  
  # Test that additional arguments don't cause errors
  expect_no_error(to_bbxml(pool, some_extra_arg = "value"))
  expect_no_error(to_markdown(pool, some_extra_arg = "value"))
  expect_no_error(capture.output(print(pool, some_extra_arg = "value")))
})