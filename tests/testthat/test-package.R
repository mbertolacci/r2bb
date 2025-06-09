test_that("from_bbxml_package imports pool successfully", {
  pool_zip_path <- system.file("bbxml", "test-import-pool.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist
  skip_if_not(file.exists(pool_zip_path), "Pool test file not found")
  
  result <- from_bbxml_package(pool_zip_path)
  
  # Basic structure checks
  expect_type(result, "list")
  expect_true("pools" %in% names(result))
  expect_true("tests" %in% names(result))
  
  # Should have pools but no tests
  expect_length(result$pools, 1)
  expect_length(result$tests, 0)
  
  # Check the pool structure
  pool <- result$pools[[1]]
  expect_s3_class(pool, "r2bb_pool")
  expect_type(pool$title, "character")
  expect_type(pool$description, "character")
  expect_type(pool$instructions, "character")
  expect_type(pool$questions, "list")
  
  # Basic sanity checks on pool content
  expect_true(nchar(pool$title) > 0)
  expect_true(length(pool$questions) > 0)
  
  # Check that questions are properly structured
  for (question in pool$questions) {
    expect_s3_class(question, "r2bb_question")
    expect_type(question$title, "character")
    expect_type(question$question_type, "character")
    expect_type(question$question_text, "character")
    expect_true(nchar(question$title) > 0)
    expect_true(nchar(question$question_type) > 0)
    expect_true(nchar(question$question_text) > 0)
  }
})

test_that("from_bbxml_package imports test successfully", {
  test_zip_path <- system.file("bbxml", "test-import-test.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist
  skip_if_not(file.exists(test_zip_path), "Test test file not found")
  
  result <- from_bbxml_package(test_zip_path)
  
  # Basic structure checks
  expect_type(result, "list")
  expect_true("pools" %in% names(result))
  expect_true("tests" %in% names(result))
  
  # Should have tests (and possibly pools if the test references them)
  expect_true(length(result$tests) >= 1)
  
  # Check the test structure
  test <- result$tests[[1]]
  expect_s3_class(test, "r2bb_test")
  expect_type(test$title, "character")
  expect_type(test$description, "character")
  expect_type(test$instructions, "character")
  expect_type(test$contents, "list")
  
  # Basic sanity checks on test content
  expect_true(nchar(test$title) > 0)
  expect_true(length(test$contents) > 0)
  
  # Check that test contents are properly structured
  for (content in test$contents) {
    expect_type(content$type, "character")
    expect_true(content$type %in% c("question", "random_block"))
    
    if (content$type == "question") {
      expect_s3_class(content$question, "r2bb_question")
      expect_type(content$points, "double")
      expect_true(content$points > 0)
    } else if (content$type == "random_block") {
      expect_true("pool" %in% names(content))
      expect_type(content$questions_to_display, "integer")
      expect_type(content$points_per_question, "double")
      expect_true(content$questions_to_display > 0)
      expect_true(content$points_per_question > 0)
    }
  }
})

test_that("from_bbxml_package handles convert_html_to_markdown parameter", {
  pool_zip_path <- system.file("bbxml", "test-import-pool.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist
  skip_if_not(file.exists(pool_zip_path), "Pool test file not found")
  
  # Test with markdown conversion (default)
  result_markdown <- from_bbxml_package(pool_zip_path, convert_html_to_markdown = TRUE)
  
  # Test without markdown conversion
  result_html <- from_bbxml_package(pool_zip_path, convert_html_to_markdown = FALSE)
  
  # Both should succeed and have the same structure
  expect_type(result_markdown, "list")
  expect_type(result_html, "list")
  expect_length(result_markdown$pools, 1)
  expect_length(result_html$pools, 1)
  
  # Both pools should be valid r2bb_pool objects
  expect_s3_class(result_markdown$pools[[1]], "r2bb_pool")
  expect_s3_class(result_html$pools[[1]], "r2bb_pool")
})

test_that("from_bbxml_package handles nonexistent files gracefully", {
  fake_path <- "nonexistent_file.zip"
  
  expect_error(from_bbxml_package(fake_path))
})

test_that("from_bbxml_package returns correct structure for empty results", {
  # This test would need a minimal valid ZIP with no pools/tests
  # For now, we just verify the basic structure requirements
  pool_zip_path <- system.file("bbxml", "test-import-pool.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist
  skip_if_not(file.exists(pool_zip_path), "Pool test file not found")
  
  result <- from_bbxml_package(pool_zip_path)
  
  # Should always return a list with tests and pools components
  expect_true(is.list(result))
  expect_true("tests" %in% names(result))
  expect_true("pools" %in% names(result))
  expect_true(is.list(result$tests))
  expect_true(is.list(result$pools))
})

test_that("from_bbxml_package preserves question types correctly", {
  pool_zip_path <- system.file("bbxml", "test-import-pool.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist
  skip_if_not(file.exists(pool_zip_path), "Pool test file not found")
  
  result <- from_bbxml_package(pool_zip_path)
  
  # Check that questions have valid question types
  if (length(result$pools) > 0) {
    pool <- result$pools[[1]]
    for (question in pool$questions) {
      expect_true(question$question_type %in% c(
        "multiple_choice", "multiple_answer", "single_blank", "multiple_blanks",
        "numeric", "short_answer", "file_upload", "matching"
      ))
    }
  }
})

test_that("from_bbxml_package preserves score information", {
  test_zip_path <- system.file("bbxml", "test-import-test.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist  
  skip_if_not(file.exists(test_zip_path), "Test test file not found")
  
  result <- from_bbxml_package(test_zip_path)
  
  if (length(result$tests) > 0) {
    test <- result$tests[[1]]
    
    # Check that score information is preserved
    for (content in test$contents) {
      if (content$type == "question") {
        expect_true(is.numeric(content$question$max_score))
        expect_true(content$question$max_score >= 0)
      }
    }
  }
})

test_that("from_bbxml_package round-trip preserves basic structure", {
  pool_zip_path <- system.file("bbxml", "test-import-pool.zip", package = "r2bb")
  
  # Skip test if the file doesn't exist
  skip_if_not(file.exists(pool_zip_path), "Pool test file not found")
  
  # Import the pool
  imported <- from_bbxml_package(pool_zip_path)
  
  # Export it back (if we have pools)
  if (length(imported$pools) > 0) {
    temp_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(temp_zip))
    
    # Export the first pool
    to_bbxml_package(imported$pools[[1]], temp_zip)
    
    # Should create a valid ZIP file
    expect_true(file.exists(temp_zip))
    expect_true(file.size(temp_zip) > 0)
    
    # Import it again
    reimported <- from_bbxml_package(temp_zip)
    
    # Should have the same basic structure
    expect_length(reimported$pools, 1)
    expect_s3_class(reimported$pools[[1]], "r2bb_pool")
    expect_equal(reimported$pools[[1]]$title, imported$pools[[1]]$title)
  }
})

test_that("to_bbxml_package creates ZIP file for simple pool", {
  # Create a simple pool
  pool <- normalize_pool(list(
    title = "Test Package Pool",
    description = "A simple pool for package testing",
    instructions = "Answer the questions",
    questions = list(
      list(
        title = "Simple Question",
        question_type = "multiple_choice",
        question_text = "What is 2 + 2?",
        answers = list(
          list(text = "4", correct = TRUE),
          list(text = "3", correct = FALSE),
          list(text = "5", correct = FALSE)
        )
      )
    )
  ))
  
  # Create temp file for output
  temp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(temp_zip))
  
  # Export to package
  result_path <- to_bbxml_package(pool, temp_zip)
  
  # Check that ZIP file was created
  expect_true(file.exists(temp_zip))
  expect_true(file.size(temp_zip) > 0)
  expect_equal(result_path, temp_zip)
})

test_that("to_bbxml_package creates ZIP file for simple test", {
  # Create a simple test
  test <- normalize_test(list(
    title = "Test Package Test",
    description = "A simple test for package testing",
    instructions = "Complete all questions",
    contents = list(
      list(
        type = "question",
        points = 2,
        question = list(
          title = "Test Question 1",
          question_type = "short_answer",
          question_text = "Explain photosynthesis"
        )
      ),
      list(
        type = "question", 
        points = 3,
        question = list(
          title = "Test Question 2",
          question_type = "multiple_choice",
          question_text = "What is the capital of France?",
          answers = list(
            list(text = "Paris", correct = TRUE),
            list(text = "London", correct = FALSE),
            list(text = "Berlin", correct = FALSE)
          )
        )
      )
    )
  ))
  
  # Create temp file for output
  temp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(temp_zip))
  
  # Export to package
  result_path <- to_bbxml_package(test, temp_zip)
  
  # Check that ZIP file was created
  expect_true(file.exists(temp_zip))
  expect_true(file.size(temp_zip) > 0)
  expect_equal(result_path, temp_zip)
})

test_that("to_bbxml_package creates ZIP file for multiple pools", {
  # Create multiple pools
  pool1 <- normalize_pool(list(
    title = "Pool 1",
    description = "First pool",
    questions = list(
      list(
        title = "Pool 1 Question",
        question_type = "short_answer",
        question_text = "Question from pool 1"
      )
    )
  ))
  
  pool2 <- normalize_pool(list(
    title = "Pool 2", 
    description = "Second pool",
    questions = list(
      list(
        title = "Pool 2 Question",
        question_type = "multiple_choice",
        question_text = "Question from pool 2",
        answers = list(
          list(text = "A", correct = TRUE),
          list(text = "B", correct = FALSE)
        )
      )
    )
  ))
  
  # Create temp file for output
  temp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(temp_zip))
  
  # Export multiple pools to package
  result_path <- to_bbxml_package(list(pool1, pool2), temp_zip)
  
  # Check that ZIP file was created
  expect_true(file.exists(temp_zip))
  expect_true(file.size(temp_zip) > 0)
  expect_equal(result_path, temp_zip)
})

test_that("to_bbxml_package handles convert_rich_text parameter", {
  # Create a pool with markdown content
  pool <- normalize_pool(list(
    title = "Rich Text Pool",
    description = "Pool with **bold** text",
    instructions = "Follow *italic* instructions",
    questions = list(
      list(
        title = "Markdown Question",
        question_type = "short_answer",
        question_text = "Question with **formatting**"
      )
    )
  ))
  
  # Test with default conversion
  temp_zip1 <- tempfile(fileext = ".zip")
  on.exit(unlink(temp_zip1), add = TRUE)
  
  result_path1 <- to_bbxml_package(pool, temp_zip1)
  expect_true(file.exists(temp_zip1))
  expect_true(file.size(temp_zip1) > 0)
  
  # Test with no conversion
  temp_zip2 <- tempfile(fileext = ".zip")
  on.exit(unlink(temp_zip2), add = TRUE)
  
  result_path2 <- to_bbxml_package(pool, temp_zip2, convert_rich_text = FALSE)
  expect_true(file.exists(temp_zip2))
  expect_true(file.size(temp_zip2) > 0)
})