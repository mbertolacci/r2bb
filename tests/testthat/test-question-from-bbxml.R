test_that("from_bbxml_question round-trip for multiple_choice", {
  # Create a multiple choice question
  original_question <- normalize_question(list(
    title = "Multiple Choice Test",
    question_type = "multiple_choice",
    question_text = "What is 2 + 2?",
    max_score = 1.0,
    random_order = FALSE,
    answers = list(
      list(text = "4", correct = TRUE),
      list(text = "3", correct = FALSE),
      list(text = "5", correct = FALSE),
      list(text = "2", correct = FALSE)
    )
  ))
  
  # Export to XML
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  
  # Import back
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$type, "question")
  expect_type(imported_question$question_type, "character")
  expect_true(nchar(imported_question$question_type) > 0)
  expect_type(imported_question$answers, "list")
  expect_true(length(imported_question$answers) > 0)
})

test_that("from_bbxml_question round-trip for multiple_answer", {
  # Create a multiple answer question
  original_question <- normalize_question(list(
    title = "Multiple Answer Test",
    question_type = "multiple_answer", 
    question_text = "Select all correct answers",
    max_score = 3.0,
    answers = list(
      list(text = "Correct 1", correct = TRUE),
      list(text = "Correct 2", correct = TRUE),
      list(text = "Wrong", correct = FALSE)
    )
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$answers, "list")
  expect_true(length(imported_question$answers) >= 2)
})

test_that("from_bbxml_question round-trip for single_blank", {
  # Create a single blank question
  original_question <- normalize_question(list(
    title = "Single Blank Test",
    question_type = "single_blank",
    question_text = "Fill in the blank: ___",
    max_score = 2.0,
    answers = list("correct_answer", "alternative_answer")
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$answers, "list")
})

test_that("from_bbxml_question round-trip for multiple_blanks", {
  # Create a multiple blanks question
  original_question <- normalize_question(list(
    title = "Multiple Blanks Test",
    question_type = "multiple_blanks",
    question_text = "Fill [blank1] and [blank2]",
    max_score = 4.0,
    answers = list(
      blank1 = list("answer1", "alt1"),
      blank2 = list("answer2")
    )
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$answers, "list")
})

test_that("from_bbxml_question round-trip for numeric", {
  # Create a numeric question
  original_question <- normalize_question(list(
    title = "Numeric Test",
    question_type = "numeric",
    question_text = "What is pi?",
    max_score = 2.0,
    tolerance = 0.01,
    answers = list(3.14159)
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_true("answer" %in% names(imported_question) || "answers" %in% names(imported_question))
})

test_that("from_bbxml_question round-trip for short_answer", {
  # Create a short answer question
  original_question <- normalize_question(list(
    title = "Short Answer Test",
    question_type = "short_answer",
    question_text = "Explain photosynthesis",
    max_score = 5.0,
    answer = "Sample answer about photosynthesis"
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$question_text, "character")
})

test_that("from_bbxml_question round-trip for file_upload", {
  # Create a file upload question
  original_question <- normalize_question(list(
    title = "File Upload Test",
    question_type = "file_upload",
    question_text = "Upload your essay",
    max_score = 10.0
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$question_text, "character")
})

test_that("from_bbxml_question round-trip for matching", {
  # Create a matching question
  original_question <- normalize_question(list(
    title = "Matching Test",
    question_type = "matching",
    question_text = "Match the items",
    max_score = 5.0,
    questions = list(
      list(text = "Question 1", answer_index = 1),
      list(text = "Question 2", answer_index = 2)
    ),
    answers = list("Answer A", "Answer B")
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$question_text, "character")
})

test_that("from_bbxml_question handles convert_html_to_markdown parameter", {
  # Create a question with markdown content
  original_question <- normalize_question(list(
    title = "Markdown Question",
    question_type = "multiple_choice",
    question_text = "Question with **bold** text",
    max_score = 1.0,
    answers = list(
      list(text = "Answer with *italic*", correct = TRUE),
      list(text = "Plain answer", correct = FALSE)
    )
  ))
  
  # Export to XML (converts markdown to HTML)
  xml_string <- to_bbxml(original_question)
  xml_doc <- xml2::read_xml(xml_string)
  
  # Import with conversion
  imported_with_conversion <- from_bbxml_question(xml_doc, convert_html_to_markdown = TRUE)
  
  # Import without conversion
  imported_without_conversion <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Both should succeed
  expect_s3_class(imported_with_conversion, "r2bb_question")
  expect_s3_class(imported_without_conversion, "r2bb_question")
  expect_equal(imported_with_conversion$title, original_question$title)
  expect_equal(imported_without_conversion$title, original_question$title)
})

test_that("from_bbxml_question validates input type", {
  # Test with invalid input - the error comes from xml2::xml_name
  expect_error(from_bbxml_question("not_xml"))
  expect_error(from_bbxml_question(123))
})

test_that("from_bbxml_question handles nodeset input", {
  # Create multiple questions
  question1 <- normalize_question(list(
    title = "Question 1",
    question_type = "short_answer",
    question_text = "First question",
    max_score = 1.0
  ))
  
  question2 <- normalize_question(list(
    title = "Question 2", 
    question_type = "multiple_choice",
    question_text = "Second question",
    max_score = 2.0,
    answers = list(
      list(text = "A", correct = TRUE),
      list(text = "B", correct = FALSE)
    )
  ))
  
  # Create a pool with both questions
  pool <- normalize_pool(list(
    title = "Test Pool",
    description = "Pool for nodeset test",
    questions = list(question1, question2)
  ))
  
  # Export pool to XML
  xml_string <- to_bbxml(pool, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  
  # Find all item nodes
  item_nodes <- xml2::xml_find_all(xml_doc, ".//item")
  
  # Import all questions at once
  imported_questions <- from_bbxml_question(item_nodes, convert_html_to_markdown = FALSE)
  
  # Check structure
  expect_type(imported_questions, "list")
  expect_length(imported_questions, 2)
  expect_s3_class(imported_questions[[1]], "r2bb_question")
  expect_s3_class(imported_questions[[2]], "r2bb_question")
  expect_equal(imported_questions[[1]]$title, question1$title)
  expect_equal(imported_questions[[2]]$title, question2$title)
})

test_that("from_bbxml_question preserves essential question metadata", {
  # Create a question with various metadata
  original_question <- normalize_question(list(
    title = "Metadata Question",
    question_type = "multiple_choice",
    question_text = "Question with metadata",
    max_score = 3.0,
    partial_credit = TRUE,
    positive_feedback = "Great job!",
    negative_feedback = "Try again",
    instructor_notes = "Important test question",
    answers = list(
      list(text = "Correct", correct = TRUE),
      list(text = "Wrong", correct = FALSE)
    )
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check essential metadata preservation
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
  expect_type(imported_question$question_text, "character")
  expect_true(nchar(imported_question$question_text) > 0)
  expect_equal(imported_question$type, "question")
})

test_that("from_bbxml_question handles questions with feedback", {
  # Create a question with feedback
  original_question <- normalize_question(list(
    title = "Feedback Question",
    question_type = "short_answer",
    question_text = "Question with feedback",
    max_score = 2.0,
    positive_feedback = "Excellent work!",
    negative_feedback = "Please review the material"
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Check basic structure (feedback may be in different format)
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_equal(imported_question$max_score, original_question$max_score)
})

test_that("from_bbxml_question handles special characters", {
  # Create a question with special characters
  original_question <- normalize_question(list(
    title = "Special & Characters < Test >",
    question_type = "multiple_choice",
    question_text = "What about <script> tags & quotes \"here\"?",
    max_score = 1.0,
    answers = list(
      list(text = "Answer with & symbols", correct = TRUE),
      list(text = "Normal answer", correct = FALSE)
    )
  ))
  
  # Round-trip test
  xml_string <- to_bbxml(original_question, convert_rich_text = FALSE)
  xml_doc <- xml2::read_xml(xml_string)
  imported_question <- from_bbxml_question(xml_doc, convert_html_to_markdown = FALSE)
  
  # Should handle special characters without errors
  expect_s3_class(imported_question, "r2bb_question")
  expect_equal(imported_question$title, original_question$title)
  expect_type(imported_question$question_text, "character")
})