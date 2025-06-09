test_that("to_bbxml.r2bb_question_matching returns valid XML string", {
  question <- normalize_question(list(
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
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Matching Test", xml_output))
  expect_true(grepl("Match the items", xml_output))
})

test_that("to_bbxml.r2bb_question_single_blank returns valid XML string", {
  question <- normalize_question(list(
    title = "Single Blank Test",
    question_type = "single_blank",
    question_text = "Fill in the blank: ___",
    max_score = 2.0,
    answers = list("correct_answer", "another_answer")
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Single Blank Test", xml_output))
  expect_true(grepl("Fill in the blank", xml_output))
})

test_that("to_bbxml.r2bb_question_multiple_blanks returns valid XML string", {
  question <- normalize_question(list(
    title = "Multiple Blanks Test",
    question_type = "multiple_blanks",
    question_text = "Fill [blank1] and [blank2]",
    max_score = 4.0,
    answers = list(
      blank1 = list("answer1", "alt1"),
      blank2 = list("answer2")
    )
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Multiple Blanks Test", xml_output))
  expect_true(grepl("Fill.*and", xml_output))
})

test_that("to_bbxml.r2bb_question_multiple_choice returns valid XML string", {
  question <- normalize_question(list(
    title = "Multiple Choice Test",
    question_type = "multiple_choice",
    question_text = "What is 2 + 2?",
    max_score = 1.0,
    random_order = TRUE,
    answers = list(
      list(text = "4", correct = TRUE),
      list(text = "3", correct = FALSE),
      list(text = "5", correct = FALSE)
    )
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Multiple Choice Test", xml_output))
  expect_true(grepl("What is 2 \\+ 2", xml_output))
  expect_true(grepl("shuffle.*Yes", xml_output))
})

test_that("to_bbxml.r2bb_question_multiple_answer returns valid XML string", {
  question <- normalize_question(list(
    title = "Multiple Answer Test",
    question_type = "multiple_answer",
    question_text = "Select all correct answers",
    max_score = 3.0,
    random_order = FALSE,
    answers = list(
      list(text = "Correct 1", correct = TRUE),
      list(text = "Correct 2", correct = TRUE),
      list(text = "Wrong", correct = FALSE)
    )
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Multiple Answer Test", xml_output))
  expect_true(grepl("Select all correct", xml_output))
  expect_true(grepl("shuffle.*No", xml_output))
})

test_that("to_bbxml.r2bb_question_numeric returns valid XML string", {
  question <- normalize_question(list(
    title = "Numeric Test",
    question_type = "numeric",
    question_text = "What is pi?",
    max_score = 2.0,
    tolerance = 0.01,
    answers = list(3.14159)
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Numeric Test", xml_output))
  expect_true(grepl("What is pi", xml_output))
  expect_true(grepl("3.14", xml_output))
})

test_that("to_bbxml.r2bb_question_file_upload returns valid XML string", {
  question <- normalize_question(list(
    title = "File Upload Test",
    question_type = "file_upload",
    question_text = "Upload your essay",
    max_score = 10.0
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("File Upload Test", xml_output))
  expect_true(grepl("Upload your essay", xml_output))
})

test_that("to_bbxml.r2bb_question_short_answer returns valid XML string", {
  question <- normalize_question(list(
    title = "Short Answer Test",
    question_type = "short_answer",
    question_text = "Explain photosynthesis",
    max_score = 5.0,
    answer = "Sample answer about photosynthesis"
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Short Answer Test", xml_output))
  expect_true(grepl("Explain photosynthesis", xml_output))
})

test_that("to_bbxml functions handle convert_rich_text parameter", {
  question <- normalize_question(list(
    title = "Rich Text Test",
    question_type = "multiple_choice",
    question_text = "**Bold** text with *italics*",
    max_score = 1.0,
    answers = list(
      list(text = "Answer with *emphasis*", correct = TRUE),
      list(text = "Plain answer", correct = FALSE)
    )
  ))
  
  # Test with default markdown conversion
  xml_default <- to_bbxml(question)
  expect_type(xml_default, "character")
  expect_true(grepl("<strong>Bold</strong>", xml_default))
  
  # Test with no conversion
  xml_no_convert <- to_bbxml(question, convert_rich_text = FALSE)
  expect_type(xml_no_convert, "character")
  expect_true(grepl("\\*\\*Bold\\*\\*", xml_no_convert))
})

test_that("to_bbxml functions handle convert_rich_text_options parameter", {
  question <- normalize_question(list(
    title = "Math Test",
    question_type = "short_answer",
    question_text = "What is $x^2 + y^2$?",
    max_score = 2.0,
    answer = "Pythagorean theorem"
  ))
  
  # Test with custom options
  xml_output <- to_bbxml(question, convert_rich_text_options = list(options = "--mathml"))
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Math Test", xml_output))
})

test_that("to_bbxml functions handle feedback fields", {
  question <- normalize_question(list(
    title = "Feedback Test",
    question_type = "multiple_choice",
    question_text = "Test question",
    max_score = 1.0,
    positive_feedback = "**Great job!**",
    negative_feedback = "*Try again*",
    instructor_notes = "This tests basic concepts",
    answers = list(
      list(text = "Correct", correct = TRUE),
      list(text = "Wrong", correct = FALSE)
    )
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(grepl("Great job", xml_output))
  expect_true(grepl("Try again", xml_output))
  expect_true(grepl("This tests basic concepts", xml_output))
})

test_that("to_bbxml functions handle partial credit settings", {
  question <- normalize_question(list(
    title = "Partial Credit Test",
    question_type = "multiple_choice",
    question_text = "Test partial credit",
    max_score = 5.0,
    partial_credit = TRUE,
    answers = list(
      list(text = "Best answer", correct = TRUE),
      list(text = "Partial answer", correct = FALSE)
    )
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  # Just check that the XML is generated successfully with partial credit setting
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Partial Credit Test", xml_output))
})

test_that("to_bbxml functions generate unique IDs", {
  question <- normalize_question(list(
    title = "ID Test",
    question_type = "multiple_choice",
    question_text = "Test unique IDs",
    max_score = 1.0,
    answers = list(
      list(text = "A", correct = TRUE),
      list(text = "B", correct = FALSE)
    )
  ))
  
  # Generate XML twice
  xml1 <- to_bbxml(question)
  xml2 <- to_bbxml(question)
  
  expect_type(xml1, "character")
  expect_type(xml2, "character")
  
  # Extract IDs from both (they should be different)
  expect_false(identical(xml1, xml2))
})

test_that("to_bbxml functions handle special characters in text", {
  question <- normalize_question(list(
    title = "Special & Characters < Test >",
    question_type = "multiple_choice",
    question_text = "What about <script> tags & quotes \"here\"?",
    max_score = 1.0,
    answers = list(
      list(text = "Answer with & symbols", correct = TRUE),
      list(text = "Normal answer", correct = FALSE)
    )
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  # XML should be generated without errors even with special characters
})

test_that("to_bbxml functions handle empty or minimal content", {
  # Test with minimal required fields
  question <- normalize_question(list(
    title = "Minimal",
    question_type = "short_answer",
    question_text = "?"
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Minimal", xml_output))
})

test_that("to_bbxml functions handle numeric precision correctly", {
  question <- normalize_question(list(
    title = "Precision Test",
    question_type = "numeric",
    question_text = "Very precise number",
    max_score = 1.0,
    tolerance = 0.0001,
    answers = list(3.1415926535)
  ))
  
  xml_output <- to_bbxml(question)
  
  expect_type(xml_output, "character")
  expect_true(nchar(xml_output) > 0)
  expect_true(grepl("Precision Test", xml_output))
  expect_true(grepl("3.141", xml_output))
})

test_that("to_bbxml functions pass additional arguments correctly", {
  question <- normalize_question(list(
    title = "Args Test",
    question_type = "multiple_choice",
    question_text = "Test arguments",
    max_score = 1.0,
    answers = list(
      list(text = "Answer", correct = TRUE)
    )
  ))
  
  # Test that additional arguments don't cause errors
  expect_no_error(to_bbxml(question, some_extra_arg = "value"))
})