test_that("to_markdown.r2bb_question throws error for unknown type", {
  # Create a question with the base class only
  question <- structure(
    list(title = "Base Test", question_type = "unknown"),
    class = "r2bb_question"
  )
  
  expect_error(to_markdown(question), "Unknown question type")
})

test_that("to_markdown.r2bb_question_matching returns formatted markdown", {
  question <- normalize_question(list(
    title = "Matching Test",
    question_type = "matching",
    question_text = "Match the items below",
    max_score = 5.0,
    questions = list(
      list(text = "Question 1", answer_index = 1),
      list(text = "Question 2", answer_index = 2)
    ),
    answers = list("Answer A", "Answer B"),
    positive_feedback = "Great job!",
    instructor_notes = "Test matching concepts"
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Matching Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("Match the items below", md_output))
  expect_true(grepl("## Questions", md_output))
  expect_true(grepl("\\[A1\\].*Question 1", md_output))
  expect_true(grepl("\\[A2\\].*Question 2", md_output))
  expect_true(grepl("## Answers", md_output))
  expect_true(grepl("\\(A1\\).*Answer A", md_output))
  expect_true(grepl("\\(A2\\).*Answer B", md_output))
  expect_true(grepl("Great job!", md_output))
  expect_true(grepl("Test matching concepts", md_output))
})

test_that("to_markdown.r2bb_question_single_blank returns formatted markdown", {
  question <- normalize_question(list(
    title = "Single Blank Test",
    question_type = "single_blank",
    question_text = "Fill in the blank: ___",
    max_score = 2.0,
    answers = list("correct_answer", "another_answer"),
    negative_feedback = "Try again"
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Single Blank Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("Fill in the blank", md_output))
  expect_true(grepl("## Answers", md_output))
  expect_true(grepl("- correct_answer", md_output))
  expect_true(grepl("- another_answer", md_output))
  expect_true(grepl("case insensitive", md_output)) # Default case sensitivity
  expect_true(grepl("Try again", md_output))
})

test_that("to_markdown.r2bb_question_multiple_blanks returns formatted markdown", {
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
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Multiple Blanks Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("Fill.*and", md_output))
  expect_true(grepl("## Answers", md_output))
  expect_true(grepl("blank1:", md_output))
  expect_true(grepl("blank2:", md_output))
  expect_true(grepl("- answer1", md_output))
  expect_true(grepl("- answer2", md_output))
})

test_that("to_markdown.r2bb_question_numeric returns formatted markdown", {
  question <- normalize_question(list(
    title = "Numeric Test",
    question_type = "numeric",
    question_text = "What is pi?",
    max_score = 2.0,
    tolerance = 0.01,
    answers = list(3.14159)
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Numeric Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("What is pi", md_output))
  expect_true(grepl("## Answer", md_output))
  expect_true(grepl("3.14159", md_output))
  expect_true(grepl("tolerance 0.01", md_output))
})

test_that("to_markdown.r2bb_question_file_upload returns formatted markdown", {
  question <- normalize_question(list(
    title = "File Upload Test",
    question_type = "file_upload",
    question_text = "Upload your essay",
    max_score = 10.0,
    instructor_notes = "Check for plagiarism"
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# File Upload Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("Upload your essay", md_output))
  expect_true(grepl("## Answer", md_output))
  expect_true(grepl("\\(File upload\\)", md_output))
  expect_true(grepl("Check for plagiarism", md_output))
})

test_that("to_markdown.r2bb_question_short_answer returns formatted markdown", {
  question <- normalize_question(list(
    title = "Short Answer Test",
    question_type = "short_answer",
    question_text = "Explain photosynthesis",
    max_score = 5.0,
    answer = "Sample answer about photosynthesis"
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Short Answer Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("Explain photosynthesis", md_output))
  expect_true(grepl("## Answer", md_output))
  expect_true(grepl("\\(Short answer\\)", md_output))
})

test_that("to_markdown.r2bb_question_multiple_choice returns formatted markdown", {
  question <- normalize_question(list(
    title = "Multiple Choice Test",
    question_type = "multiple_choice",
    question_text = "What is 2 + 2?",
    max_score = 1.0,
    random_order = FALSE,
    answers = list(
      list(text = "4", correct = TRUE),
      list(text = "3", correct = FALSE),
      list(text = "5", correct = FALSE)
    )
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Multiple Choice Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("What is 2 \\+ 2", md_output))
  expect_true(grepl("## Answers", md_output))
  expect_true(grepl("- \\[x\\].*4", md_output)) # Correct answer marked with x
  expect_true(grepl("- \\[ \\].*3", md_output)) # Incorrect answer marked with space
  expect_true(grepl("- \\[ \\].*5", md_output))
  expect_true(grepl("\\[x\\] = correct answer", md_output))
})

test_that("to_markdown.r2bb_question_multiple_choice handles random order", {
  question <- normalize_question(list(
    title = "Random Order Test",
    question_type = "multiple_choice",
    question_text = "Test random order",
    max_score = 1.0,
    random_order = TRUE,
    answers = list(
      list(text = "Answer A", correct = TRUE),
      list(text = "Answer B", correct = FALSE),
      list(text = "Answer C", correct = FALSE)
    )
  ))
  
  # Generate multiple outputs to test randomization
  md_outputs <- replicate(10, to_markdown(question), simplify = FALSE)
  
  # All should be valid markdown
  for (md_output in md_outputs) {
    expect_type(md_output, "character")
    expect_true(nchar(md_output) > 0)
    expect_true(grepl("# Random Order Test", md_output))
    expect_true(grepl("Answer A", md_output))
    expect_true(grepl("Answer B", md_output))
    expect_true(grepl("Answer C", md_output))
  }
  
  # Should have some variation in order (not guaranteed but very likely)
  expect_true(length(unique(md_outputs)) > 1)
})

test_that("to_markdown.r2bb_question_multiple_answer returns formatted markdown", {
  question <- normalize_question(list(
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
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("# Multiple Answer Test", md_output))
  expect_true(grepl("## Question", md_output))
  expect_true(grepl("Select all correct", md_output))
  expect_true(grepl("## Answers", md_output))
  expect_true(grepl("- \\[x\\].*Correct 1", md_output))
  expect_true(grepl("- \\[x\\].*Correct 2", md_output))
  expect_true(grepl("- \\[ \\].*Wrong", md_output))
})

test_that("to_markdown functions handle partial credit answers", {
  question <- normalize_question(list(
    title = "Partial Credit Test",
    question_type = "multiple_choice",
    question_text = "Test partial credit",
    max_score = 5.0,
    partial_credit = TRUE,
    answers = list(
      list(text = "Best answer", correct = TRUE),
      list(text = "Partial answer", correct = FALSE),
      list(text = "Wrong answer", correct = FALSE)
    )
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(grepl("\\[x\\] = correct answer", md_output))
  expect_true(grepl("\\[p\\] = partial credit given", md_output))
})

test_that("to_markdown functions handle feedback properly", {
  # Test with both positive and negative feedback
  question_both <- normalize_question(list(
    title = "Both Feedback Test",
    question_type = "short_answer",
    question_text = "Test both feedback",
    positive_feedback = "Excellent work!",
    negative_feedback = "Try harder next time"
  ))
  
  md_both <- to_markdown(question_both)
  expect_true(grepl("# Positive feedback", md_both))
  expect_true(grepl("Excellent work!", md_both))
  expect_true(grepl("# Negative feedback", md_both))
  expect_true(grepl("Try harder next time", md_both))
  
  # Test with only positive feedback
  question_pos <- normalize_question(list(
    title = "Positive Only Test",
    question_type = "short_answer",
    question_text = "Test positive only",
    positive_feedback = "Good job!"
  ))
  
  md_pos <- to_markdown(question_pos)
  expect_true(grepl("# Positive feedback", md_pos))
  expect_true(grepl("Good job!", md_pos))
  expect_false(grepl("# Negative feedback", md_pos))
  
  # Test with same feedback for both
  question_same <- normalize_question(list(
    title = "Same Feedback Test",
    question_type = "short_answer",
    question_text = "Test same feedback",
    positive_feedback = "Standard feedback",
    negative_feedback = "Standard feedback"
  ))
  
  md_same <- to_markdown(question_same)
  expect_true(grepl("# Feedback", md_same))
  expect_true(grepl("Standard feedback", md_same))
  expect_false(grepl("# Positive feedback", md_same))
  expect_false(grepl("# Negative feedback", md_same))
})

test_that("to_markdown functions handle empty feedback and notes", {
  question <- normalize_question(list(
    title = "No Feedback Test",
    question_type = "short_answer",
    question_text = "Test no feedback"
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_false(grepl("# Feedback", md_output))
  expect_false(grepl("# Positive feedback", md_output))
  expect_false(grepl("# Negative feedback", md_output))
  expect_false(grepl("# Instructor notes", md_output))
})

test_that("to_markdown functions handle multiline text correctly", {
  question <- normalize_question(list(
    title = "Multiline Test",
    question_type = "multiple_choice",
    question_text = "This is a question with\nmultiple lines\nof text",
    answers = list(
      list(text = "Answer with\nmultiple lines", correct = TRUE),
      list(text = "Single line answer", correct = FALSE)
    ),
    instructor_notes = "These are notes\nwith multiple lines\nfor testing"
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(grepl("multiple lines", md_output))
  expect_true(grepl("for testing", md_output))
})

test_that("to_markdown functions handle special markdown characters", {
  question <- normalize_question(list(
    title = "Special *Characters* Test",
    question_type = "multiple_choice",
    question_text = "What about **bold** and _italic_ text?",
    answers = list(
      list(text = "Answer with *emphasis*", correct = TRUE),
      list(text = "Normal answer", correct = FALSE)
    )
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(nchar(md_output) > 0)
  expect_true(grepl("\\*\\*bold\\*\\*", md_output))
  expect_true(grepl("\\*emphasis\\*", md_output))
})

test_that("to_markdown functions pass additional arguments", {
  question <- normalize_question(list(
    title = "Args Test",
    question_type = "multiple_choice",
    question_text = "Test arguments",
    answers = list(
      list(text = "Answer", correct = TRUE)
    )
  ))
  
  # Test that additional arguments don't cause errors
  expect_no_error(to_markdown(question, some_extra_arg = "value"))
})

test_that("to_markdown functions handle blank answer types correctly", {
  question <- normalize_question(list(
    title = "Blank Types Test",
    question_type = "single_blank",
    question_text = "Test different answer types",
    answers = list("exact_answer")
  ))
  
  md_output <- to_markdown(question)
  
  expect_type(md_output, "character")
  expect_true(grepl("exact", md_output)) # Should show answer type
  expect_true(grepl("case", md_output)) # Should show case sensitivity
})