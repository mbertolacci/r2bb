test_that("md_to_html_pandoc works with length 1 character vector", {
  md_text <- "This is **bold** text with *italic* formatting."
  result <- md_to_html_pandoc(md_text)
  
  expect_length(result, 1)
  expect_true(grepl("<strong>bold</strong>", result))
  expect_true(grepl("<em>italic</em>", result))
})

test_that("md_to_html_pandoc works with length > 1 character vector (no empty strings)", {
  md_texts <- c(
    "First paragraph with **bold** text.",
    "Second paragraph with *italic* text.",
    "Third paragraph with `code` text."
  )
  result <- md_to_html_pandoc(md_texts)
  
  expect_length(result, 3)
  expect_true(grepl("<strong>bold</strong>", result[1]))
  expect_true(grepl("<em>italic</em>", result[2]))
  expect_true(grepl("<code>code</code>", result[3]))
})

test_that("md_to_html_pandoc handles empty strings", {
  result1 <- md_to_html_pandoc("")
  expect_equal(result1, "")
  
  # All empty strings return vector of correct length with empty strings
  result_all_empty <- md_to_html_pandoc(c("", "", ""))
  expect_length(result_all_empty, 3)
  expect_true(all(result_all_empty == ""))
})

test_that("md_to_html_pandoc handles NULL input", {
  result <- md_to_html_pandoc(NULL)
  expect_length(result, 0)
})

test_that("md_to_html_pandoc with deferred = TRUE marks text for conversion", {
  md_text <- "This is **bold** text."
  result <- md_to_html_pandoc(md_text, deferred = TRUE)
  
  expect_length(result, 1)
  expect_true(grepl("!!!pandoc_convert_open!!!", result))
  expect_true(grepl("!!!pandoc_convert_close!!!", result))
  expect_true(grepl("This is \\*\\*bold\\*\\* text\\.", result))
})

test_that("md_to_html_pandoc with deferred = TRUE handles multiple elements", {
  md_texts <- c("First **bold** text.", "Second *italic* text.")
  result <- md_to_html_pandoc(md_texts, deferred = TRUE)
  
  expect_length(result, 2)
  expect_true(all(grepl("!!!pandoc_convert_open!!!", result)))
  expect_true(all(grepl("!!!pandoc_convert_close!!!", result)))
})

test_that("md_to_html_pandoc with deferred = TRUE handles empty strings", {
  result <- md_to_html_pandoc(c("", "test", ""), deferred = TRUE)
  expect_length(result, 3)
  expect_equal(result[1], "")
  expect_equal(result[3], "")
  expect_true(grepl("!!!pandoc_convert_open!!!", result[2]))
})

test_that("md_to_html_pandoc_convert_deferred processes deferred text", {
  deferred_texts <- c(
    "Normal text with !!!pandoc_convert_open!!!**bold** text!!!pandoc_convert_close!!! here.",
    "Another !!!pandoc_convert_open!!!*italic* text!!!pandoc_convert_close!!! example."
  )
  
  result <- md_to_html_pandoc_convert_deferred(deferred_texts)
  
  expect_length(result, 2)
  expect_true(grepl("<strong>bold</strong>", result[1]))
  expect_true(grepl("<em>italic</em>", result[2]))
  expect_false(any(grepl("!!!pandoc_convert_open!!!", result)))
  expect_false(any(grepl("!!!pandoc_convert_close!!!", result)))
})

test_that("md_to_html_pandoc_convert_deferred handles multiple deferred sections", {
  deferred_text <- "Text with !!!pandoc_convert_open!!!**bold**!!!pandoc_convert_close!!! and !!!pandoc_convert_open!!!*italic*!!!pandoc_convert_close!!! formatting."
  
  result <- md_to_html_pandoc_convert_deferred(deferred_text)
  
  expect_length(result, 1)
  expect_true(grepl("<strong>bold</strong>", result))
  expect_true(grepl("<em>italic</em>", result))
  expect_false(grepl("!!!pandoc_convert_open!!!", result))
})

test_that("md_to_html_pandoc_convert_deferred handles text with no deferred sections", {
  normal_text <- "This is just normal text without any deferred sections."
  result <- md_to_html_pandoc_convert_deferred(normal_text)
  
  expect_length(result, 1)
  expect_equal(result, normal_text)
})

test_that("html_to_md_pandoc works with length 1 character vector", {
  html_text <- "<p>This is <strong>bold</strong> text with <em>italic</em> formatting.</p>"
  result <- html_to_md_pandoc(html_text)
  
  expect_length(result, 1)
  expect_true(grepl("\\*\\*bold\\*\\*", result))
  expect_true(grepl("\\*italic\\*", result))
})

test_that("html_to_md_pandoc handles empty string", {
  result <- html_to_md_pandoc("")
  expect_equal(result, "")
})

test_that("html_to_md_pandoc handles NULL input", {
  result <- html_to_md_pandoc(NULL)
  expect_equal(result, "")
})

test_that("html_to_md_pandoc throws error for length > 1 character vector", {
  html_texts <- c(
    "<p>First paragraph</p>",
    "<p>Second paragraph</p>"
  )
  
  expect_error(
    html_to_md_pandoc(html_texts),
    "html_to_md_pandoc only supports a single HTML string"
  )
})

test_that("html_to_md_pandoc converts complex HTML correctly", {
  html_text <- "<p>Text with <strong>bold</strong>, <em>italic</em>, and <code>code</code>.</p><ul><li>List item 1</li><li>List item 2</li></ul>"
  result <- html_to_md_pandoc(html_text)
  
  expect_true(grepl("\\*\\*bold\\*\\*", result))
  expect_true(grepl("\\*italic\\*", result))
  expect_true(grepl("`code`", result))
  expect_true(grepl("-   List item", result))
})

test_that("deferred conversion detects conflicting markers", {
  conflicting_text <- "Text with !!!pandoc_convert_open!!! already present"
  
  expect_error(
    md_to_html_pandoc(conflicting_text, deferred = TRUE),
    'md_to_html_pandoc_mark_for_conversion: x contains "!!!pandoc_convert_open!!!" or "!!!pandoc_convert_close!!!"'
  )
})

test_that("round trip conversion preserves markdown structure", {
  original_md <- "This is **bold** text with *italic* and `code` formatting."
  
  # Convert to HTML and back to markdown
  html_result <- md_to_html_pandoc(original_md)
  md_result <- html_to_md_pandoc(html_result)
  
  # Should preserve the formatting structure (though exact format may vary)
  expect_true(grepl("\\*\\*bold\\*\\*", md_result))
  expect_true(grepl("\\*italic\\*", md_result))
  expect_true(grepl("`code`", md_result))
})

test_that("md_to_html_pandoc preserves order in multi-element vectors", {
  md_texts <- c("First", "Second", "Third")
  result <- md_to_html_pandoc(md_texts)
  
  expect_length(result, 3)
  expect_true(grepl("First", result[1]))
  expect_true(grepl("Second", result[2]))
  expect_true(grepl("Third", result[3]))
})

test_that("md_to_html_pandoc handles mixed content with empty strings correctly", {
  # Test the edge case behavior with mixed empty and non-empty strings
  # Note: This tests the actual implementation behavior, which may not split correctly
  # when empty strings are present due to the separator approach
  md_texts <- c("text1", "", "text2")
  result <- md_to_html_pandoc(md_texts)
  
  expect_length(result, 3)
  expect_true(grepl("text1", result[1]))
  expect_true(result[2] == "")
  expect_true(grepl("text2", result[3]))
})

test_that("deferred conversion workflow works end-to-end", {
  # Step 1: Mark text for deferred conversion
  md_texts <- c(
    "Regular text with **bold**.",
    "",
    "More text with *italic*."
  )
  deferred <- md_to_html_pandoc(md_texts, deferred = TRUE)
  
  # Step 2: Embed in larger text
  combined_text <- paste("Prefix:", deferred, "Suffix:")
  
  # Step 3: Convert all deferred sections
  final_result <- md_to_html_pandoc_convert_deferred(combined_text)
  
  expect_length(final_result, 3)
  expect_true(all(grepl("Prefix:", final_result)))
  expect_true(all(grepl("Suffix:", final_result)))
  expect_true(grepl("<strong>bold</strong>", final_result[1]))
  expect_true(grepl("<em>italic</em>", final_result[3]))
  expect_false(any(grepl("!!!pandoc_convert", final_result)))
})