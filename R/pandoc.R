#' Convert a markdown string to HTML using pandoc
#'
#' This function converts a markdown string to HTML using pandoc. It is a
#' wrapper around [rmarkdown::pandoc_convert()].
#' 
#' If `deferred` is TRUE, the text will be marked for conversion and included in
#' a longer string or set of strings. Later, these can be converted in one go 
#' using [md_to_html_pandoc_convert_deferred()].
#' 
#' @param x A character vector of markdown text (potentially containing multiple
#' elements), or a string containing text marked for conversion.
#' @param deferred Whether to defer conversion of markdown to HTML using pandoc.
#' If TRUE, the text will be marked for conversion and included in a longer
#' string or set of strings. Later, these can be converted in one go.
#' @param ... Additional arguments to pass to [rmarkdown::pandoc_convert()].
#' @return A character vector of HTML text.
#' @export
md_to_html_pandoc <- function(
  x,
  deferred = FALSE,
  ...
) {
  if (deferred) {
    .md_to_html_pandoc_deferred(x, ...)
  } else {
    .md_to_html_pandoc_direct(x, ...)
  }
}

#' @rdname md_to_html_pandoc
#' @export
md_to_html_pandoc_convert_deferred <- function(x, ...) {
  part_regex <- stringr::regex(
    '!!!pandoc_convert_open!!!(?<text>.*?)!!!pandoc_convert_close!!!',
    dotall = TRUE
  )
  # Extract all deferred strings
  to_convert <- lapply(x, function(x_i) {
    stringr::str_match_all(x_i, part_regex)[[1]][, 'text']
  })
  to_convert_flat <- unlist(to_convert)
  # Convert all deferred strings at once
  converted_flat <- md_to_html_pandoc(to_convert_flat, ...)
  # Replace the deferred strings in the original text
  last_index <- 0L
  sapply(seq_along(x), function(i) {
    start_index <- last_index + 1L
    end_index <- last_index + length(to_convert[[i]])
    # Matches happen from right to left, so we need to reverse the converted
    # strings
    converted_i <- rev(converted_flat[start_index : end_index])
    last_index <<- end_index
    j <- 0L
    stringr::str_replace_all(x[i], part_regex, function(x) {
      j <<- j + 1L
      converted_i[j]
    })
  })
}

#' Convert an HTML string to markdown using pandoc
#'
#' This function converts an HTML string to markdown using pandoc. It is a
#' wrapper around the `pandoc_convert` function from the `rmarkdown` package.
#'
#' @param x A character vector of HTML text
#' elements).
#' @return A character vector of markdown text.
html_to_md_pandoc <- function(x) {
  if (length(x) > 1) {
    stop('html_to_md_pandoc only supports a single HTML string')
  }
  if (is.null(x) || x == '') {
    return('')
  }

  input_file <- tempfile()
  output_file <- tempfile()
  on.exit(unlink(c(input_file, output_file)))
  writeLines(x, input_file)
  rmarkdown::pandoc_convert(
    input_file,
    from = 'html',
    to = 'markdown',
    output = output_file
  )
  paste(readLines(output_file), collapse = '\n')
}

.md_to_html_pandoc_direct <- function(x, ...) {
  if (is.null(x) || all(x == '')) {
    return(rep('', length(x)))
  }

  separator <- as.character(runif(1))
  x_concat <- paste(x, collapse = sprintf('\n\n%s\n\n', separator))
  input_file <- tempfile()
  output_file <- tempfile()
  on.exit(unlink(c(input_file, output_file)))
  writeLines(x_concat, input_file)
  rmarkdown::pandoc_convert(
    input_file,
    from = 'markdown',
    to = 'html',
    output = output_file,
    ...
  )
  output_concat <- paste(readLines(output_file), collapse = '\n')
  strsplit(output_concat, sprintf("\n<p>%s</p>\n", separator))[[1]]
}

.md_to_html_pandoc_deferred <- function(x, ...) {
  if (any(grepl('!!!pandoc_convert_open!!!', x) | grepl('!!!pandoc_convert_close!!!', x))) {
    stop('md_to_html_pandoc_mark_for_conversion: x contains "!!!pandoc_convert_open!!!" or "!!!pandoc_convert_close!!!"')
  }
  ifelse(
    x == '',
    '',
    paste0('!!!pandoc_convert_open!!!', x, '!!!pandoc_convert_close!!!')
  )
}
