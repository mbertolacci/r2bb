.normalize_logical <- function(x, default = FALSE) {
  if (is.null(x)) {
    return(default)
  }
  as.logical(x)
}

.normalize_string <- function(x, default = '') {
  if (is.null(x)) {
    return(default)
  }
  trimws(as.character(x))
}

.normalize_integer <- function(x, default = 0) {
  if (is.null(x)) {
    return(default)
  }
  as.integer(x)
}

.normalize_numeric <- function(x, default = 0) {
  if (is.null(x)) {
    return(default)
  }
  as.numeric(x)
}

.write_yaml_multiple <- function(x, path, ...) {
  yaml_parts <- sapply(x, yaml::as.yaml, ...)
  output <- paste0(yaml_parts, collapse = '---\n')
  writeLines(output, path)
}

.fill_template <- function(filename, template_data, collapse = TRUE) {
  template_lines <- readLines(system.file(
    'templates',
    filename,
    package = 'r2bb'
  ))
  output_lines <- whisker::whisker.render(template_lines, template_data)
  if (collapse) {
    paste0(output_lines, collapse = '\n')
  } else {
    output_lines
  }
}

.drop_names <- function(x) {
  names(x) <- NULL
  x
}

.trim_with_newline <- function(text) {
  paste0(trimws(text), '\n')
}

.get_convert_rich_text_wrapped <- function(convert_rich_text, convert_rich_text_options) {
  if (is.logical(convert_rich_text) && !convert_rich_text) {
    function(x) if (is.null(x)) '' else x
  } else {
    convert_rich_text <- match.fun(convert_rich_text)
    function(x) {
      do.call(convert_rich_text, c(list(x), convert_rich_text_options))
    }
  }
}
