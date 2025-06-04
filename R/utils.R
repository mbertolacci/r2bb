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

.read_ryaml_or_yaml <- function(path, allow_multiple = FALSE, ...) {
  file_extension <- tools::file_ext(path)
  lines <- if (tolower(file_extension) %in% c('yaml', 'yml')) {
    readLines(path)
  } else if (tolower(file_extension) == 'ryaml') {
    tmp_file <- tempfile()
    on.exit(unlink(tmp_file))
    render_ryaml(path, tmp_file, ...)
    readLines(tmp_file)
  } else  {
    stop('Unknown file extension: ', file_extension, path)
  }

  .load_yaml_lines(lines, allow_multiple = allow_multiple)
}

.read_yaml <- function(path, ...) {
  .load_yaml_lines(readLines(path), ...)
}

.load_yaml_lines <- function(lines, allow_multiple = FALSE, ...) {
  boundary_lines <- c(0, which(lines == '---'), length(lines) + 1)
  n_documents <- length(boundary_lines) - 1
  output <- lapply(seq_len(n_documents), function(i) {
    start_line <- boundary_lines[i] + 1L
    end_line <- boundary_lines[i + 1] - 1L
    yaml::yaml.load(paste0(lines[start_line : end_line], collapse = '\n'))
  })
  if (allow_multiple) {
    output
  } else {
    if (length(output) != 1) {
      stop('Multiple YAML documents found')
    }
    output[[1]]
  }
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
