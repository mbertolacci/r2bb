#' Render an Ryaml file to a YAML file
#'
#' This function renders an Ryaml file to a YAML file using the knitr package.
#' 
#' The wrapper around knitr is very thin. The only addition is that you can set
#' the `render` option to `yaml_print` to print the output in YAML format;
#' when paired with echo = FALSE and results = 'asis', this will print the YAML
#' to the output document. Since knitr maintains the indentation of the output
#' equal to that of the code chunk, the YAML will be rendered in a way that is
#' a valid sub-property of where the code chunk is in the document.
#'
#' @param from The input Ryaml file
#' @param to The output YAML file
#' @param embed_images Whether to embed images in the output YAML file in
#' base64 format
#' @param clean_intermediates Whether to clean the intermediate files
#' created by knitr
#' @param envir The environment to use for evaluating R code
#' @param validate Whether to validate the output YAML file. This is done by
#' calling \code{\link[yaml]{yaml.load}} on the output file; if an error is
#' thrown, the function will stop with an error message.
#' @param ... Additional arguments to pass to \code{\link[knitr]{knit}}
#' @return The output of \code{\link[knitr]{knit}}
#' @export
render_ryaml <- function(
  from,
  to,
  embed_images = TRUE,
  clean_intermediates = embed_images,
  envir = parent.frame(),
  validate = TRUE,
  ...
) {
  if (missing(to)) {
    to <- sub("\\.Ryaml$", ".yaml", from)
    to <- sub("\\.yaml$", ".yaml", to)
  }

  current_opts_knit <- knitr::opts_knit$get()
  on.exit(knitr::opts_knit$restore(current_opts_knit), add = TRUE)
  current_opts_chunk <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$restore(current_opts_chunk), add = TRUE)
  current_knit_hooks <- knitr::knit_hooks$get()
  on.exit(knitr::knit_hooks$restore(current_knit_hooks), add = TRUE)
  current_opts_hooks <- knitr::opts_hooks$get()
  on.exit(knitr::opts_hooks$restore(current_opts_hooks), add = TRUE)
  current_opts_template <- knitr::opts_template$get()
  on.exit(knitr::opts_template$restore(current_opts_template), add = TRUE)

  # Set knitr to use markdown settings
  knitr::render_markdown()

  if (embed_images) {
    plot_hook <- knitr::knit_hooks$get('plot')
    knitr::knit_hooks$set(plot = function(x, options, ...) {
      image_base64 <- base64enc::base64encode(x)

      x <- if (tools::file_ext(x) == 'png') {
        sprintf('data:image/png;base64,%s', image_base64)
      } else if (tools::file_ext(x) %in% c('jpg', 'jpeg')) {
        sprintf('data:image/jpeg;base64,%s', image_base64)
      } else if (tools::file_ext(x) %in% c('gif')) {
        sprintf('data:image/gif;base64,%s', image_base64)
      } else {
        stop('Unknown image type: ', tools::file_ext(x))
      }

      plot_hook(x, options, ...)
    })
  }

  output <- knitr::knit(from, to, ..., envir = envir)

  if (clean_intermediates) {
    # Drops any trailing /
    figures_path <- gsub('/$', '', knitr::opts_chunk$get('fig.path'))
    if (file.exists(figures_path)) {
      unlink(figures_path, recursive = TRUE)
    }
  }

  if (validate) {
    tryCatch({
      yaml::yaml.load(to)
    }, error = function(e) {
      stop('Error reading YAML file: ', e)
    })
  }

  output
}

#' Read a Ryaml or YAML file
#'
#' This function reads an Ryaml or YAML file into an R object. This is almost the
#' same as calling \code{\link{render_ryaml}} and then \code{\link{yaml.load}}
#' on the output file, except that it also supports reading multiple YAML
#' documents from a single file.
#'
#' @param path The path to the Ryaml file
#' @param allow_multiple Whether to allow multiple questions in the file
#' @param ... Additional arguments to pass to \code{\link{render_ryaml}}
#' @return If `allow_multiple` is `FALSE`, the return value is a single R
#' object. If `allow_multiple` is `TRUE`, the return value is a list of R
#' objects.
#' @export
read_ryaml <- function(path, allow_multiple = FALSE, ...) {
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

#' Print an object in YAML format
#'
#' This function prints an object in YAML format. It is used by the `render_ryaml`
#' function to print the output in YAML format.
#'
#' @param x The object to print
#' @param ... Additional arguments to pass to `yaml::as.yaml`
#' @export
yaml_print <- function(x, ...) {
  cat(yaml::as.yaml(x))
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