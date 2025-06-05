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
#' @param ... Additional arguments to pass to \code{\link[knitr]{knit}}
#' @return The output YAML file
#' @export
render_ryaml <- function(
  from,
  to,
  embed_images = TRUE,
  clean_intermediates = embed_images,
  envir = parent.frame(),
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

  output
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

