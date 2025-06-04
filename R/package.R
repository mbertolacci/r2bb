#' Convert an R2BB object to a BbXML package
#'
#' This function converts an R2BB object to a BbXML package, which can be
#' used to import into Blackboard.
#'
#' @param x An R2BB object to convert
#' @param output_path The path to save the output BbXML package
#' @param convert_rich_text Whether to convert rich text to HTML. By default,
#' `md_to_html_pandoc_mark_for_conversion` is used, which converts Markdown to
#' HTML using [pandoc](https://pandoc.org/). Can be FALSE to prevent any
#' conversion.
#' @return The path to the output BbXML package
#' @export
to_bbxml_package <- function(
  x,
  output_path,
  convert_rich_text = 'md_to_html_pandoc',
  convert_rich_text_options = list(deferred = TRUE),
  postprocess_xml = 'md_to_html_pandoc_convert_deferred',
  postprocess_xml_options = list(options = '--mathml')
) {
  convert_rich_text <- .get_convert_rich_text_wrapped(convert_rich_text, convert_rich_text_options)

  postprocess_xml <- match.fun(postprocess_xml)
  postprocess_xml_wrapped <- function(x) {
    do.call(postprocess_xml, c(list(x), postprocess_xml_options))
  }

  if (inherits(x, 'r2bb_test')) {
    .to_bbxml_package_test(x, output_path, convert_rich_text, postprocess_xml_wrapped)
  } else {
    if (inherits(x, 'r2bb_pool')) {
      x <- list(x)
    }
    .to_bbxml_package_pools(x, output_path, convert_rich_text, postprocess_xml_wrapped)
  }
}

.to_bbxml_package_test <- function(x, output_path, convert_rich_text, postprocess_xml) {
  resources_index <- 1L
  resources <- list()
  for (i in seq_along(x$contents)) {
    if (x$contents[[i]]$type == 'random_block') {
      x$contents[[i]]$pool$title <- sprintf(
        'single.qti.export.referenced.canvas.name.prefix %s',
        x$contents[[i]]$pool$title
      )
      resource <- list(
        file = sprintf('res%05d.dat', resources_index),
        title = x$contents[[i]]$pool$title,
        id = sprintf('res%05d', resources_index),
        type = 'assessment/x-bb-qti-pool',
        text = to_bbxml(
          x$contents[[i]]$pool,
          convert_rich_text = convert_rich_text,
          convert_rich_text_options = NULL
        )
      )
      resources_index <- resources_index + 1L
      resources <- c(resources, list(resource))
    }
  }
  resources <- c(resources, list(list(
    file = sprintf('res%05d.dat', resources_index),
    title = x$title,
    id = sprintf('res%05d', resources_index),
    type = 'assessment/x-bb-qti-test',
    text = to_bbxml(
      x,
      convert_rich_text = convert_rich_text,
      convert_rich_text_options = NULL
    )
  )))

  .write_package(resources, output_path, postprocess_xml)
}

.to_bbxml_package_pools <- function(x, output_path, convert_rich_text, postprocess_xml) {
  resource_index <- 1L
  resources <- lapply(x, function(pool) {
    name <- sprintf('res%05d', resource_index)
    resource_index <- resource_index + 1L
    list(
      file = sprintf('%s.dat', name),
      title = pool$title,
      id = name,
      type = 'assessment/x-bb-qti-pool',
      text = to_bbxml(
        pool,
        convert_rich_text = convert_rich_text,
        convert_rich_text_options = NULL
      )
    )
  })
  .write_package(resources, output_path, postprocess_xml)
}

.write_package <- function(resources, output_path, postprocess_xml) {
  resource_texts <- sapply(resources, getElement, 'text')
  resource_texts_postprocessed <- postprocess_xml(resource_texts)
  resources_postprocessed <- resources
  for (i in seq_along(resources_postprocessed)) {
    resources_postprocessed[[i]]$text <- resource_texts_postprocessed[i]
  }

  output_zip <- tempfile(fileext = '.zip')
  output_dir <- tempfile()
  dir.create(output_dir)
  on.exit(unlink(c(output_zip, output_dir), recursive = TRUE))

  writeLines('cx.package.info.version=6.0', file.path(output_dir, '.bb-package-info'))

  manifest_template_data <- list(resources = resources_postprocessed)
  manifest_xml_text <- .fill_template('imsmanifest.xml.mustache', manifest_template_data)
  writeLines(manifest_xml_text, file.path(output_dir, 'imsmanifest.xml'))

  for (i in seq_along(resources_postprocessed)) {
    writeLines(resources_postprocessed[[i]]$text, file.path(output_dir, resources_postprocessed[[i]]$file))
  }

  current_dir <- getwd()
  setwd(output_dir)
  on.exit(setwd(current_dir))
  zip(zipfile = output_zip, files = c(list.files(), '.bb-package-info'), flags = '-r9Xq')
  setwd(current_dir)

  return_value <- file.copy(output_zip, output_path, copy.mode = FALSE, overwrite = TRUE)
  if (!return_value) {
    stop('Failed to copy output zip file to ', output_path)
  }
  invisible(output_path)
}

#' Read a BbXML package
#'
#' This function reads a BbXML package zip file and extracts all tests and
#' pools from it. The results are returned as a list with two attributes:
#' `pools` and `tests`, either of which could be an empty list.
#'
#' By default, rich text fields in HTML are converted to Markdown using
#' pandoc. pandoc is sufficiently magical that this can work quite well, but
#' results are not guaranteed to be identical to the original.
#'
#' @param path The path to the BbXML package
#' @param convert_html_to_markdown Whether to convert HTML to Markdown.
#' @return An R2BB object
from_bbxml_package <- function(path, convert_html_to_markdown = TRUE) {
  output_dir <- tempfile()
  unzip(path, exdir = output_dir)
  on.exit(unlink(output_dir, recursive = TRUE))
  
  imsmanifest_path <- file.path(output_dir, 'imsmanifest.xml')
  imsmanifest_doc <- xml2::read_xml(imsmanifest_path)
  resource_nodes <- xml2::xml_find_all(imsmanifest_doc, '//resource')

  pools <- list()
  tests <- list()
  for (resource_node in resource_nodes) {
    resource_name <- xml2::xml_attr(resource_node, 'identifier')
    resource_filename <- xml2::xml_attr(resource_node, 'file')
    resource_type <- xml2::xml_attr(resource_node, 'type')

    resource_path <- file.path(output_dir, resource_filename)
    if (resource_type == 'assessment/x-bb-qti-pool') {
      resource_doc <- xml2::read_xml(resource_path)
      pools[[resource_name]] <- from_bbxml_pool(resource_doc, convert_html_to_markdown)
    } else if (resource_type == 'assessment/x-bb-qti-test') {
      # Will process this later
      tests[[resource_name]] <- resource_path
    }
  }

  for (name in names(tests)) {
    test_xml <- xml2::read_xml(tests[[name]])
    tests[[name]] <- from_bbxml_test(test_xml, convert_html_to_markdown, pools)
  }

  names(tests) <- NULL
  names(pools) <- NULL

  list(
    tests = tests,
    pools = pools
  )
}
