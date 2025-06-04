

rendered_images <- list()

png_base64 <- function(filename, width, height) {
  temp_path <- tempfile(fileext = '.png')
  png(temp_path, width = width, height = height, res = dpi, units = 'in')
  rendered_images <<- c(rendered_images, list(temp_path))
}

plot_hook <- knitr::knit_hooks$get('plot')
knitr::knit_hooks$set(plot = function(x, options, ...) {
  if (options$dev == 'png_base64') {
    actual_path <- rendered_images[[x]]
    x <- sprintf('data:image/png;base64,%s', base64enc::base64encode(actual_path))
  }
  plot_hook(x, options, ...)
})
knitr::opts_chunk$set(dev = 'png_base64', fig.ext = 'png')

on.exit({
  for (path in rendered_images) {
    if (file.exists(path)) {
      file.remove(path)
    }
  }
})
