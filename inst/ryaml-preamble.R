yaml_print <- function(x, ...) {
  cat(yaml::as.yaml(x, ...))
}
