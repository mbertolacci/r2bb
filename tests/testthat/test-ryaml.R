test_that("render_ryaml renders basic RYaml to YAML", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
animal: dog
how_good_are_dogs: `r "great!"`
', file = temp_ryaml)
  
  result <- render_ryaml(temp_ryaml, temp_yaml, quiet = TRUE)
  expect_true(file.exists(temp_yaml))
  
  yaml_content <- readLines(temp_yaml)
  expect_true(any(grepl("animal: dog", yaml_content)))
  expect_true(any(grepl("how_good_are_dogs: great!", yaml_content)))
})

test_that("render_ryaml with yaml_print works", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
```{r, echo=FALSE}
animal_names <- c("dog", "cat", "bird")
```
animal_names:
  ```{r, render = yaml_print, echo=FALSE, results="asis"}
  animal_names
  ```
', file = temp_ryaml)
  
  result <- render_ryaml(temp_ryaml, temp_yaml, quiet = TRUE)
  expect_true(file.exists(temp_yaml))
  
  yaml_content <- yaml::yaml.load_file(temp_yaml)
  expect_equal(yaml_content$animal_names, c("dog", "cat", "bird"))
})

test_that("render_ryaml with nested objects works", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
```{r, echo=FALSE}
animal_info <- list(
  dog = list(
    name = "dog",
    color = "brown"
  )
)
```
animal_info:
  ```{r, render = yaml_print, echo=FALSE, results="asis"}
  animal_info
  ```
', file = temp_ryaml)
  
  result <- render_ryaml(temp_ryaml, temp_yaml, quiet = TRUE)
  expect_true(file.exists(temp_yaml))
  
  yaml_content <- yaml::yaml.load_file(temp_yaml)
  expect_equal(yaml_content$animal_info$dog$name, "dog")
  expect_equal(yaml_content$animal_info$dog$color, "brown")
})

test_that("render_ryaml with embed_images = FALSE works", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
title: Test
value: `r 2 + 2`
', file = temp_ryaml)
  
  result <- render_ryaml(temp_ryaml, temp_yaml, embed_images = FALSE, quiet = TRUE)
  expect_true(file.exists(temp_yaml))
  
  yaml_content <- yaml::yaml.load_file(temp_yaml)
  expect_equal(yaml_content$value, 4)
})

test_that("render_ryaml validates YAML by default", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
valid_field: value
', file = temp_ryaml)
  
  expect_no_error(render_ryaml(temp_ryaml, temp_yaml, quiet = TRUE))
})

test_that("render_ryaml can skip validation", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
valid_field: value
', file = temp_ryaml)
  
  expect_no_error(render_ryaml(temp_ryaml, temp_yaml, validate = FALSE, quiet = TRUE))
})

test_that("render_ryaml auto-generates output filename", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  expected_yaml <- sub("\\.Ryaml$", ".yaml", temp_ryaml)
  on.exit(unlink(c(temp_ryaml, expected_yaml)))
  
  cat('
test: value
', file = temp_ryaml)
  
  result <- render_ryaml(temp_ryaml, quiet = TRUE)
  expect_true(file.exists(expected_yaml))
})

test_that("read_ryaml reads YAML files", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
animal: dog
number: 42
', file = temp_yaml)
  
  result <- read_ryaml(temp_yaml)
  expect_equal(result$animal, "dog")
  expect_equal(result$number, 42)
})

test_that("read_ryaml reads RYaml files", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  on.exit(unlink(temp_ryaml))
  
  cat('
animal: dog
computed_value: `r 2 + 2`
', file = temp_ryaml)
  
  result <- read_ryaml(temp_ryaml)
  expect_equal(result$animal, "dog")
  expect_equal(result$computed_value, 4)
})

test_that("read_ryaml with allow_multiple = FALSE fails on multiple documents", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
animal: dog
---
animal: cat
', file = temp_yaml)
  
  expect_error(read_ryaml(temp_yaml, allow_multiple = FALSE))
})

test_that("read_ryaml with allow_multiple = TRUE handles multiple documents", {
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))
  
  cat('
animal: dog
---
animal: cat
', file = temp_yaml)
  
  result <- read_ryaml(temp_yaml, allow_multiple = TRUE)
  expect_length(result, 2)
  expect_equal(result[[1]]$animal, "dog")
  expect_equal(result[[2]]$animal, "cat")
})

test_that("read_ryaml handles unknown file extensions", {
  temp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temp_file))
  
  cat('
animal: dog
', file = temp_file)
  
  expect_error(read_ryaml(temp_file))
})

test_that("yaml_print prints objects in YAML format", {
  test_obj <- list(
    name = "test",
    values = c(1, 2, 3),
    nested = list(a = "hello", b = "world")
  )
  
  output <- capture.output(yaml_print(test_obj))
  
  expect_true(any(grepl("name: test", output)))
  expect_true(any(grepl("values:", output)))
  expect_true(any(grepl("nested:", output)))
})

test_that("yaml_print handles simple values", {
  output <- capture.output(yaml_print("simple string"))
  expect_equal(output, "simple string")
})

test_that("yaml_print handles vectors", {
  output <- capture.output(yaml_print(c("a", "b", "c")))
  expect_true(any(grepl("- a", output)))
  expect_true(any(grepl("- b", output)))
  expect_true(any(grepl("- c", output)))
})

test_that("render_ryaml with R code chunks in markdown text", {
  temp_ryaml <- tempfile(fileext = ".Ryaml")
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(temp_ryaml, temp_yaml)))
  
  cat('
question_text: |
  The mean of 1 to 10 is:
  
  ```{r}
  mean(1:10)
  ```
  
  What does this code do?
', file = temp_ryaml)
  
  result <- render_ryaml(temp_ryaml, temp_yaml, quiet = TRUE)
  expect_true(file.exists(temp_yaml))
  
  yaml_content <- yaml::yaml.load_file(temp_yaml)
  expect_true(grepl("mean\\(1:10\\)", yaml_content$question_text))
  expect_true(grepl("5.5", yaml_content$question_text))
})