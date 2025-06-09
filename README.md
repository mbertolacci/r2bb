<!-- badges: start -->
[![R-CMD-check](https://github.com/mbertolacci/r2bb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mbertolacci/r2bb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# r2bb

Construct Blackboard LMS tests, question pools, and individual questions with R.

## Overview

**r2bb** is an R package that enables you to create Blackboard Learning Management System assessments using R. It converts R-defined content into XML format that can be imported directly into Blackboard.

In practice, the typical workflow is:

1. Build your questions locally using YAML, RYaml, or R code.
2. Group those questions into [question pools](https://help.blackboard.com/Learn/Instructor/Tests_Pools_Surveys/Question_Pools) or [tests](https://help.blackboard.com/Learn/Instructor/Tests_Pools_Surveys/Tests).
3. Export the pool or test as a Blackboard‑compatible ZIP file with `to_bbxml_package()`.
4. Import that ZIP file into Blackboard.

Think of it as a Blackboard-specific version of the [R exams package](https://www.r-exams.org/).

### Key Features

- **Multiple question types**: Multiple choice, numeric, short answer, fill-in-the-blank, matching, and more
- **Dynamic content**: Use R code to generate randomized questions and computed answers
- **Rich text support**: Markdown formatting with mathematical notation (MathML)
- **Image embedding**: Automatic base64 encoding for portability
- **Bidirectional**: Import existing Blackboard packages back into R
- **Three creation methods**: Static YAML, dynamic RYaml, or programmatic R code

### Vignettes

See the Quick Start below, or read the following vignettes:

- [Basic Usage Vignette](https://mbertolacci.github.io/r2bb/articles/basic-usage.html) - Comprehensive usage guide
- [Question Types Vignette](https://mbertolacci.github.io/r2bb/articles/question-types.html) - Examples of all question types
- [RYaml Vignette](https://mbertolacci.github.io/r2bb/articles/ryaml.html) - Use RYaml for dynamic questions
- [Example Files](https://github.com/mbertolacci/r2bb/tree/main/inst/examples) - Sample YAML and RYaml files

## Installation

The package is not on CRAN; you can install it from GitHub as follows:

```r
devtools::install_github("mbertolacci/r2bb")
```

Then load the package as usual:

```r
library(r2bb)
```

## Quick Start

Below we talk about how to create questions using YAML, RYaml, and R code. These can be combined into question pools, which can be exported to Blackboard.

The following question types are supported:

- **multiple_choice**: Single correct answer from multiple options
- **multiple_answer**: Multiple correct answers possible  
- **numeric**: Numerical answer with tolerance
- **short_answer**: Text-based answer
- **single_blank**: Fill in one blank
- **multiple_blanks**: Fill in multiple blanks
- **matching**: Match items between two lists
- **file_upload**: File submission

More information on question types is available in the [Question Types Vignette](https://mbertolacci.github.io/r2bb/articles/question-types.html).

You can also create tests, which can draw from questions pool or just contain individual questions directly.

### Simple YAML Question

Here we create a basic multiple choice question using YAML:

```yaml
# france-question.yaml
title: Capital of France
question_type: multiple_choice
question_text: What is the capital of France?
max_score: 1.0
feedback: Paris is the capital of France.
answers:
  - text: Paris
    correct: true
  - text: London
    correct: false
  - text: Berlin
    correct: false
  - text: Madrid
    correct: false
```

This can be loaded as follows. If you call `print` on the question, it will be rendered in Markdown:

```r
# Load the question and print it in Markdown
q <- read_question("france-question.yaml")
print(q)
```

### Dynamic RYaml Question

You can also create questions in RYaml format (`.Ryaml`, see the [RYaml Vignette](https://mbertolacci.github.io/r2bb/articles/ryaml.html)) with embedded R code for computed answers. RYaml is a thin wrapper around Rmarkdown that outputs YAML.

The following example creates a multiple choice question with a computed answer:

````yaml
# t-test.Ryaml
```{r, echo = FALSE}
x <- c(1, 5, 3, 4, 2)
y <- c(0, 2, 4, 6, 8)
t_result <- t.test(x, y)
t_stat <- sprintf('%.2f', t_result$statistic)
```

title: T-test Analysis
question_type: multiple_choice
question_text: |
  Given the data x = `r paste(x, collapse=", ")` and y = `r paste(y, collapse=", ")`,
  what is the t-statistic for testing the difference in means?
answers:
  - text: "`r t_stat`"
    correct: true
  - text: "1.23"
    correct: false
````

The RYaml file can be read in directly as follows:

```r
q <- read_question("t-test.Ryaml")
print(q)
```

If you want to see what the rendered YAML looks like, you can use the `render_ryaml` function:

```r
render_ryaml("t-test.Ryaml", "t-test.yaml")
```

This will create a file `t-test.yaml` that you can read using a text editor.

### Question Pools and Tests

On Blackboard, a question pool is a collection of questions that can be used in a test. In `r2bb`, you can create a question pool by passing a list of questions to the `pool` function:

```r
# Create a question pool
pool <- normalize_pool(list(
  title = "Statistics Questions",
  questions = list(
    read_question("question1.yaml"),
    read_question("question2.yaml"),
    read_question("question3.yaml")
  )
))
```

To import the question pool into Blackboard, you can use the `to_bbxml_package` function:

```r
to_bbxml_package(pool, "stats-pool.zip")
```

This will create a file `stats-pool.zip` that you can import into Blackboard.

### Tests

You can also create tests, which can draw from questions pool or just contain individual questions directly.

The following example creates a test with a random block using the pool created above, and a bonus question:

```r
test <- normalize_test(list(
  title = "Midterm Exam",
  description = "A short midterm exam.",
  instructions = "Answer all questions.",
  contents = list(
    list(
      type = "random_block",
      pool = pool,
      questions_to_display = 1,
      points_per_question = 4
    ),
    list(
      type = "question",
      points = 1,
      question = read_question("bonus-question.yaml")
    )
  )
))
```

To export the test to Blackboard, you can use the `to_bbxml_package` function:

```r
to_bbxml_package(test, "midterm-exam.zip")
```

## Importing pools and tests from Blackboard

You can import pools and tests from Blackboard by passing the path to the ZIP file to the `from_bbxml_package` function. Here is an example of importing a pool:

```r
pool <- from_bbxml_package("stats-pool.zip")
```

The same method works for tests as well. This function by default attempts to convert the HTML in the questions to Markdown using Pandoc. You can disable this by setting `convert_html_to_markdown = FALSE`.

Please note that this functionality (importing from Blackboard) is the most experimental and unreliable part of this package. It has worked okay for me (the package maintainer) once or twice, but I would double-check very carefully its output.

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

This package is under active development. Please report issues or contribute improvements via GitHub.
