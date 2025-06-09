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

## Installation

```r
# Install from GitHub (development version)
# devtools::install_github("username/r2bb")  # Update with actual repo

# Load the package
library(r2bb)
```

## Quick Start

### Simple YAML Question

Create a basic multiple choice question using YAML:

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

```r
# Load the question and print it in Markdown
q <- read_question("france-question.yaml")
print(q)
```

### Dynamic RYaml Question

Create questions with embedded R code for computed answers:

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

```r
# Render RYaml to YAML, then load and print
render_ryaml("t-test.Ryaml", "t-test.yaml")
q <- read_question("t-test.yaml")
print(q)
```

### Question Pools and Tests

```r
# Create a question pool
pool_questions <- list(
  read_question("question1.yaml"),
  read_question("question2.yaml"),
  read_question("question3.yaml")
)

pool <- list(
  title = "Statistics Questions",
  questions = pool_questions
)
class(pool) <- "r2bb_pool"

# Export pool
to_bbxml_package(pool, "stats-pool.zip")

# Create a test with the pool
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

# Export test
to_bbxml_package(test, "midterm-exam.zip")
```

## Supported Question Types

- **multiple_choice**: Single correct answer from multiple options
- **multiple_answer**: Multiple correct answers possible  
- **numeric**: Numerical answer with tolerance
- **short_answer**: Text-based answer
- **single_blank**: Fill in one blank
- **multiple_blanks**: Fill in multiple blanks
- **matching**: Match items between two lists
- **file_upload**: File submission

## Documentation

- [Basic Usage Vignette](https://mbertolacci.github.io/r2bb/articles/basic-usage.html) - Comprehensive usage guide
- [Question Types Vignette](https://mbertolacci.github.io/r2bb/articles/question-types.html) - Examples of all question types
- [RYaml Vignette](https://mbertolacci.github.io/r2bb/articles/ryaml.html) - Use RYaml for dynamic questions
- [Example Files](https://github.com/mbertolacci/r2bb/tree/main/inst/examples) - Sample YAML and RYaml files

## Workflow

1. **Create questions** using YAML files, RYaml files, or R code
2. **Organize** questions into pools or tests as needed
3. **Convert** pools or tests to Blackboard format using `to_bbxml_package()`
4. **Import** the generated ZIP file into Blackboard

## Requirements

- R (>= 3.5.0)
- Pandoc (for Markdown conversion)
- Required R packages: yaml, xml2, uuid, stringr, tools, base64enc, knitr, rmarkdown

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

This package is under active development. Please report issues or contribute improvements via GitHub.