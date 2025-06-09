#' Convert from Blackboard XML to an R2BB question
#'
#' This function converts an object from Blackboard XML format to an R2BB question.
#'
#' Generally you will not call this directly; instead, call [from_bbxml_package()]
#' on an exported zip package file.
#'
#' By default, rich text fields in HTML are converted to Markdown using pandoc. This
#' can work quite well, but is not perfect; results are not guaranteed to be
#' identical to the original.
#'
#' @param x An xml2 node or nodeset
#' @param convert_html_to_markdown Whether to convert HTML to Markdown.
#' @seealso [from_bbxml_package()], [from_bbxml_pool()], [from_bbxml_test()]
#' @return An R2BB question
from_bbxml_question <- function(x, convert_html_to_markdown = TRUE) {
  if (!all(xml2::xml_name(x) == 'item')) {
    stop('x must be an xml2 node or nodeset of items')
  }
  if (inherits(x, 'xml_node')) {
    .from_bbxml_question_node(x, convert_html_to_markdown = convert_html_to_markdown)
  } else {
    lapply(x, .from_bbxml_question_node, convert_html_to_markdown = convert_html_to_markdown)
  }
}

.from_bbxml_question_node <- function(x, convert_html_to_markdown = TRUE) {
  convert_rich_text <- function(x) {
    if (convert_html_to_markdown) {
      html_to_md_pandoc(x)
    } else {
      x
    }
  }
  output <- list(
    type = 'question',
    title = xml2::xml_attr(x, 'title')
  )

  output$question_text <- x |>
    xml2::xml_find_first('.//presentation/flow[@class="Block"]/flow[@class="QUESTION_BLOCK"]/flow[@class="FORMATTED_TEXT_BLOCK"]/material/mat_extension/mat_formattedtext') |>
    xml2::xml_text() |>
    convert_rich_text()

  output$max_score <- x |>
    xml2::xml_find_first('.//itemmetadata/qmd_absolutescore_max') |>
    xml2::xml_text() |>
    as.numeric()

  output$partial_credit <- x |>
    xml2::xml_find_first('.//itemmetadata/bbmd_partialcredit') |>
    xml2::xml_text() |>
    as.logical()

  output$instructor_notes <- x |>
    xml2::xml_find_first('.//itemmetadata/qmd_instructornotes') |>
    xml2::xml_text()

  output$positive_feedback <- x |>
    xml2::xml_find_first('.//itemfeedback[@ident="correct"]/flow_mat/flow_mat/material/mat_extension/mat_formattedtext') |>
    xml2::xml_text() |>
    convert_rich_text()

  output$negative_feedback <- x |>
    xml2::xml_find_first('.//itemfeedback[@ident="incorrect"]/flow_mat/flow_mat/material/mat_extension/mat_formattedtext') |>
    xml2::xml_text() |>
    convert_rich_text()

  question_type_raw <- x |>
    xml2::xml_find_first('.//itemmetadata/bbmd_questiontype') |>
    xml2::xml_text()

  output$question_type <- c(
    'Multiple Answer' = 'multiple_answer',
    'Multiple Choice' = 'multiple_choice',
    'File Upload' = 'file_upload',
    'Matching' = 'matching',
    'Fill in the Blank Plus' = 'multiple_blanks',
    'Numeric' = 'numeric',
    'Short Response' = 'short_answer',
    'Fill in the Blank' = 'single_blank'
  )[question_type_raw]

  output <- if (output$question_type == 'matching') {
    .from_bbxml_question_node_matching(x, output, convert_rich_text)
  } else if (output$question_type == 'single_blank') {
    .from_bbxml_question_node_single_blank(x, output, convert_rich_text)
  } else if (output$question_type == 'multiple_blanks') {
    .from_bbxml_question_node_multiple_blanks(x, output, convert_rich_text)
  } else if (output$question_type == 'multiple_choice') {
    .from_bbxml_question_node_multiple_answer_or_choice(x, output, convert_rich_text)
  } else if (output$question_type == 'multiple_answer') {
    .from_bbxml_question_node_multiple_answer_or_choice(x, output, convert_rich_text)
  } else if (output$question_type == 'numeric') {
    .from_bbxml_question_node_numeric(x, output, convert_rich_text)
  } else if (output$question_type == 'file_upload') {
    .from_bbxml_question_node_file_upload(x, output, convert_rich_text)
  } else if (output$question_type == 'short_answer') {
    .from_bbxml_question_node_short_answer(x, output, convert_rich_text)
  } else {
    stop('Unknown question type: ', output$question_type)
  }

  class(output) <- c(
    sprintf('r2bb_question_%s', output$question_type),
    'r2bb_question'
  )
  output
}


.from_bbxml_question_node_matching <- function(x, output, convert_rich_text) {
  response_blocks <- x |>
    xml2::xml_find_all('.//presentation/flow[@class="Block"]/flow[@class="RESPONSE_BLOCK"]/flow[@class="Block"]')

  output$random_order <- response_blocks[[1]] |>
    xml2::xml_find_first('.//render_choice') |>
    xml2::xml_attr('shuffle') == 'Yes'

  question_ids <- response_blocks |>
    xml2::xml_find_all('./response_lid') |>
    xml2::xml_attr('ident')
  question_texts <- response_blocks |>
    xml2::xml_find_all('.//mat_formattedtext') |>
    xml2::xml_text() |>
    sapply(convert_rich_text)
  question_choice_ids <- response_blocks |>
    lapply(function(x) {
      x |>
        xml2::xml_find_all('.//response_label') |>
        xml2::xml_attr('ident')
    })

  output$answers <- x |>
    xml2::xml_find_all('.//presentation/flow[@class="Block"]/flow[@class="RIGHT_MATCH_BLOCK"]//mat_formattedtext') |>
    xml2::xml_text() |>
    sapply(convert_rich_text)

  question_respconditions <- x |>
    xml2::xml_find_all('.//resprocessing/respcondition[not(@title)]')
  question_answer_ids <- question_respconditions |>
    xml2::xml_find_all('.//conditionvar/varequal') |>
    xml2::xml_attr('respident')
  question_correct_answer_ids <- question_respconditions |>
    xml2::xml_find_all('.//conditionvar/varequal') |>
    xml2::xml_text()
  names(question_correct_answer_ids) <- question_answer_ids
  question_partial_credit_percentages <- question_respconditions |>
    xml2::xml_find_all('.//setvar[@variablename="PartialCreditPercent"]') |>
    xml2::xml_text() |>
    as.numeric()
  names(question_partial_credit_percentages) <- question_answer_ids

  output$questions <- lapply(seq_along(question_ids), function(i) {
    id <- question_ids[i]
    choice_ids <- question_choice_ids[[i]]
    list(
      text = .drop_names(question_texts[i]),
      answer_index = which(choice_ids == question_correct_answer_ids[id]),
      partial_credit_percentage = .drop_names(question_partial_credit_percentages[id])
    )
  })

  output
}

.from_bbxml_question_node_single_blank <- function(x, output, convert_rich_text) {
  output$answers <- list()

  answer_respcondition <- x |>
    xml2::xml_find_all('.//resprocessing/respcondition[@title!="incorrect"]')
  for (xx in answer_respcondition) {
    xx_varequals <- xml2::xml_find_all(xx, './/conditionvar/varequal')
    xx_setvar <- xml2::xml_find_all(xx, './/setvar')

    xx_case_sensitive <- xml2::xml_attr(xx_varequals, 'case') == 'Yes'
    xx_text <- xml2::xml_text(xx_varequals)
    xx_type <- xml2::xml_text(xx_setvar)

    xx_answer <- list(
      text = xx_text,
      case_sensitive = xx_case_sensitive,
      type = if (tolower(xx_type) == 'exact') {
        'exact'
      } else if (tolower(xx_type) == 'contains') {
        'contains'
      } else if (tolower(xx_type) == 'matches') {
        'pattern'
      } else {
        stop('Unknown setmatch type: ', xx_type)
      }
    )
    output$answers <- c(output$answers, list(xx_answer))
  }

  output
}

.from_bbxml_question_node_multiple_blanks <- function(x, output, convert_rich_text) {
  answer_names <- x |>
    xml2::xml_find_all('.//presentation/flow[@class="Block"]/flow[@class="RESPONSE_BLOCK"]/response_str') |>
    xml2::xml_attr('ident')

  output$answers <- rep(list(list()), length(answer_names))
  names(output$answers) <- answer_names

  answer_varequals <- x |>
    xml2::xml_find_all('.//resprocessing/respcondition[@title="correct"]/conditionvar//varequal')
  for (xx in answer_varequals) {
    xx_name <- xml2::xml_attr(xx, 'respident')
    xx_case_sensitive <- xml2::xml_attr(xx, 'case') == 'Yes'
    xx_type <- xml2::xml_attr(xx, 'setmatch')
    xx_text <- xml2::xml_text(xx)
    xx_answer <- list(
      text = xx_text,
      case_sensitive = xx_case_sensitive,
      type = if (is.na(xx_type)) {
        'exact'
      } else if (xx_type == 'Contains') {
        'contains'
      } else if (xx_type == 'Matches') {
        'pattern'
      } else {
        stop('Unknown setmatch type: ', xx_type)
      }
    )
    output$answers[[xx_name]] <- c(output$answers[[xx_name]], list(xx_answer))
  }

  output
}

.from_bbxml_question_node_multiple_answer_or_choice <- function(x, output, convert_rich_text) {
  output$answer_numbering <- x |>
    xml2::xml_find_first('.//itemmetadata/bbmd_numbertype') |>
    xml2::xml_text()
  output$answer_orientation <- x |>
    xml2::xml_find_first('.//itemmetadata/bbmd_orientationtype') |>
    xml2::xml_text()

  response_render_choice <- x |>
    xml2::xml_find_first('.//presentation/flow[@class="Block"]/flow[@class="RESPONSE_BLOCK"]/response_lid/render_choice')
  output$random_order <- response_render_choice |>
    xml2::xml_attr('shuffle') == 'Yes'

  answer_ids <- response_render_choice |>
    xml2::xml_find_all('.//response_label') |>
    xml2::xml_attr('ident')
  answer_texts <- response_render_choice |>
    xml2::xml_find_all('.//mat_formattedtext') |>
    xml2::xml_text() |>
    sapply(convert_rich_text)
 
  answer_corrects <- rep(FALSE, length(answer_ids))
  names(answer_corrects) <- answer_ids

  answer_varequals <- xml2::xml_find_all(
    x,
    './/resprocessing/respcondition[@title="correct"]/conditionvar//varequal'
  )
  for (xx in answer_varequals) {
    if (xml2::xml_name(xml2::xml_parent(xx)) != 'not') {
      answer_corrects[xml2::xml_text(xx)] <- TRUE
    }
  }

  answer_partial_credit_percentages <- x |>
    xml2::xml_find_all('.//resprocessing/respcondition[not(@title)]/setvar') |>
    xml2::xml_text() |>
    as.numeric()
  answer_partial_credit_ids <- x |>
    xml2::xml_find_all('.//resprocessing/respcondition[not(@title)]/conditionvar/varequal') |>
    xml2::xml_attr('respident')
  names(answer_partial_credit_percentages) <- answer_partial_credit_ids

  output$answers <- lapply(seq_along(answer_ids), function(i) {
    id <- answer_ids[i]
    list(
      text = .drop_names(answer_texts[i]),
      correct = .drop_names(answer_corrects[id]),
      partial_credit_percentage = .drop_names(answer_partial_credit_percentages[id])
    )
  })

  output
}

.from_bbxml_question_node_numeric <- function(x, output, convert_rich_text) {
  output$answer <- x |>
    xml2::xml_find_first('.//resprocessing//varequal') |>
    xml2::xml_text() |>
    as.numeric()
  lower_bound <- x |>
    xml2::xml_find_first('.//resprocessing//vargte') |>
    xml2::xml_text() |>
    as.numeric()
  output$tolerance <- output$answer - lower_bound
  output
}

.from_bbxml_question_node_file_upload <- function(x, output, convert_rich_text) {
  output
}

.from_bbxml_question_node_short_answer <- function(x, output, convert_rich_text) {
  output$answer_rows <- x |>
    xml2::xml_find_first('.//presentation/flow[@class="Block"]/flow[@class="RESPONSE_BLOCK"]/response_str/render_fib') |>
    xml2::xml_attr('rows') |>
    as.numeric()
  output$answer <- x |>
    xml2::xml_find_first('.//itemfeedback[@ident="solution"]//mat_formattedtext') |>
    xml2::xml_text() |>
    convert_rich_text()
  output
}
