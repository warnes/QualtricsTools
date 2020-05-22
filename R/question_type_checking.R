#' Determine if a question is a multiple choice and multiple answer question
#'
#' Each of the is-functions defined in the qualtrics package are used
#' for determining which response parsing function should be used. A
#' function is considered multiple choice if it is listed in
#' the qsf file as having [['Payload']][['QuestionType']] == "MC"
#' (standing for Multiple Choice), and the `[['Payload']][['Selector']]` is set to one of the
#' following:
#' "Multiple Answer Vertical",
#' "Multiple Answer Horizontal",
#' "Multiple Choice Select Box",
#' "Multiple Answer Column",
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_mc_multiple_answer <- function(question) {

    is_Multiple_Choice = (question[['Payload']][['QuestionType']] == "MC")
    has_MultipleAnswer_selector = (question[['Payload']][['Selector']] == "MAVR" ||
                                   question[['Payload']][['Selector']] == "MAHR" ||
                                   question[['Payload']][['Selector']] == "MSB" ||
                                   question[['Payload']][['Selector']] == "MACOL")

    is_Multiple_Answer = isTRUE(is_Multiple_Choice && has_MultipleAnswer_selector)

    return(is_Multiple_Answer)
}


#' Determine if a question is a matrix and multiple answer question
#'
#' A question is considered a matrix multiple answer question if it
#' is a `[['Payload']][['QuestionType']] == "Matrix"` question with
#' `[['Payload']][['SubSelector']] == "MultipleAnswer"`
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_matrix_multiple_answer <- function(question) {
  is_Matrix = (question[['Payload']][['QuestionType']] == "Matrix")
  has_Matrix_MA_selector = (question[['Payload']][['SubSelector']] == "MultipleAnswer")
  is_Matrix_Multiple_Answer <- isTRUE(is_Matrix && has_Matrix_MA_selector)
  return(is_Matrix_Multiple_Answer)
}

#' Determine if a question is a text entry question with a numerical
#' validation
#'
#' A question is considered a text entry with numeric validation if it
#' is a `[['Payload']][['QuestionType']] == "TE"` question with
#' `[['Payload']][['Validation']][['Settings']][['Type']] == "ContentType"
#' and `[['Payload']][['Validation']][['Settings']][['ContentType']] ==
#' "ValidNumber"
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#' @return Boolean value indicating whether the quest is text entry questions with numeric validation
is_text_entry_numeric <- function(question){
  is_text_entry_numeric <- question[['Payload']][['QuestionType']] == "TE" &&
    #Check that it has content validation
    ! is.null(question[['Payload']][['Validation']][['Settings']][['ContentType']]) &&
    #Content Type validation is ValidNumber
    question[['Payload']][['Validation']][['Settings']][['ContentType']] == "ValidNumber"

  return(is_text_entry_numeric)
}

#' Determine if a question is text entry without numerical validation
#'
#' Text question without numerical validation should be reported as text appendices typically.
#' questions with numeric validation are reported instead with summary statistics tables.
#' Check here for standard appendix text entry
#'
#' @param question A question from a qualtrics survey that includes "Payload"
#' @return Boolean value indicating whether this is a text entry question that should have an associated appendix
is_text_entry_appendix <- function(question) {
  #This needs to be text entry and either have null validation content type
    #OR have content type that is not ValidNumber
  is_text_entry_appendix <- question[['Payload']][['QuestionType']] == "TE" &&
    (is.null(question[['Payload']][['Validation']][['Settings']][['ContentType']]) ||
       question[['Payload']][['Validation']][['Settings']][['ContentType']] != "ValidNumber")
  return(is_text_entry_appendix)
}

#' Determine if a question is a single answer question
#'
#' Each of the is-functions defined in the qualtrics package are used
#' for determining which response parsing function should be used. A
#' function is considered multiple choice if it is listed in
#' the qsf file as having [['Payload']][['QuestionType']] == "MC"
#' (standing for Multiple Choice), and the `[['Payload']][['Selector']]` is set to one of the
#' following:
#' "Single Answer Vertical",
#' "Single Answer Horizontal",
#' "Single Answer Column",
#' "Dropdown List",
#' "Select Box",
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_mc_single_answer <- function(question) {
    is_Multiple_Choice = (question[['Payload']][['QuestionType']] == "MC")
    has_SingleAnswer_selector = (question[['Payload']][['Selector']] == "SAVR" ||
                                 question[['Payload']][['Selector']] == "SAHR" ||
                                 question[['Payload']][['Selector']] == "SACOL" ||
                                 question[['Payload']][['Selector']] == "DL" ||
                                 question[['Payload']][['Selector']] == "SB")
    is_MC_Single_answer <- isTRUE(is_Multiple_Choice && has_SingleAnswer_selector)

    return(is_MC_Single_answer)
}


#' Determine if a question has an NA type choice by looking for negative recode values.
#' Using negative recode values to indicate N/A
#' type choices is the convention used in this project to identify N/A type
#' choices regardless of specific question text. N/A type choices may be "Not Applicable"
#' or some other option that is tabled separately from the other, "valid" responses.
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it has any NA type choices
#' indicated by negative recode values and false otherwise.
has_na <- function(question) {
  # determine if the question has any NA-type choices
  if ('RecodeValues' %in% names(question[['Payload']])) {
    has_na <- any(question[['Payload']][['RecodeValues']] < 0)
  } else
    has_na <- FALSE

  return(has_na)
}




#' Determine if a question is a matrix and multiple answer question
#'
#' A question is considered a matrix multiple answer question if it
#' is a `[['Payload']][['QuestionType']] == "Matrix"` question with
#' `[['Payload']][['SubSelector']] == "SingleAnswer"` or `[['Payload']][['SubSelector']] == "DL"`
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_matrix_single_answer <- function(question) {
  is_Matrix = question[['Payload']][['QuestionType']] == "Matrix"

  has_Matrix_SA_selector = (question[['Payload']][['SubSelector']] == "DL" ||
                              question[['Payload']][['SubSelector']] == "SingleAnswer")
  is_Matrix_Single_Answer <- isTRUE(is_Matrix && has_Matrix_SA_selector)
  return(is_Matrix_Single_Answer)
}


#' Determine if a question is a matrix and multiple answer question
#'
#' A question is considered a matrix multiple answer question if it
#' is a `[['Payload']][['QuestionType']] == "Matrix"` question with
#' `[['Payload']][['SubSelector']] == "SingleAnswer"` or `[['Payload']][['SubSelector']] == "DL"`
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_matrix_bipolar <- function(question) {
  is_Matrix = question[['Payload']][['QuestionType']] == "Matrix"

  has_Matrix_SA_selector = question[['Payload']][['Selector']] == "Bipolar"
  is_Matrix_Bipolar <- isTRUE(is_Matrix && has_Matrix_SA_selector)
  return(is_Matrix_Bipolar)
}


#' Determine if a question is a multiple answer question
is_multiple_answer <- function(x) {
  return(is_mc_multiple_answer(x) || is_matrix_multiple_answer(x))
}


#' Determine if a question is a single answer question
is_single_answer <- function(x) {
  return(is_mc_single_answer(x) || is_matrix_bipolar(x) || is_matrix_single_answer(x))
}


#' Determine if a question is a rank order question
is_rank_order <- function(x) {
  return(x[['Payload']][['QuestionType']] == "RO")
}


#' Determine if a question is a text entry question
is_text_entry <- function(x) {
  return(x[['Payload']][['QuestionType']] == "TE")
}


#' Determine if a question is a matrix question
is_matrix_question <- function(x) {
  return(x[['Payload']][['QuestionType']] == "Matrix")
}


#' Determine if a question has a single text entry component
#'
#' A question has a single text entry component if it is not a Text Entry Question
#' (QuestionType != "TE") and the text string "TEXT" appears in the name of one response column.
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, TRUE if there is a single text entry
#' component and FALSE otherwise.
has_single_te_component <- function(question) {

  if ("Payload" %in% names(question) &&
      question[['Payload']][['QuestionType']] != "TE" &&
      length(grep("TEXT", names(question[['Responses']]))) == 1) {TRUE}
  else {FALSE}
}
