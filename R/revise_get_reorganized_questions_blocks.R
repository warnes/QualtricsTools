
#' Generate a list of user notes
#'
#' Each list element is named with the
#' parent question export tag and contains the survey note text prepended with "User note: "
#'
#' @inheritParams get_reorganized_questions_and_blocks
#'
#' @return This returns a list of user notes named with the parent question export tag.
note_text_from_survey <- function(survey) {
  #From the survey, keep elements that have user notes; these are identified from the list of survey elements
  #as those with Elements "NT"
  user_notes_text <- keep(survey[["SurveyElements"]], ~.x[["Element"]]=="NT")
  #Now rename the user notes with their QID based on parent ID instead of the primary attribute
  user_notes_text <- set_names(user_notes_text, map_chr(user_notes_text, ~ .x[["Payload"]][["ParentID"]]))
  #From the notes block payload, pluck "Notes" element
  #Now we want to pull only the next for the user notes
  #But only if the element "Removed" is false
  #First keep only the "Notes" section within Payload, which has the information we need
  user_notes_text <- map(user_notes_text, ~ pluck(.x, "Payload","Notes"))
  #only keep notes for which the "Removed" status is FALSE; these are notes that have not been deleted
  user_notes_text <- map(user_notes_text, ~ keep(.x, ~ !.x[["Removed"]]))
  #Now we want to pluck only the messages, which is the text of the note
  user_notes_text <- map(user_notes_text, ~map(.x, ~pluck(.x,"Message")))
  user_notes_text <- map(user_notes_text, ~ flatten(.x))
  #Prepend each note with the "User note: " text tag
  user_notes_text <- map(user_notes_text, ~map(.x,~stringr::str_c("User note: ",.x)))
  return(user_notes_text)
}

#' Insert the Notes for a question into its qtNotes element
#' @inheritParams link_responses_to_questions
#' @param notes A list of blocks with type element "NT"
#' @return A list of questions with user notes from the Qualtrics Survey appended as
#' named element "qtNotes"
insert_notes_into_questions <- function(questions, notes) {
  #For each question, first see if it has corresponding notes.
  #If it does, append the question list with element "qtNotes"
  questions_with_notes <- purrr::modify_if(questions, ~.x[["PrimaryAttribute"]] %in% names(notes),
                                           ~ append(.x, list("qtNotes"= pluck(notes,.x[["PrimaryAttribute"]]))))
  return(questions_with_notes)
}


#' Determine the question type from the QSF
#'
#' Determine the question type from the question's Payload information and return this as human readable text.
#'
#' Determine the type of question based on user understanding and how the question results will be processed.
#' This is in the QualtricsTools intially with questions, then again when splitting
#' side-by-side questions into their components. The list of question types has been defined
#' by Tufts OIR based on their reporting needs and will be expanded as needed with additional
#' types of questions. Current options include Multiple Answer, Single Answer, Rank Order,
#' Text Entry, Side-by-side, Descriptive Box, Other question type.
#' @param question A survey question
#' @return Human readable question type to assist with understanding how the quuestion is
#' interpreted by QualtricsTools and how results are processed. Question types that are
#' currently unspecified in this function with return value "Other question type."
qtype_human <- function(question) {

  QuestionTypeHuman <-  dplyr::case_when(is_multiple_answer(question) ~ "Multiple Answer",
                                         is_single_answer(question) ~ "Single Answer",
                                         is_rank_order(question) ~ "Rank Order",
                                         is_text_entry(question) ~ "Text Entry",
                                         question[["Payload"]][["QuestionType"]]=="SBS" ~ "Side-by-side",
                                         question[["Payload"]][["QuestionType"]]=="DB" ~ "Descriptive Box",
                                         TRUE ~ "Other question type")
  return(QuestionTypeHuman)

}


#' Split Side-by-Side Questions into Multiple Component Questions
#'
#' Given a side-by-side question, split this into separate questions list elements for each
#' of the multiple question parts.
#'
#' This function is called within the QualtricsTools setup process and splits
#' multiple questions into separate component questions so that results may be processsed.
#' QualtricsTools is designed to process results to side-by-side questions a separate questions.
#' When this is called as part of the setup process, there is an earlier check to
#' ensure that questions passed to this function are actual side-by-side questions.
#' @param question A question with additional question elements
#' @return A list of with each component question split out of the side-by-side
split_side_by_side_q <- function(question) {

  #For a SBS question only
  #As part of the setup process, there is an earlier  side-by-side actually has additional questions
  #Assume additional questions here
  split_q <- list()

  #We may not actually need this
  mainq <- modify_at(question, "AdditionalQuestions", ~NULL)

  #Extract the question text from the main part of the question
  #If we've already run the other parts of the question, this will be the clean question text
  #If not, pull the original question text
  mainq_text <- if_else("QuestionTextClean" %in% names(question),
                        question[["QuestionTextClean"]],
                        question[["Payload"]][["QuestionText"]])

  main_qid <- mainq[["PrimaryAttribute"]]

  additional_questions <- question[["Payload"]][["AdditionalQuestions"]]
  #Add names based on the QID; replace "#" symbol with underscore
  additional_questions <- set_names(additional_questions ,
                                    map_chr(additional_questions, ~ str_replace(.x[["QuestionID"]], "#","_")))
  #Create a new element for with split questions. These have the information
  #from additional questions supplemented with key information from the main question.
  split_q <- additional_questions
  #Assign the additional question information as Payload for the new question element
  split_q <- map(split_q, ~ list("Payload" = .x))
  #Pull qtNotes from the original question and append them to each item
  #Add an additional note that this was split from a side-by-side question
  split_q <- map(splitq,
                 ~ rlist::list.append(.x,qt_notes = if_else("qtNotes" %in% names(mainq),
                                                            list(append(mainq[["qtNotes"]],
                                                                        "This question was split from a side-by-side question.")),
                                                            list("This question was split from a side-by-side question."))))
  #Add clean question text to the side-by-side question element
  split_q <- map(split_q,
                 ~ rlist::list.append(.x, QuestionTextClean = str_c(mainq_text, "---",
                                                                    .x[["Payload"]][["QuestionText"]])))
  #Add QualtricsQtype for the side by side; prepend with "SBS" to indicate this came from a side-by-side question
  split_q <- map(split_q,
                 ~ rlist::list.append(.x, Qualtrics_qtype = paste0("SBS_",.x[["Payload"]][["QuestionType"]])))
  #Add human readable question type
  split_q <- map(split_q, ~ rlist::list.append(.x, QuestionTypeHuman = qtype_human(.x)))
  #Add Primary Attribute that corresponds to the list element name
  split_q <- map(split_q, ~ rlist::list.append(.x, PrimaryAttribute = str_replace(.x[["Payload"]][["QuestionID"]], "#","_")))
  #Add a new element indicating that this was split from a side-by-side question
  split_q <- map(split_q, ~ rlist::list.append(.x, SBS_origQID = main_qid))


  return(split_q)
}


#' Split side by side question into their component question parts
#'
#' @param questions A list of questions pulled from Survey Elements of a Qualtrics QSF
#' @return A list of questions with side-by-side questions split into their component question parts.
split_sbs_questions <-  function(questions) {
  #Iterate through the questions
  for (q in questions) {
    #For each side-by-side question
    if(q[["Qualtrics_qtype"]]=="SBS") {
      #Update the list of split questions
      questions <- append(questions,
                          split_side_by_side_q(q),
                          after=match(q[["PrimaryAttribute"]],names(questions)))
    }
  }
  #Now remove the original side-by-side questions since we no longer need these
  questions <- discard(questions, ~ .x[["Qualtrics_qtype"]]=="SBS")
}


#' Split block elements for side-by-side questions into components for each additional question
#'
#' Iterate through a block and, where appropriate, split the block element
#' for a side-by-side question into separate elements for each component of the side-by-side.
#'
#' This function will update the Block Elements item of a single block. A list of QIDs
#' for side-by-side questions will be generated automatically as part of the
#' setup process within QualtricsTools.
#' @param block A Qualtrics block survey element
#' @param sbs_question_ids A list of QIDs for side-by-side questions to be split
#' @return A block with block elements revised to have split for side-by-side questions
split_block_elements <- function(block,sbs_question_qids) {
  if (! "BlockElements" %in% names(block)) {
    return(block)
  }
  block_elements <- block[["BlockElements"]]

  #Create the elements that we will want to split
  sbs_questions <- list()

  for (i in 1:length(sbs_question_qids)) {
    sbs_question_entry <- list(Type = "Question",
                               QuestionID = names(sbs_question_qids)[[i]],
                               ParentID = sbs_question_qids[[i]])
    sbs_questions[[i]] <- sbs_question_entry
  }

  for (element in block_elements) {
    #Check that the element has a question ID, then check whether it's in the list
    if ("QuestionID" %in% names(element) &&
        element[["QuestionID"]] %in% map_chr(sbs_questions,"ParentID")) {

      qid_orig <- element[["QuestionID"]]
      split_elements <- keep(sbs_questions, ~.x[["ParentID"]]==qid_orig)
      #Now name them with the Question ID
      split_elements <- set_names(split_elements, paste0(map_chr(., "QuestionID"),"-Question"))
      #And now remove the parent ID element
      split_elements <- purrr::map(split_elements, ~rlist::list.remove(.x,"ParentID"))

      block_elements <- append(block_elements,
                               split_elements,
                               after = match(element[["QuestionID"]],str_replace(names(block_elements),"-Question$","")))
    }
  }

  #And now we want to remove those original question elements
  block_elements <- discard(block_elements, ~.x[["QuestionID"]] %in% sbs_question_qids)

  block[["BlockElements"]] <- block_elements
  return(block)

}


#' Insert skip logic into questions
#' @param questions A list of questions from a Qualtrics survey
#' @param blocks A list of blocks from a Qualtrics survey
#' @return A list of questions with skip logic added to questions where appropriate
insert_skiplogic_into_questions <- function(questions,blocks) {
  for (b in blocks) {
    if (! "BlockElements" %in% names(b)) {
      return(questions)
    } else {
      for (be in b[["BlockElements"]]) {
        has_skip_logic <- "SkipLogic" %in% names(be)
        if(has_skip_logic) {
          skip_logic <- be[["SkipLogic"]]
          #Add display logic to the question
          questions <- modify_at(questions,be[["QuestionID"]], ~rlist::list.append(.x,SkipLogic = skip_logic))
        }
      }
      return(questions)
    }
  }
}


#' Insert questions into blocks
#' @param questions A list of questions from a Qualtrics survey
#' @param blocks A list of blocks from a Qualtrics survey
#' @return The list of blocks with question elements updated to include the full question.
insert_questions_into_block <- function(block,questions) {
  if ("BlockElements" %in% names(block)) {
    block[["BlockElements"]] <- modify_if(block[["BlockElements"]],
                                          ~.x[["Type"]]=="Question",
                                          ~pluck(questions,.x[["QuestionID"]]))
  }
  return(block)
}

#' Get Restructured Questions (with inserted Responses)
#' and Blocks (with inserted Questions) from data
#' provided by Qualtrics.
#'
#' This function returns a list with two elements, where
#' the first element is questions with their responses
#' and results-tables listed in them, and secondly
#' the blocks of the survey, with questions listed in
#' each block as BlockElements.
#' @param survey A qualtrics survey list object,
#' uploaded from a Qualtrics Survey File (QSF). Use
#' ask_for_qsf() to create such a survey list object from a QSF file.
#' @param responses A dataframe of Qualtrics responses to a survey.
#' Use ask_for_csv() to create such a dataframe from a CSV file.
#' @param original_first_rows A dataframe contianing the header information
#' for each column of response data. This dataframe includes a row for the DataExportTag based
#' response column names, another for the Question Text stem and choice text (although
#' truncated), and a row with QID based column names.
#' @return A list with two elements: the questions,
#' and the survey blocks. All questions in the trash are
#' excluded, HTML and CSS are removed from question text,
#' responses, notes, and a more easily human readable question
#' type are inserted into each question. The blocks then have
#' their BlockElements replaced by corresponding questions
#' where applicable. The BlockElements of the list of Blocks
#' from a survey originally only refer to questions
#' by their DataExportTag. However, this is inconvenient
#' because it adds an additional lookup step, and so they are
#' replaced by the real objects here.
get_reorganized_questions_and_blocks_rv <- function(survey,
                                                    responses,
                                                    original_first_rows) {
  #The survey JSON was read in during an early block, so we can just work with this.
  #Extract the survey elements from survey

  survey_elements <- survey[["SurveyElements"]]
  # Name each item of survey elements based on the primary attribute.
  # This includes Survey Blocks, Survey Flow, Survey Options, and QIDs.
  # These names should make it much easier for us to get the information we need to extract.
  survey_elements <- purrr::set_names(survey_elements, map_chr(., "PrimaryAttribute"))

  #Extract QualtricsTools user notes so we can add these to questions.
  qtNotesList <- note_text_from_survey(survey)

  #Now extract the blocks; this is really the payload element of element "Survey Blocks"
  #Replace blocks_from_survey
  blocks_all <- purrr::pluck(survey_elements, "Survey Blocks", "Payload")
  #Name the blocks with their descriptive value
  blocks_all <- purrr::set_names(blocks_all, map_chr(blocks_all,"Description"))

  trash_questions <- blocks_all
  #Now pluck just the trash blocktrash_questions
  trash_questions <- pluck(trash_questions, "Trash / Unused Questions", "BlockElements")
  #Pull a character vector of only the QIDs for questions that are in the trash
  trash_questions <- map_chr(trash_questions,"QuestionID")

  #Remove trash block based on the "Type" element of the block
  blocks_notrash <- purrr::keep(blocks_all, ~ .x[["Type"]]!= "Trash")
  #Name the BlockElements (quesiton items) within the blocks
  blocks_notrash <- map(blocks_notrash, ~ modify_at(.x, "BlockElements",
                                                    ~set_names(.x, map_chr(.x, ~ str_c(.x[["QuestionID"]],.x[["Type"]],sep="-")))))

  #Extract a list of questions, SurveyElements that have Element "SQ"
  questions <- purrr::keep(survey_elements, ~ .x[["Element"]]=="SQ")
  #Remove questions that are in the trash, as specified by "trash_questions
  questions <- discard(questions, ~ .x[["PrimaryAttribute"]] %in% trash_questions)
  #Now clean question text using clean_html
  questions <- map(questions, ~ append(.x,list("QuestionTextClean" = clean_html(.x[["Payload"]][["QuestionText"]]))))
  #Add the Qualtrics question type; this is for reference and won't be used in results
  questions <- map(questions, ~append(.x, list("Qualtrics_qtype" = .x[["Payload"]][["QuestionType"]])))
  #Add human readable question type to each question; this can be edited if we want
  #We will want ot expand this as we keep going
  #We will need to reclassify the question type when we split side-by-side!!
  questions <- map(questions, ~append(.x,list("QuestionTypeHuman" = qtype_human(.x))))
  #Insert user notes into questions as element "qtNotes"
  questions <- insert_notes_into_questions_rv(questions, notes = qtNotesList)
  #Insert skip logic into the questions
  questions <- insert_skiplogic_into_questions(questions, blocks=blocks_notrash)
  #Split side-by-side questions into their components
  #first append side-by-side question components to the list,
  #them remove the original side-by-side question elements
  questions <- split_sbs_questions(questions)
  #Link responses to questions
  questions <- link_responses_to_questions(questions = questions,
                                           responses = responses,
                                           original_first_rows = original_first_rows)
  #Generate a results table for each question
  questions <- generate_results(questions, original_first_rows = original_first_rows)
  #Now get a list of the side-by-side questions
  #This is a named list where the name is the NEW split question ID and the element is the original QID
  sbs_question_qids <- keep(questions,~"SBS_origQID" %in% names(.x))
  sbs_question_qids <- map(sbs_question_qids, "SBS_origQID")

  #Revise the blocks to create space for elements split from side-by-side questions
  blocks <- map(blocks_notrash, ~ split_block_elements(.x,sbs_question_qids))
  #Now insert questions into blocks
  blocks <- map(blocks, ~ insert_questions_into_block(block = .x, questions = questions))

  return(list("questions" = questions, "blocks" = blocks))

}
