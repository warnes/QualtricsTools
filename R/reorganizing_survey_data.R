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
#' replaced by the real objects here. Questions is a named list for which names
#' are data export tags. Blocks containing questions have Block Elements for these
#' questions named with the Data Export Tags.
get_reorganized_questions_and_blocks <- function(survey,
                                                 responses,
                                                 original_first_rows) {
  #The survey JSON was read in during an early block, so we can just work with this.
  valid_questions_blocks <- valid_questions_blocks_from_survey(survey)
  questions <- valid_questions_blocks[["questions"]]
  blocks <- valid_questions_blocks[["blocks"]]
  #Get notes from the survey so we can add these to the questions
  qtNotesList <- note_text_from_survey(survey)
  #Add clean question text and human readable question type
  #Insert notes and skip logic into questions
  questions <- add_question_detail(questions = questions, blocks = blocks, qtNotesList = qtNotesList)
  #Split side-by-side questions into their components and create space for the split questions within these blocks.
  split_questions_blocks <- split_sbs_questions_blocks(questions = questions, blocks = blocks)
  questions <- split_questions_blocks[['questions']]
  blocks <- split_questions_blocks[['blocks']]
  #Link responses to questions
  questions <- link_responses_to_questions(questions = questions,
                                           responses = responses,
                                           original_first_rows = original_first_rows)
  #Generate a results table for each question
  questions <- generate_results(questions, original_first_rows = original_first_rows)

  #Now insert questions into blocks
  blocks <- insert_questions_into_blocks(questions = questions, blocks = blocks)

  # insert the header into the blocks
  blocks[['header']] <- c(paste0("Survey Name: ",
                                 survey[['SurveyEntry']][['SurveyName']]),
                          paste0("Number of Respondents: ",
                                 nrow(responses)))

  return(list("questions" = questions, "blocks" = blocks))

}

#' Pull the valid questions and blocks from a Qualtrics Survey.
#'
#' This pulls the questions and blocks from a QSF survey file.
#' Trash questions are removed from the blocks and the list of questions.
#' Side-by-side questions are split into their component question parts for ease
#' of processing results.
#' @inheritParams get_reorganized_questions_and_blocks
#' @return List with named elements questions and blocks. this includes only valid
#' questions. Trash questions have been removed from both questions and blocks.
#' Questions are named by their data export tags, blocks named by their descriptive names.
#' Block elements for questions are name by the QIDs.
valid_questions_blocks_from_survey <- function(survey) {
  #Extract the survey elements from survey
  survey_elements <- survey[["SurveyElements"]]
  # Name each item of survey elements based on the primary attribute.
  # This includes Survey Blocks, Survey Flow, Survey Options, and QIDs.
  # These names should make it much easier for us to get the information we need to extract.
  survey_elements <- purrr::set_names(survey_elements, purrr::map_chr(survey_elements, "PrimaryAttribute"))

  #Now extract the blocks; this is really the payload element of element "Survey Blocks"
  #Replace blocks_from_survey
  blocks_all <- purrr::pluck(survey_elements, "Survey Blocks", "Payload")
  #Name the blocks with their descriptive value
  blocks_all <- purrr::set_names(blocks_all, purrr::map_chr(blocks_all,"Description"))

  #Now pluck just the trash blocktrash_questions
  trash_questions <- purrr::pluck(blocks_all, "Trash / Unused Questions", "BlockElements")
  #Pull a character vector of only the QIDs for questions that are in the trash
  trash_questions <- purrr::map_chr(trash_questions,"QuestionID")

  #Remove trash block based on the "Type" element of the block
  blocks_notrash <- purrr::keep(blocks_all, ~ .x[["Type"]]!= "Trash")

  #Remove any BlockElements that are NOT survey questions
  #We do this by applying a function over the blocks and keeping the block elements from each block of type "Question"
  blocks_notrash <- lapply(blocks_notrash, function(x) {
    if ("BlockElements" %in% names(x)) {
      if (length(x) > 0) {
        x[["BlockElements"]] <- purrr::keep(x[["BlockElements"]], ~ .x[["Type"]]=="Question")
        return(x)
      }
    }
  })

  #Name the BlockElements (question items) within the blocks
  blocks_notrash <- lapply(blocks_notrash, function(x) {
    names(x[['BlockElements']]) <- paste0(purrr::map_chr(x[['BlockElements']], "QuestionID"),
                                          "-", purrr::map_chr(x[['BlockElements']], "Type"))
    return(x)
  })

  #Extract a list of questions, SurveyElements that have Element "SQ"
  questions <- purrr::keep(survey_elements, ~ .x[["Element"]]=="SQ")
  #Remove questions that are in the trash, as specified by "trash_questions
  questions <- purrr::discard(questions, ~ .x[["PrimaryAttribute"]] %in% trash_questions)
  question_names <- purrr::map_chr(questions, ~ .x[['Payload']][['DataExportTag']])
  #Check that there are no duplicate question names
  if (any(duplicated(question_names))) {
    duplicate_exporttags <- unique(question_names[which(duplicated(question_names))])
    stop("The following Data Export Tags are duplicated in the survey.\n",
                stringr::str_c(duplicate_exporttags, collapse = "; "),
                "\nThese must be unique for the survey to process correctly.")
  }
  #Rename quesions with their Data Export Tags (question variable name) instead of QID
  questions <- purrr::set_names(questions, question_names)
  #Rename the Block elements with variable names

  return(list("questions" = questions, "blocks"=blocks_notrash))
}

#' Add question type, clean text, notes, skip logic to question elements
#'
#' Given a list of questions, blocks, and notes from a survey, this function
#' will add information to the question elements that will be used in later
#' QualtricsTools functions.
#'
#' Information added to these questions are elements within the question list.
#' The earlier version of QualtricsTools added these to the Payload, but it is more easily
#' to access them as named items directly within the quesiton. They are not part of the
#' Payload from Qualtrics, so it seems fine to pull the information from here.
#' @param questions A list of questions extracted from a Qualtrics survey file. See
#' `valid_questions_blocks_from_survey` to create question list.
#' @param blocks A list of blocks extracted from a Qualtrics Survey file. See
#' `valid_questions_blocks_from_survey` to generate this list of blocks.
#' @param qtNotes A list of user notes extracted from a Qualtrics survey file. Use
#' `note_text_from_survey` to create the user note list. Elements of this list
#' are named with the parent QID.
#' @return A list of survey questions with added elements for Question Text cleaned of
#' HTML and CSS, human readable question type, user notes, and skip logic.
add_question_detail <- function(questions, blocks, qtNotesList){
  #Now clean question text using clean_html_and_css
  questions <- purrr::map(questions,
                          ~ clean_question_text(.x) )
  #Add the Qualtrics question type; this is for reference and won't be used in results
  questions <- purrr::map(questions, ~append(.x, list("Qualtrics_qtype" = .x[["Payload"]][["QuestionType"]])))
  #Add human readable question type to each question; this can be edited if we want
  #We will want ot expand this as we keep going
  #We will need to reclassify the question type when we split side-by-side!!
  questions <- purrr::map(questions, ~append(.x,list("QuestionTypeHuman" = qtype_human(.x))))
  #Insert user notes into questions as element "qtNotes"
  questions <- insert_notes_into_questions(questions, notes = qtNotesList)
  #Insert skip logic into the questions
  questions <- insert_skiplogic_into_questions(questions, blocks=blocks)
  return(questions)
}

#' Split side by side questions and corresponding block elements
#'
#' Split side-by-side survey questions into their component questions. Create
#' spaces within the blocks for these split elements.
#' @param questions A list of Qualtrics survey questions that may include side-by-side.
#' @param blocks A list of Qualtrics blocks for which a side-by-side question is a single block element.
#' @return A named list with two elements: "questions" is a list of survey questions
#' with side-by-side questions split into their component questions, and "blocks" is a list
#' of survey blocks within which side-by-side question block elements have been replaced with
#' spaces for each of the component question elements.
split_sbs_questions_blocks <- function(questions, blocks) {
  #first append side-by-side question components to the list,
  #them remove the original side-by-side question elements
  questions <- split_sbs_questions(questions)
  #Now get a list of the side-by-side questions
  #This is a named list where the name is the NEW split question ID and the element is the original QID
  sbs_question_qids <- purrr::keep(questions,~"SBS_origQID" %in% names(.x))
  #Now set the names to be the split QID instead of the split variable names
  sbs_question_qids <- purrr::set_names(sbs_question_qids, purrr::map_chr(sbs_question_qids, "PrimaryAttribute"))
  sbs_question_qids <- purrr::map(sbs_question_qids, "SBS_origQID")
  #Revise the blocks to create space for elements split from side-by-side questions
  blocks <- purrr::map(blocks, ~ split_block_elements(.x,sbs_question_qids))
  return(list("questions" = questions, "blocks" = blocks))
}


#' Clean Question Text
#'
#' Clean Question Text from the QuestionText Element of Payload and return a question
#' with added cleanQuestionText
#'
#' Given a question that includes a Payload element with QuestionText, clean the Question Text
#' of HTML and CSS and return the question with element QuestionTextClean added to the payload.
#' @param question A survey question with Payload element
#' @return The question with QuestionTextClean added to the Payload
clean_question_text <- function(question) {
  question[['Payload']][['QuestionTextClean']] <- clean_html_and_css(question[['Payload']][['QuestionText']])
  return(question)
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

#' Generate a list of user notes
#'
#' Each list element is named with the
#' parent question export tag and contains the survey note text prepended with "User Note: "
#'
#' @inheritParams get_reorganized_questions_and_blocks
#'
#' @return This returns a list of user notes named with the parent question export tag.
note_text_from_survey <- function(survey) {
  #From the survey, keep elements that have user notes; these are identified from the list of survey elements
  #as those with Elements "NT"
  user_notes_text <- purrr::keep(survey[["SurveyElements"]], ~.x[["Element"]]=="NT")
  #Now rename the user notes with their QID based on parent ID instead of the primary attribute
  user_notes_text <- purrr::set_names(user_notes_text, purrr::map_chr(user_notes_text, ~ .x[["Payload"]][["ParentID"]]))
  #From the notes block payload, pluck "Notes" element
  #Now we want to pull only the next for the user notes
  #But only if the element "Removed" is false
  #First keep only the "Notes" section within Payload, which has the information we need
  user_notes_text <- purrr::map(user_notes_text, ~ purrr::pluck(.x, "Payload","Notes"))
  #only keep notes for which the "Removed" status is FALSE; these are notes that have not been deleted
  user_notes_text <- purrr::map(user_notes_text, ~ purrr::keep(.x, ~ !.x[["Removed"]]))
  #Now we want to pluck only the messages, which is the text of the note
  user_notes_text <- purrr::map(user_notes_text, ~ purrr::map(.x, ~ purrr::pluck(.x,"Message")))
  user_notes_text <- purrr::map(user_notes_text, ~ purrr::flatten(.x))
  #Prepend each note with the "User Note: " text tag
  user_notes_text <- purrr::map(user_notes_text, ~ purrr::map(.x,~stringr::str_c("User Note: ",.x)))
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
                                           ~ append(.x, list("qtNotes"= purrr::pluck(notes,.x[["PrimaryAttribute"]]))))
  return(questions_with_notes)
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
          questions <- purrr::modify_at(questions,which(be[["QuestionID"]]==purrr::map_chr(questions,"PrimaryAttribute")),
                                        ~rlist::list.append(.x,SkipLogic = skip_logic))
        }
      }
      return(questions)
    }
  }
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
                          split_side_by_side_q(question = q),
                          after=match(q[['Payload']][["DataExportTag"]],names(questions)))
    }
  }
  #Now remove the original side-by-side questions since we no longer need these
  questions <- purrr::discard(questions, ~ .x[["Qualtrics_qtype"]]=="SBS")
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
  mainq <- purrr::modify_at(question, "AdditionalQuestions", ~NULL)

  #Extract the question text from the main part of the question
  #If we've already run the other parts of the question, this will be the clean question text
  #If not, pull the original question text
  mainq_text <- dplyr::if_else("QuestionTextClean" %in% names(question[['Payload']]),
                               question[['Payload']][["QuestionTextClean"]],
                               question[["Payload"]][["QuestionText"]])

  mainq_exporttag <- mainq[['Payload']][["DataExportTag"]]
  mainq_QID <- mainq[['PrimaryAttribute']]

  additional_questions <- question[["Payload"]][["AdditionalQuestions"]]
  #Add names based on the QID; replace "#" symbol with underscore
  additional_questions <- purrr::set_names(additional_questions ,
                                           purrr::map_chr(additional_questions, ~ stringr::str_replace(.x[["DataExportTag"]], "#","_")))
  #Create a new element for with split questions. These have the information
  #from additional questions supplemented with key information from the main question.
  split_q <- additional_questions
  #Assign the additional question information as Payload for the new question element
  split_q <- purrr::map(split_q, ~ list("Payload" = .x))
  #Pull qtNotes from the original question and append them to each item
  #Add an additional note that this was split from a side-by-side question
  for (i in 1:length(split_q)) {
    split_q[[i]][['qtNotes']] <- if ("qtNotes" %in% names(mainq)) {
      append(mainq[["qtNotes"]],
             "This question was split from a side-by-side question.")
    } else {list("This question was split from a side-by-side question.")}
    }


  split_q <- purrr::map(split_q, ~ .x)
  #Add clean question text to the side-by-side question element

  for (i in 1:length(split_q)) {
    #Now overwright QuestionText with both parts
    split_q[[i]][['Payload']][['QuestionText']] <- stringr::str_c(mainq_text, " -",
                                                                  split_q[[i]][["Payload"]][["QuestionText"]])
    split_q[[i]][['Payload']][['QuestionTextClean']] <- clean_html_and_css(split_q[[i]][["Payload"]][["QuestionText"]])

  }

  #Add QualtricsQtype for the side by side; prepend with "SBS" to indicate this came from a side-by-side question
  split_q <- purrr::map(split_q,
                        ~ rlist::list.append(.x, Qualtrics_qtype = paste0("SBS_",.x[["Payload"]][["QuestionType"]])))
  #Add human readable question type
  split_q <- purrr::map(split_q, ~ rlist::list.append(.x, QuestionTypeHuman = qtype_human(.x)))
  #Add Primary Attribute that corresponds to the list element name
  split_q <- purrr::map(split_q, ~ rlist::list.append(.x, PrimaryAttribute = stringr::str_replace(.x[["Payload"]][["QuestionID"]], "#","_")))
  #Add a new element indicating that this was split from a side-by-side question
  split_q <- purrr::map(split_q, ~ rlist::list.append(.x, SBS_DataExportTag = mainq_exporttag))
  split_q <- purrr::map(split_q, ~ rlist::list.append(.x, SBS_origQID = mainq_QID))


  return(split_q)
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
  if (! length(sbs_question_qids)>=1) {return(block)}

  for (i in 1:length(sbs_question_qids)) {
    sbs_question_entry <- list(Type = "Question",
                               QuestionID = names(sbs_question_qids)[[i]],
                               ParentID = sbs_question_qids[[i]])
    sbs_questions[[i]] <- sbs_question_entry
  }

  for (element in block_elements) {
    #Check that the element has a question ID, then check whether it's in the list
    if ("QuestionID" %in% names(element) &&
        element[["QuestionID"]] %in% purrr::map_chr(sbs_questions,"ParentID")) {

      qid_orig <- element[["QuestionID"]]
      split_elements <- purrr::keep(sbs_questions, ~.x[["ParentID"]]==qid_orig)
      #Now name them with the Question ID
      split_elements <- purrr::set_names(split_elements, paste0(purrr::map_chr(split_elements, "QuestionID"),"-Question"))
      #And now remove the parent ID element
      split_elements <- purrr::map(split_elements, ~rlist::list.remove(.x,"ParentID"))

      block_elements <- append(block_elements,
                               split_elements,
                               after = match(element[["QuestionID"]],stringr::str_replace(names(block_elements),"-Question$","")))
    }
  }

  #And now we want to remove those original question elements
  block_elements <- purrr::discard(block_elements, ~.x[["QuestionID"]] %in% sbs_question_qids)

  block[["BlockElements"]] <- block_elements
  return(block)

}


#' Insert questions into blocks
#'
#' This inserts questions into a single block. It should be iterated over all blocks
#' to fully update the survey.
#' @param questions A list of questions from a Qualtrics survey
#' @param block A single block from a Qualtrics survey
#' @return The single survey block with question elements updated to include the full question. In the setup process,
#' BlockElements within the blocks that correspond to survey questions are initially named with question QIDs. This function
#' renames the Block Elements with question Data Export Tags corresponding to the question list.
insert_questions_into_block <- function(questions, block) {
  if ("BlockElements" %in% names(block)) {
    block[["BlockElements"]] <- purrr::modify_if(block[["BlockElements"]],
                                                 ~.x[["Type"]]=="Question",
                                                 ~ append(questions[[which(.x[['QuestionID']]==purrr::map_chr(questions, "PrimaryAttribute"))]],
                                                          #purrr::pluck(questions,.x[["QuestionID"]]),
                                                          values = c("Type" = "Question",
                                                                     "QuestionID" = .x[["QuestionID"]])))
    #Now rename the block elements with question names
    blockelement_names <- purrr::map_chr(block[['BlockElements']], ~ dplyr::if_else(.x[['Type']]=="Question",
                                                                                    stringr::str_replace(.x[['Payload']][['DataExportTag']],"#","_"),
                                                                                    stringr::str_c(.x[["QuestionID"]], .x[["Type"]],sep="-")))
    block <- purrr::modify_at(block, "BlockElements",
                              ~ purrr::set_names(.x, blockelement_names))

  }
  return(block)
}

#' Iterate over blocks and insert questions to each
#'
#' This iterates over a list of blocks and inserts questions to each of the survey blocks.
#' @param blocks A list of survey blocks
#' @param questions A list of questions
#' @return A list of blocks with the questions inserted into the appropriate block elements

insert_questions_into_blocks <- function(questions, blocks) {
  blocks <- purrr::map(blocks, ~ insert_questions_into_block(block = .x, questions = questions))
  return (blocks)
}


#' Link Responses to Questions
#'
#' The columns of the response data must be matched up to their corresponding
#' questions and question-parts in order to analyze them. One of two methods is employed, depending
#' on whether or not the original_first_rows are from Insights or Legacy data.
#'
#' If Insights data is used, each question is looped through and the QuestionIDs are used to
#' match response columns to a question.
#'
#' Otherwise, a much more complicated process is used:
#' each question is looped through, determining the DataExportTag of the question,
#' and the ChoiceDataExportTags of the question. The DataExportTag might look
#' like a variable name that has been set by the user in Qualtrics, or an
#' automatically generated name like "QID1" or "QID1.1". The columns
#' with names automatically generated by Qualtrics usually are of a form
#' starting with the DataExportTag, followed by an underscore, and then more details.
#' Sometimes they are exactly the DataExportTag without any modification. Lastly,
#' if the user sets their own variable naming in Qualtrics, then the question
#' contains in its [['Payload']] the [['ChoiceDataExportTags']] list, which contains
#' these user defined variables. This function goes through each question,
#' determines the DataExportTag and ChoiceDataExportTags, and uses each to select
#' the matching columns of the responses. Those are then inserted into that specific
#' question under [['Responses']], and the whole questions list is returned. One small
#' caveat is that if the column name starts with an integer, then R
#' will prepend it with an "X", so there is a helper function included here to include
#' those columns with prepended "X"s as well.
#'
#' @param questions A list of questions selected from a Qualtrics survey
#' @param responses A data frame of responses from a Qualtrics survey
#' @param original_first_rows The original header rows to the CSV response set
#'
#' @return The updated list of questions, each including its relevant response columns
#' as a data frame stored in [['Responses']].
link_responses_to_questions <-
  function (questions,
            responses,
            original_first_rows) {
    if (!missing(original_first_rows) &&
        nrow(original_first_rows) >= 2) {
      for (i in 1:length(questions)) {
        question_id <- questions[[i]][['Payload']][['QuestionID']]
        matching_responses <-
          which(grepl(
            paste0(question_id, "$", "|", question_id, "[#_-]+.*$"),
            original_first_rows[2, ],
            perl = TRUE
          ))
        if (length(matching_responses) > 0) {
          matching_responses_names <-
            colnames(original_first_rows)[matching_responses]
          matching_responses <-
            as.data.frame(as.data.frame(responses)[, matching_responses_names])
          colnames(matching_responses) <- matching_responses_names
          questions[[i]][['Responses']] <- matching_responses
        }
      }
    } else if (missing(original_first_rows) ||
               !missing(original_first_rows) && nrow(original_first_rows) < 2) {
      responses <- as.data.frame(responses)
      for (i in 1:length(questions)) {
        # create a string with the data export tag and an underscore
        # create a string with the data export tag and a period
        export_tag_with_underscore <-
          paste0(questions[[i]][['Payload']][['DataExportTag']], "_")
        export_tag_with_period <-
          paste0(questions[[i]][['Payload']][['DataExportTag']], ".")
        export_tag_with_hashtag <-
          paste0(questions[[i]][['Payload']][['DataExportTag']], "#")

        # there's also the possibility that a response column starts
        # with a choice data export tag from a question.
        # this unlists the choice data export tags and creates
        # a list of response columns that start with a choice data export tag.
        starts_with_choice_export_tags <- vector('integer')
        if ("ChoiceDataExportTags" %in% names(questions[[i]][['Payload']])) {
          choice_export_tags <-
            unlist(questions[[i]][['Payload']][['ChoiceDataExportTags']])
          choice_export_tags <-
            sapply(choice_export_tags, function(x)
              gsub("-", "_", x))
          for (j in choice_export_tags) {
            starts_with_choice_export_tags <- c(starts_with_choice_export_tags,
                                                which(gdata::startsWith(names(
                                                  responses
                                                ), j)))
          }
        }

        # the response columns that match a question are the ones that:
        # - start with a data export tag followed by a underscore,
        # - start with a data export tag followed by a period,
        # - match the data export tag exactly,
        # - start with a choice data export tag.
        # take those matching response columns and join them to the question under [['Responses']]
        matching_responses <-
          c(
            which(gdata::startsWith(
              names(responses), export_tag_with_underscore
            )),
            which(gdata::startsWith(
              names(responses), export_tag_with_period
            )),
            which(gdata::startsWith(
              names(responses), export_tag_with_hashtag
            )),
            which(names(responses) == questions[[i]][['Payload']][['DataExportTag']]),
            starts_with_choice_export_tags
          )
        questions[[i]][['Responses']] <-
          as.data.frame(responses[unique(matching_responses)])
      }
    }
    return(questions)
  }


#' Clean HTML and CSS from a string
#'
#' This function uses regex extensively to clean HTML and CSS out of a given text block.
#'
#' This function uses regular expressions to clean HTML and CSS from text. This is used
#' in various places to clean quesiton and choice text.
#' All line breaks and non-break space are replaced with a space character.
#' HTML tags are removed, so HTML formatting will NOT be included in any reports.
#' This also removes CSS based on patterns specific to matrix formatting and row banding
#' used by Tufts OIR.
#' At the end, all leading, trailing and extra whitespace are removed, and repeated
#' whiteshapce is replaced by a single whitespace character.
#'
#' @param text any text string that might contain HTML or whitespace that needs to be
#' stripped.
#' @return text without any html, css or extraneous whitespace, and with breaks replaced by space character.

clean_html_and_css <- function(text) {
  # Removes extra whitespace
  text <- stringr::str_replace_all(text, "\\s+", " ")
  # Replaces all linebreaks and non-breaking space with a space character
  text <- stringr::str_replace_all(text, "<br>|&nbsp;", " ")
  # Cleans HTML tags and Entries and replace with ""
  text <- stringr::str_replace_all(text, "<.*?>|&[# a-z 0-9]*;", "")
  # Removes CSS based tags with .Matrix, .Skin, .SBS
  text<- stringr::str_replace_all(text, ".Matrix.*?\\.c\\d|.Skin.*?\\.c\\d|.SBS.*?\\.c\\d", "")
  # Removing all formatting tags, except piped text
  text <- stringr::str_replace_all(text, "(?<!\\$)\\{.*\\}*|&[# a-z 0-9]*;", " ")
  # Remove leading or trailing whitespace
  text <- stringr::str_replace_all(text, "^\\s+|\\s+$", "")
  # Remove any remaning extra whitespace
  text <- stringr::str_replace_all(text, "\\s+", " ")

  return(text)
}


#' Create a Question Dictionary
#'
#' @param blocks The blocks provided to this function must include questions inserted into
#' the BlockElements. Extract the list of block and the list of questions from the survey
#' with `valid_questions_blocks_from_survey` and add necessary information with `add_question_detail`,
#' then insert questions into the blocks
#' with `insert_questions_into_blocks`.
#'
#' Question dictionaries require that each question already have items for QuestionTextClean
#' and human readable question type. If you have not already prepared questions and blocks for
#' this, use `question_dictionary_from_qsf` to generate a question dictionary from the QSF directly.
#' @param flow A list of strings identifying the blocks in the order that they appear
#' within the survey. Extract this form the survey QSF with `flow_from_survey`.
#' @return A data frame with a row for each question describing the question's details.
create_question_dictionary <- function(blocks, flow) {

  # Determine the ordering of the block indices that we will use to
  # iterate through the blocks.
  if (!missing(flow)) {
    # If flow was specified, use it to order the blocks and store the
    # blocks' ordering in the block_ordering list of indices.
    block_ordering <- list()
    for (h in flow) {
      # For each flow element, try and match it to a block.
      matched_block <- sapply(blocks, function(x) {
        if ('ID' %in% names(x)) {
          return(x[['ID']] == h)
        } else
          return(FALSE)
      })
      if (table(matched_block)['TRUE'] == 1) {
        block_ordering <- c(block_ordering, which(matched_block))
      }
    }
  } else {
    # If no flow is provided, go in order through the blocks.
    block_ordering <- 1:length(blocks)
  }

  # create_entry creates the row for any individual
  # response with the following elements in it:
  # - The data export tag,
  # - "QuestionTextClean", the question text stripped of any HTML strings/entities,
  # - "QuestionTypeHuman", the human readable question type,
  # - "QuestionType", the qualtrics supplied question type,
  # - "Selector", the qualtrics defined question selector
  create_entry <- function(i, j) {
    if (!"SubSelector" %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']])) {
      blocks[[i]][['BlockElements']][[j]][['Payload']][['SubSelector']] <-
        ""
    }
    if (! "QuestionTextClean" %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']]) &
        "QuestiontypeHuman" %in% names(blocks[[i]][['BlockElements']][[j]])) {
      stop("Questions do not contain clean question text and human readable question type.
           Please reprocess your questions and try again.")
    }
    return(c(
      blocks[[i]][['BlockElements']][[j]][['Payload']][['DataExportTag']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTextClean']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['Selector']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['SubSelector']],
      blocks[[i]][['BlockElements']][[j]][['QuestionTypeHuman']]
    ))
  }

  ### loop through each block, then each question,
  # then of the columns of the responses,
  # then each of the entries in each of the response columns,
  # and create an entry using "create_entry"
  entries <- list()
  e <- 0
  for (i in block_ordering) {
    if ('BlockElements' %in% names(blocks[[i]]) &&
        length(blocks[[i]][['BlockElements']]) != 0) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        e <- e + 1
        entries[[e]] <- create_entry(i, j)
      }
    }
  }

  # This function takes a list of rows, all with the same length, and
  # turns them into a data frame. This is almost exactly the same as
  # rbind, except for the fact that this works effectively on a single
  # row whereas rbind does not.
  list_of_rows_to_df <- function(data) {
    nCol <- max(vapply(data, length, 0))
    data <-
      lapply(data, function(row)
        c(row, rep(NA, nCol - length(row))))
    data <-
      matrix(
        unlist(data),
        nrow = length(data),
        ncol = nCol,
        byrow = TRUE
      )
    data.frame(data)
  }

  if (length(entries) > 0) {
    question_dictionary <- list_of_rows_to_df(entries)
    colnames(question_dictionary) <-
      c(
        "Question Export Tag",
        "Question Text",
        "Question Type 1",
        "Question Type 2",
        "Question Type 3",
        "Response Type"
      )
  } else {
    question_dictionary <- NULL
  }
  return(question_dictionary)
}


#' Create Uncodeable Question Dictionary
#'
#' The "uncodeable" questions are the questions that
#' do not have results tables inserted into them. This
#' function is meant to run on a list of blocks that have
#' had questions with their results tables inserted into them.
#' For any that do not have results tables, this function
#' assumes they were not successfully processed, adds them to the
#' list of uncodeable questions, and then returns a
#' question dictionary detailing them.
#'
#' @param blocks A list of blocks with questions that have been
#' processed with generate_results(questions). The questions can be inserted
#' into the blocks from a survey by using questions_into_blocks(questions, blocks).
#' @return A data frame providing the details of the questions that were not
#' successfully processed by generate_results(questions).
uncodeable_question_dictionary <- function(blocks) {
  # loop through each question,
  # and then remove everything that's not a survey question,
  # any questions that have a results table, and any questions
  # that are text entry or descriptive box questions.

  # make sure we run backwards so that we don't
  # move the next question to our current iterator, and then
  # skip it.
  for (i in 1:number_of_blocks(blocks)) {
    if ('BlockElements' %in% names(blocks[[i]]) &&
        length(blocks[[i]][['BlockElements']]) != 0) {
      for (j in length(blocks[[i]][['BlockElements']]):1) {
        if (!("Element" %in% names(blocks[[i]][['BlockElements']][[j]]))) {
          blocks[[i]][['BlockElements']][[j]] <- NULL
        }
        else if ("Element" %in% names(blocks[[i]][['BlockElements']][[j]])) {
          if ("Table" %in% names(blocks[[i]][['BlockElements']][[j]]) ||
              blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] == "TE" ||
              blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] == "DB" ||
              all(grepl("TEXT", colnames(blocks[[i]][['BlockElements']][[j]][['Responses']])))) {
            blocks[[i]][['BlockElements']][[j]] <- NULL
          }
        }
      }
    }
  }

  # we've cut out everything that isn't something that didn't get coded,
  # so now we just create a question dictionary with the remaining
  # results-tables-less questions.
  return(create_question_dictionary(blocks))
}


#' Create a Lookup Table for Response Variables
#'
#' For each variable response in the responses to a question,
#' this function creates a row which contains the variable,
#' its recode value if the question uses recode values, and the
#' corresponding text for the given variable response. The exact
#' format of the output lookup table (dataframe) is dictated
#' by the question type depending on whether it is a
#' multiple answer question, a multiple choice single answer question,
#' or a matrix single answer question. Note that this function
#' only creates a lookup table for each response which appears in the responses.
#'
#' @param question A list of questions extracted from a Qualtrics QSF file with
#' responses inserted into them.
#' @return A dataframe with columns "var", "recode_value", and "text", where each
#' row corresponds to a unique variable response to the question.
create_response_lookup_table <-
  function(question) {
    # Get the list of unique non-text responses
    not_text_columns <- !(grepl("TEXT", colnames(question[['Responses']])))
    relevant_responses <- question[['Responses']][, not_text_columns]
    relevant_responses <- unlist(relevant_responses, use.names=FALSE)
    unique_responses <- unique(relevant_responses)

    # Remove -99 and "" from the list of unique responses
    valid_responses <- as.logical(sapply(unique_responses, function(x) !(x %in% c("-99", ""))))
    unique_responses <- unique_responses[which(valid_responses)]

    # The lookup_table starts as a list, to which we will insert another list for each
    # variable response in the question.
    lookup_table <- list()
    if (is_multiple_answer(question)) {
      # If the question is multiple answer, then set "1" and "" to correspond to
      # Selected and Not Selected.
      lookup_table[['0']] <- list()
      lookup_table[['0']][['var']] <- ""
      lookup_table[['0']][['text']] <- "Not Selected"
      lookup_table[['1']] <- list()
      lookup_table[['1']][['var']] <- "1"
      lookup_table[['1']][['text']] <- "Selected"
    } else if (is_mc_single_answer(question)) {
      # If the question is a multiple choice single answer question:
      has_recode_values <- any("RecodeValues" == names(question[['Payload']]))
      for (r in unique_responses) {
        # Insert a new list for the given variable response r
        i <- length(lookup_table) + 1
        lookup_table[[i]] <- list()
        lookup_table[[i]][['var']] <- r
        # If the question has recode values, lookup r as a recode value,
        # include its recoded value, and replace r with its recoded_value
        if (has_recode_values) {
          recode_value_index <- which(question[['Payload']][['RecodeValues']] == r)
          if (length(recode_value_index) != 0) {
            recoded_value <- names(question[['Payload']][['RecodeValues']])[[as.integer(recode_value_index[[1]])]]
            lookup_table[[i]][['recode_value']] <- recoded_value
            r <- recoded_value
          }
        }
        # Insert the choice text that corresponds to r
        lookup_table[[i]][['text']] <- clean_html_and_css(question[['Payload']][['Choices']][[r]][[1]])
      }
    } else if (is_matrix_single_answer(question)) {
      has_recode_values <- any("RecodeValues" == names(question[['Payload']]))
      for (r in unique_responses) {
        # Insert a new list for the given variable response r
        i <- length(lookup_table) + 1
        lookup_table[[i]] <- list()
        lookup_table[[i]][['var']] <- r
        if (has_recode_values) {
          # If the question has recode values, lookup r as a recode value,
          # include its recoded value, and replace r with its recoded_value
          recode_value_index <- which(question[['Payload']][['RecodeValues']] == r)
          if (length(recode_value_index) != 0) {
            recoded_value <- names(question[['Payload']][['RecodeValues']])[[as.integer(recode_value_index[[1]])]]
            lookup_table[[i]][['recode_value']] <- recoded_value
            r <- recoded_value
          }
        }
        # Matrix questions use "Answers" instead of "Choices" -- look up the text corresponding
        # to r and insert it as r's corresponding "text".
        lookup_table[[i]][['text']] <- clean_html_and_css(question[['Payload']][['Answers']][[r]][[1]])
      }
    }
    # Convert the lookup table from a list to a Dataframe:
    # sapply(lookup_table, c) creates a dataframe with the rows
    # as "var", "recode_value", and "text" which needs to be transposed.
    lookup_table <- data.frame(t(sapply(lookup_table, c)))
    return(lookup_table)
  }


#' Create Long and Lean Response Dictionary
#'
#' lean_responses() creates a data frame where each row corresponds to
#' an individual response to the survey. Each response contains
#' the respondents' id, the response column's name,
#' the variable response, and the coded
#' response.
#' @param question_blocks A list of blocks, with questions inserted in place of the
#' BlockElements representing them.
#' @param survey_responses The responses to the survey, as imported by ask_for_csv()
#' @param include_text_entry A parameter which defaults to FALSE indicating whether or not
#' open ended text responses should be included in the dictionary of lean responses.
#' @return a data frame with each row detailing an individual survey response.
lean_responses <- function(question_blocks, survey_responses, include_text_entry = FALSE) {
  requireNamespace("plyr")
  requireNamespace("dplyr")

  # get the blocks, responses, and original_first_row from the global environment
  if (missing(question_blocks)) {
    blocks <- get("blocks", envir = 1)
  } else {
    blocks <- question_blocks
  }
  if (missing(survey_responses)) {
    responses <- get("responses", envir = 1)
  } else {
    responses <- survey_responses
  }

  # This creates an individual entry corresponding to a
  # variable response: it is a list with the respondent ID,
  # question response column name, and the raw variable response.
  create_entry <-
    function(question,
             responses,
             response_column,
             response_row) {
      return(c(
        # Respondent ID:
        as.character(responses[, 1][[response_row]]),
        # Question Response Column:
        names(question[['Responses']])[[response_column]],
        # Raw Response:
        toString(question[['Responses']][[response_column]][[response_row]])
      ))
    }



  # Create a dictionary_list to store the dataframes for each question in.
  dictionary_list <- list()
  e <- 0
  for (b in 1:length(blocks)) {
    if ('BlockElements' %in% names(blocks[[b]])) {
      for (be in 1:length(blocks[[b]][['BlockElements']])) {
        question <- blocks[[b]][['BlockElements']][[be]]
        # If the question is not text entry, or include_text_entry is set to true,
        # and the question is not a descriptive box, proceed to check for its responses.
        if ((! is_text_entry(question) || include_text_entry) &&
            question[['Payload']][['QuestionType']] != "DB") {
          if ("Responses" %in% names(question)) {
            coln <- ncol(question[['Responses']])
            rown <-
              nrow(question[['Responses']])
            if (coln > 0 && rown > 0) {
              # Dictionary will be first a list which contains entries for each
              # variable response, each made by create_entry.
              dictionary <- list()
              f <- 0
              for (c in 1:coln) {
                if (!grepl("TEXT", colnames(question[['Responses']])[[c]])) {
                  for (r in 1:rown) {
                    f <- f+1
                    # if a block element has responses,
                    # for each response increment the dictionary index e once,
                    # and try to add to the dictionary the entry for that
                    # question. If creating the entry fails, return to the
                    # console a message saying

                    dictionary[[f]] <-
                      tryCatch(
                        create_entry(
                          question = question,
                          responses = responses,
                          response_column = c,
                          response_row = r
                        ),
                        error = function(e) {
                          cat(
                            paste0(
                              "\nCreating an entry for the following question failed. \nDataExportTag: ",
                              question[['Payload']][['DataExportTag']],
                              "\nResponse Column: ",
                              c,
                              "\nResponse Row: ",
                              r
                            )
                          )
                          return(NULL)
                        }
                      )

                  }
                }
              }
              if (length(dictionary) != 0) {
                e <- e + 1
                # Turn dictionary into a dataframe and name its columns
                df <- data.frame(t(sapply(dictionary, c)))
                colnames(df) <- c("Respondent ID",
                                  "Question Response Column",
                                  "Raw Response")
                # Use dplyr to filter out blank responses
                df <- dplyr::filter(df, `Raw Response` != "")
                # Create a response lookup table
                response_lookup <-
                  create_response_lookup_table(question)
                # Format and merge the response lookup table into the dictionary
                if (length(response_lookup) != 0) {
                  # Select only the "var" and "text" columns from the response_lookup dataframe
                  #First check to see if there has been an error creating the lookup
                  #This should be fixed in create_response_lookup_table(question) ideally!!
                  if(! ("var" %in% names(response_lookup) & "text" %in% names(response_lookup))) {
                    stop("\nError creating a response lookup table for the following question. ",
                         "\nData Export Tag: ", question[['Payload']][['DataExportTag']],
                         "\nSee blocks[[",b,"]][['BlockElements']][[",be,"]]")
                  }

                  response_lookup <- response_lookup[, c("var", "text")]
                  # Rename the "text" column to "Coded Responses" so that it appears next to
                  # "Raw Response" with the correct name in the dataframe for this question
                  colnames(response_lookup)[2] <- "Coded Response"
                  # Make sure that the columns which we're trying to merge based on are of the
                  # same structure, otherwise dplyr will complain.
                  response_lookup[[1]] <- as.character(response_lookup[[1]])
                  response_lookup[[2]] <- as.character(response_lookup[[2]])
                  df[['Raw Response']] <- as.character(df[['Raw Response']])
                  # Left join, so that we keep anything from df and we insert columns from
                  # response_lookup on the right.
                  df <- dplyr::left_join(df, response_lookup, by=c("Raw Response" = "var"))
                }
                # Save this dataframe with the others
                dictionary_list[[e]] <- df
              }
            }
          }
        }
      }
    }
  }

  # plyr's (NOT DPLYR) ldply function splits a list, applies the given function,
  # and then returns the results in a dataframe.
  df <- plyr::ldply(dictionary_list, data.frame)
  # Rename the columns again, because ldply turns "Raw Response" -> "Raw.Response"
  colnames(df) <- c(
    "Response ID",
    "Question Response Column",
    "Raw Response",
    "Coded Response")
  # Remove all NULL values in the "Coded Response" column.
  df[['Coded Response']] <- unlist(lapply(df[['Coded Response']], function(x) ifelse( is.null(x), "", x)))
  return(df)
}

#' Get the Survey Respondents Answers from a Specific Response Column
#'
#' This function is to help in selecting the response data to a specific response
#' column. It selects that data from the lean_responses data (if it's available),
#' or the responses data frame. If it selects the response data from the lean_responses
#' data frame, the returned data frame includes a "Raw Response" and a "Coded Response"
#' column. If not, it includes exactly the response column as it appears in the responses.
#'
#' @param response_column The name of a response column that appears in the response set.
#' @param responses the data frame of responses
#' @param lean_responses responses reshaped with the lean_responses() function
#' @param question_dict a data frame with each question response column, created by
#' the create_response_column_dictionary() function
#' @return a data frame with 2-3 columns, the first being "Respondent ID", the next 1-2 being the
#' response data for each respondent.
answers_from_response_column <-
  function(response_column,
           responses,
           lean_responses,
           question_dict) {
    # if the lean_responses are included as an argument, and the response_column appears
    # in the "Question Response Column" -- use the lean_responses
    if (!missing(question_dict) &&
        !missing(lean_responses) &&
        response_column %in% lean_responses[[2]]) {
      # select the respondent ID, raw response, and coded response
      # from the lean_responses data frame
      selected_df <-
        lean_responses[lean_responses[[2]] == response_column,
                       c(1, 3, 4)]

      # paste the question stem and question choice together from the
      # question dictionary to create the column names
      names(selected_df) <- c(
        "Respondent ID",
        paste0("Raw Response: ",
               question_dict[question_dict[[2]] == response_column, 3][[1]],
               question_dict[question_dict[[2]] == response_column, 4][[1]]),
        paste0("Coded Response: ",
               question_dict[question_dict[[2]] == response_column, 3][[1]],
               question_dict[question_dict[[2]] == response_column, 4][[1]])
      )

      # otherwise, use the responses data frame directly
    } else {
      selected_df <-
        responses[c(1, which(names(responses) == response_column))]
      names(selected_df) <- c("Respondent ID", response_column)
    }

    return(selected_df)
  }


#' Return a list of a Question's Display Logic Components
#'
#' For each question, if they appear, go through the
#' Question's DisplayLogic, each Choice's DisplayLogic, and
#' each Answer's DisplayLogic. For each of them, use clean_html_and_css
#' to format them, and then add them to the list. If a question
#' has any of these display logic components, insert before
#' adding any display logic a line detailing what part of the
#' question the following display logic corresponds to.
#'
#' @param question A qualtrics survey question
#' @return an ordered list of display logic messages
display_logic_from_question <- function(question) {
  # display_logic is a list for storing display logic messages,
  # e will be the index we use to increment as we add to display_logic.
  display_logic <- list()
  e <- 1

  # if there is "DisplayLogic" in the question's payload,
  # add a message saying "Question Display Logic:", and then increment once.
  # Next, since DisplayLogic has many components, not all of which we are looking
  # to examine, we select the elements that are numeric.
  # DisplayLogic looks something like this:
  # question[['Payload']][['DisplayLogic']]$`0`$`1`[['Description']]
  # Determining the first and second indices within the DisplayLogic is the goal
  # of the operations used to define the dl_indices_1 and dl_indices_2.
  if ("DisplayLogic" %in% names(question[['Payload']])) {
    display_logic[[e]] <- "Question Display Logic:"
    e <- e + 1
    dl_indices_1 <-
      suppressWarnings(which(!is.na(as.numeric(
        names(question[['Payload']][['DisplayLogic']])
      ))))
    for (i in dl_indices_1) {
      dl_indices_2 <-
        suppressWarnings(which(!is.na(as.numeric(
          names(question[['Payload']][['DisplayLogic']][[i]])
        ))))
      for (j in dl_indices_2) {
        if ("Description" %in% names(question[['Payload']][['DisplayLogic']][[i]][[j]])) {
          display_logic[[e]] <-
            clean_html_and_css(question[['Payload']][['DisplayLogic']][[i]][[j]][['Description']])
          e <- e + 1
        }
      }
    }
  }

  # we do the same process for the
  # choices, but including a message before each display logic describing which
  # choice it corresponds to.
  if ("Choices" %in% names(question[['Payload']])) {
    choices_with_logic <-
      sapply(question[['Payload']][['Choices']], function(x)
        "DisplayLogic" %in% names(x))
    has_choice_logic <- any(choices_with_logic)
    if (has_choice_logic) {
      choices_with_logic <- which(choices_with_logic)
      e <- e + 1
      for (i in choices_with_logic) {
        display_logic[[e]] <-
          paste0("Choice Display Logic for ", question[['Payload']][['Choices']][[i]][['Display']], ":")
        e <- e + 1
        dl_indices_1 <-
          suppressWarnings(which(!is.na(as.numeric(
            names(question[['Payload']][['Choices']][[i]][['DisplayLogic']])
          ))))
        for (j in dl_indices_1) {
          dl_indices_2 <-
            suppressWarnings(which(!is.na(as.numeric(
              names(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]])
            ))))
          for (k in dl_indices_2) {
            if ("Description" %in% names(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]][[k]])) {
              display_logic[[e]] <-
                clean_html_and_css(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]][[k]][['Description']])
              e <- e + 1
            }
          }
        }
      }
    }
  }

  # for the answers, we do the exact same as the choices.
  if ("Answers" %in% names(question[['Payload']])) {
    answers_with_logic <-
      sapply(question[['Payload']][['Answers']], function(x)
        "DisplayLogic" %in% names(x))
    has_answer_logic <- any(answers_with_logic)
    answers_with_logic <- which(answers_with_logic)
    if (has_answer_logic) {
      e <- e + 1
      for (i in answers_with_logic) {
        display_logic[[e]] <-
          paste0("Choice Display Logic for ", question[['Payload']][['Answers']][[i]][['Display']], ":")
        e <- e + 1
        dl_indices_1 <-
          suppressWarnings(which(!is.na(as.numeric(
            names(question[['Payload']][['Answers']][[i]][['DisplayLogic']])
          ))))
        for (j in dl_indices_1) {
          dl_indices_2 <-
            suppressWarnings(which(!is.na(as.numeric(
              names(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]])
            ))))
          for (k in dl_indices_2) {
            if ("Description" %in% names(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]][[k]])) {
              display_logic[[e]] <-
                clean_html_and_css(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]][[k]][['Description']])
              e <- e + 1
            }
          }
        }
      }
    }
  }

  return(display_logic)
}


#' Split Respondents by a Response Column
#'
#' This function splits the respondents into separate respondent groups
#' based on the values in the specified response column. Then, for each
#' respondent group, the blocks with questions are duplicated. Each set of
#' blocks has a different responent group inserted into its questions, and
#' then each set of blocks is processed. The output is a list of blocks with
#' the results processed and inserted into each BlockElement.
#'
#' @param response_column The response column that will be used to split the respondents
#' @param already_loaded This can be set to TRUE to indicate that the survey and responses
#' should be sourced from the global scope; in other words that the survey and its responses
#' have are "already loaded."
#' @inheritParams get_reorganized_questions_and_blocks
#' @inheritParams create_question_dictionary
#'
#' @return A list of a list of blocks. The same question, but with different respondent groups,
#' might look something like split_blocks[[1]][[1]][['BlockElements']][[1]] and
#' split_blocks[[2]][[1]][['BlockElements']][[1]]. These refer to the first and second respondent
#' groups, the first block, and the first block element.
split_respondents <-
  function(response_column,
           responses,
           survey,
           blocks,
           questions,
           headerrows,
           already_loaded,
           original_first_rows) {
    if (missing(headerrows)) {
      headerrows <- 3
    }
    if (missing(already_loaded)) {
      already_loaded <- FALSE
    }

    if (already_loaded != TRUE) {
      if (missing(responses)) {
        try(responses <<- ask_for_csv(headerrows = headerrows))
      }
      if (missing(survey)) {
        try(survey <<- ask_for_qsf())
      }
    }

    if (already_loaded == TRUE) {
      if (!exists("survey", where = globalenv())) {
        survey <- sample_survey
      } else {
        survey <- get(x = "survey", envir = globalenv())
      }

      if (!exists("responses", where = globalenv())) {
        responses <- sample_responses
      } else {
        responses <- get("responses", envir = globalenv())
      }

      if (!exists("blocks", where = globalenv())) {
        # process the blocks and questions as per usual
        valid_questions_blocks <- valid_questions_blocks_from_survey(survey)
        questions <- valid_questions_blocks[["questions"]]
        blocks <- valid_questions_blocks[["blocks"]]
        #Get notes from the survey so we can add these to the questions
        qtNotesList <- note_text_from_survey(survey)
        #Add clean question text and human readable question type
        #Insert notes and skip logic into questions
        questions <- add_question_detail(questions = questions, blocks = blocks, qtNotesList = qtNotesList)
        #Split side-by-side questions into their components and create space for the split questions within these blocks.
        split_questions_blocks <- split_sbs_questions_blocks(questions = questions, blocks = blocks)
        questions <- split_questions_blocks[['questions']]
        blocks <- split_questions_blocks[['blocks']]
      } else {
        blocks <- get(x = "blocks", envir = globalenv())
        questions <- get(x = "questions", envir = globalenv())
      }
    }

    # For each question, check if its notes contain the "Denominator Used:"
    # substring. If a note on a question does include this substring,
    # remove it, because the "Denominators Used" will be recalculated when
    # the split blocks' questions are processed.
    for (i in 1:length(questions)) {
      if ('qtNotes' %in% names(questions[[i]])) {
        j = 1
        while (j <= length(questions[[i]][['qtNotes']])) {
          if (grepl('Denominator Used:', questions[[i]][['qtNotes']][[j]])) {
            questions[[i]][['qtNotes']] <- questions[[i]][['qtNotes']][-j]
            j <- j - 1
          }
          j <- j + 1
        }
      }
    }

    # split the respondents by their responses to in the response_column
    split_responses <-
      split(responses, responses[response_column], drop = TRUE)


    # insert the header into the blocks
    blocks[['header']] <- c(
      paste0("Survey Name: ",
             survey[['SurveyEntry']][['SurveyName']]),
      paste0("Total Number of Original Respondents: ",
             nrow(responses))
    )

    # duplicate the blocks and questions once for every respondent group
    split_blocks <- rep(list(blocks), times = length(split_responses))
    split_questions <-
      rep(list(questions), times = length(split_responses))

    # for each of the respondent groups, insert the responses into the
    # questions for that respondent group, generate the question's results,
    # and then insert the questions into the blocks for that respondent group.
    for (i in 1:length(split_responses)) {
      split_questions[[i]] <-
        link_responses_to_questions(split_questions[[i]], split_responses[[i]], original_first_rows)
      split_questions[[i]] <-
        generate_results(split_questions[[i]], original_first_rows)
      split_blocks[[i]] <-
        insert_questions_into_blocks(questions = split_questions[[i]], blocks = split_blocks[[i]])
      split_blocks[[i]][['header']] <-
        c(
          split_blocks[[i]][['header']],
          paste0(
            "Respondents with ",
            names(split_responses)[[i]],
            " in the ",
            response_column,
            " column"
          ),
          paste0("Size of Respondent Group: ",
                 nrow(split_responses[[i]]))
        )
      split_blocks[[i]][['split_group']] <-
        names(split_responses)[[i]]
    }
    split_blocks <- purrr::set_names(split_blocks, names(split_responses))
    return(split_blocks)
  }


#' Create a Response Column Dictionary
#'
#' By providing the questions structured by their blocks and the original first
#' row of the response CSV from Qualtrics, this function is able to create a
#' dictionary where each row is an entry for a response column. The response
#' columns are listed along with the data export tag of the question they correspond
#' to, their question stem and question choice, the question types (1, 2, and 3 levels
#' of question types!), and the response type.
#'
#' @param question_blocks use questions_into_blocks() to create a list of blocks with
#' the survey's questions inserted appropriately into them.
#' @param original_first_row this is the first row of the response data CSV, it is
#' automatically provided to you when you use get_setup() or in the shiny application.
#' @inheritParams create_question_dictionary
#' @return a dataframe detailing in each row the response columns and their description.
create_response_column_dictionary <-
  function(question_blocks, original_first_row, flow) {
    # get the blocks, responses, and original_first_row from the global environment
    if (missing(question_blocks)) {
      blocks <- get("blocks", envir = 1)
    } else {
      blocks <- question_blocks
    }
    if (missing(original_first_row)) {
      original_first_row <- get("original_first_rows", envir = 1)
    }

    # Determine the ordering of the block indices that we will use to
    # iterate through the blocks.
    if (!missing(flow)) {
      # If flow was specified, use it to order the blocks and store the
      # blocks' ordering in the block_ordering list of indices.
      block_ordering <- list()
      for (h in flow) {
        # For each flow element, try and match it to a block.
        matched_block <- sapply(blocks, function(x) {
          if ('ID' %in% names(x)) {
            return(x[['ID']] == h)
          } else
            return(FALSE)
        })
        if ('TRUE' %in% names(table(matched_block))) {
        if (table(matched_block)['TRUE'] == 1) {
          block_ordering <- c(block_ordering, which(matched_block))
        }
        }
      }
    } else {
      # If no flow is provided, go in order through the blocks.
      block_ordering <- 1:length(blocks)
    }

    # this create_entry function returns an entry (a row)
    # to be used in the lean_responses output.
    create_entry <-
      function(question,
               response_column,
               original_first_row) {
        # make sure that the subselector either exists or is set to "", so that
        # including it in an entry doesn't error
        if (!("SubSelector" %in% names(question[['Payload']]))) {
          question[['Payload']][['SubSelector']] <- ""
        }

        # get the choice text and append it to the question text based on the
        # response column and the original_first_row entry in that column
        rcol <- names(question[['Responses']])[[response_column]]
        choice_text <-
          choice_text_from_response_column(rcol, original_first_row, blocks)

        return(
          c(
            # Question Data Export Tag:
            question[['Payload']][['DataExportTag']],
            # Question Response Column:
            names(question[['Responses']])[[response_column]],
            # Question Stem:
            question[['Payload']][['QuestionTextClean']],
            # Question Choice:
            choice_text,
            # Question Type 1:
            question[['Payload']][['QuestionType']],
            # Question Type 2:
            question[['Payload']][['Selector']],
            # Question Type 3:
            question[['Payload']][['SubSelector']],
            # Response Type:
            question[['QuestionTypeHuman']]
          )
        )
      }

    # create a dictionary as a list to store row-entries in.
    # for each block element, try to create an entry and add it
    # to the dictionary.
    # TODO: does this fail well?
    dictionary <- list()
    e <- 0
    for (b in block_ordering) {
      if ('BlockElements' %in% names(blocks[[b]])) {
        for (be in 1:length(blocks[[b]][['BlockElements']])) {
          if ("Responses" %in% names(blocks[[b]][['BlockElements']][[be]]) &&
              !is.null(blocks[[b]][['BlockElements']][[be]][['Responses']])) {
            coln <- ncol(blocks[[b]][['BlockElements']][[be]][['Responses']])
            rown <-
              nrow(blocks[[b]][['BlockElements']][[be]][['Responses']])
            if (coln > 0) {
              for (c in 1:coln) {
                # if a block element has responses,
                # for each response column increment the dictionary index e once,
                # and try to add to the dictionary the entry for that
                # response column. If creating the entry fails, return to the
                # console a message saying so.
                e <- e + 1
                dictionary[[e]] <-
                  tryCatch(
                    create_entry(
                      question = blocks[[b]][['BlockElements']][[be]],
                      response_column = c,
                      original_first_row = original_first_row
                    ),
                    error = function(e) {
                      cat(
                        paste0(
                          "\nCreating an entry for the following question failed. \nDataExportTag: "
                          ,
                          blocks[[b]][['BlockElements']][[be]][['Payload']][['DataExportTag']]
                          ,
                          "\nResponse Column: "
                          ,
                          c
                        )
                      )
                      return(NULL)
                    }
                  )
              }
            }
          }
        }
      }
    }

    # list_of_rows_to_df turns the rows into a data frame
    dictionary <- do.call(rbind.data.frame, dictionary)

    # rename the dictionary with the appropriate column names
    if (ncol(dictionary) == 8) {
      names(dictionary) <- c(
        "Question Data Export Tag",
        "Question Response Column",
        "Question Stem",
        "Question Choice",
        "Question Type 1",
        "Question Type 2",
        "Question Type 3",
        "Response Type"
      )
    }

    return(dictionary)
  }


#'Create a response column dictionary with only the qsf
#'
#'The user provides the survey qsf, and this creates a response column dictionary
#'The data .csv is not required
#'This can be used to identify quesiton types in the original survey, regardless
#'of whether you changed questions for a shell survey.
create_question_dictionary_from_qsf <- function(qsf_path) {
  survey <- ask_for_qsf(qsf_path)
  valid_questions_blocks <- valid_questions_blocks_from_survey(survey)
  questions<- valid_questions_blocks[['questions']]
  blocks <- valid_questions_blocks[['blocks']]
  #Pull QualtricsTools notes from the list
  qtNotesList <- note_text_from_survey(survey)
  #Add clean question text and human readable question type
  #Insert notes and skip logic into questions
  questions <- add_question_detail(questions = questions, blocks = blocks, qtNotesList = qtNotesList)
  #Split side-by-side questions into their components and create space for the split questions within these blocks.
  split_questions_blocks <- split_sbs_questions_blocks(questions = questions, blocks = blocks)
  questions <- split_questions_blocks[['questions']]
  blocks <- split_questions_blocks[['blocks']]
  #Now insert questions into blocks
  blocks <- insert_questions_into_blocks(questions = questions, blocks = blocks)

  flow <- flow_from_survey(survey)
  qdict <- create_question_dictionary(blocks,flow)
  return(qdict)
}

#' Create Panel Data for Reshaped Data
#'
#' The user provides this function a character vector of names of response columns (
#' as the panel_columns var), along with the response set data frame, and the returned
#' data is a data frame with respondent IDs and column(s) for each column specified.
#' If the optional parameters, lean_responses and question_dict are provided, then
#' the function will use them to include a "Raw Response" and a "Coded Response"
#' column for any columns which are question response columns.
#'
#' @param panel_columns a list of names of columns desired for inclusion
#' @param survey_responses a response set data frame imported from the Qualtrics CSV responses
#' @param lean_responses an optionally included data frame generated by the lean_responses() function
#' @param question_dict an optionally included data frame generated by the create_response_column_dictionary() function
create_panel_data <-
  function(panel_columns,
           survey_responses,
           lean_responses,
           question_dict) {
    # if the user doesn't provide a response set, try to grab 'responses' from the global scope
    if (missing(survey_responses)) {
      responses <- get("responses", envir = 1)
    } else {
      responses <- survey_responses
    }

    # if the user didn't provide panel_columns,
    # let's not waste anybody's time.
    if (missing(panel_columns))
      return(NULL)
    if (length(panel_columns) == 0)
      return(NULL)

    # initialize a list for us to store data frames in, panel_df.
    panel_df <- list()

    # for each panel column specified by the user, use answers_from_response_column() to generate
    # a data frame with the respondent IDs and response column(s) for that panel column.
    for (i in 1:length(panel_columns)) {
      if (!missing(question_dict) &&
          !missing(lean_responses)) {
        # if the user included a question_dict and a lean_responses dataframe, use them
        # to get a "Raw Response" and "Coded Response" column in the output of answers_from_response_column()
        panel_df[[i]] <-
          answers_from_response_column(panel_columns[[i]],
                                       responses,
                                       lean_responses,
                                       question_dict)
      } else {
        # otherwise, just use the responses data frame
        panel_df[[i]] <-
          answers_from_response_column(panel_columns[[i]], responses)
      }
    }

    # from our list of data panel_column based data frames,
    # either use merge_recurse to get one data frame,
    # or select the only data frame there.
    if (length(panel_columns) > 1) {
      panel_df <- reshape::merge_recurse(panel_df)
    } else if (length(panel_columns) == 1) {
      panel_df <- panel_df[[1]]
    }

    return(panel_df)
  }


#' Add a Custom Response Column with Merged Contents
#'
#' The create_merged_response_column() function allows a user to
#' create a response set with a column reflecting the combined
#' contents of several different columns. If the user specifies
#' response columns that are part of the survey question responses,
#' then the responses are coded and the coded values are used
#' instead of the variable response values. The column is appended to
#' the response set and the response set with the additional
#' new column is returned to the user.
#'
#' @param response_columns a list of names of response columns
#' @param col_name the desired name for the added output column
#' @param question_blocks a list of blocks with questions inserted
#' in place of the BlockElements.
#' @param survey_responses the data frame of responses as imported by
#' ask_for_csv()
#' @return a response data frame with a single new column which
#' represents the contents of the specified response_columns.
create_merged_response_column <- function(response_columns,
                                          col_name,
                                          question_blocks,
                                          survey_responses) {
  # if the user doesn't include the question_blocks parameter,
  # grab the "blocks" from global scope
  if (missing(question_blocks) && exists("blocks", where = -1)) {
    blocks <- get("blocks", envir = -1)
  } else if (!missing(question_blocks)) {
    blocks <- question_blocks
  }

  # if the user doens't include the survey_responses parameter,
  # grab the "responses" from the global scope
  if (missing(survey_responses) && exists("responses", where = -1)) {
    responses <- get("responses", envir = -1)
  } else if (!missing(survey_responses)) {
    responses <- survey_responses
  }

  # remove any response columns that don't make sense
  for (i in length(response_columns):1) {
    if (!response_columns[[i]] %in% names(responses)) {
      cat(paste0(
        response_columns[[i]],
        " doesn't appear in the
        response columns' names"
      ))
      response_columns[[i]] <- NULL
    }
  }

  # if there aren't any response columns to merge, exit
  if (length(response_columns) == 0)
    return(NULL)

  # for each of the names in "response_columns" get a
  # response column.
  # to do so, check if the response column can be corresponded
  # to a question. if so, use the question to get the coded response
  # column. if it can't be corresponded with a question, use the
  # response column directly.
  to_be_merged <- list()
  for (i in 1:length(response_columns)) {
    merge_col_name <- response_columns[[i]]
    question_indices <-
      tryCatch(question_from_response_column(blocks, merge_col_name),
               error = function(e) return(NULL))
    response_col <- as.vector(responses[[merge_col_name]])

    if (!is.null(question_indices)) {
      question <-
        blocks[[question_indices[[1]]]][['BlockElements']][[question_indices[[2]]]]
      should_convert <- !is_text_entry(question)
      converted <-
        lapply(response_col, function(x)
          choice_text_from_question(question, x))
      should_convert <- should_convert && !all(converted == "")
    } else
      should_convert <- FALSE


    if (should_convert) {
      to_be_merged[[i]] <- converted
    } else {
      to_be_merged[[i]] <- response_col
    }
  }

  # for each row of the responses,
  # get a list of the responses across the response columns selected.
  # then take that list and collapse it to a single string, and save that
  # as the merged response
  merged_col <- list()
  for (i in 1:length(response_col)) {
    to_merge <-
      lapply(1:length(to_be_merged), function(x)
        to_be_merged[[x]][[i]])
    merged_col[[i]] <- paste(to_merge, collapse = " + ")
  }

  # save the original rownames of the responses,
  # cbind the new column in,
  # if there was an output column name provided, use it to name the added column
  # and replace the rownames with the original rownames
  orig_rownames <- rownames(responses)
  responses <- cbind(responses, t(as.data.frame(merged_col)))
  if (!missing(col_name))
    names(responses)[length(names(responses))] <- col_name
  rownames(responses) <- orig_rownames

  return(responses)
  }
