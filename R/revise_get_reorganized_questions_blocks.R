
#These are new functions that Emma has written. I want to test them, but am not ready to embed into the full function.

#Pull notes from the survey
note_text_from_survey <- function(survey) {
  #From the survey, keep elements that have user notes; these are identified from the list of survey elements
  #as those with Elements "NT"
  user_notes_text <- keep(survey[["SurveyElements"]], ~.x[["Element"]]=="NT") %>%
    #Now rename the user notes with their QID based on parent ID instead of the primary attribute
    set_names(map_chr(., ~ .x[["Payload"]][["ParentID"]])) %>%
    #From the notes block payload, pluck "Notes" element
    #Now we want to pull only the next for the user notes
    #But only if the element "Removed" is false
    #First keep only the "Notes" section within Payload, which has the information we need
    map(~ pluck(.x, "Payload","Notes")) %>%
    #only keep notes for which the "Removed" status is FALSE; these are notes that have not been deleted
    map(~ keep(.x, ~ !.x[["Removed"]])) %>%
    #Now we want to pluck only the messages, which is the text of the note
    map(~map(.x, ~pluck(.x,"Message"))) %>%
    map(~ flatten(.x)) %>%
    #Prepend each note with the "User note: " text tag
    map(~map(.x,~stringr::str_c("User note: ",.x)))
  return(user_notes_text)
}

###Notes into Questions
#Revise the function that appends a list of questions with the appropriate user notes

insert_notes_into_questions_rv <- function(questions, notes) {
  #For each question, first see if it has corresponding notes.
  #If it does, append the question list with element "qtNotes"
  questions_with_notes <- questions %>%
    purrr::modify_if(~.x[["PrimaryAttribute"]] %in% names(notes),
                     ~ append(.x, list("qtNotes"= pluck(notes,.x[["PrimaryAttribute"]]))))
  return(questions_with_notes)
}


###Add a function for determining Qualtrics Type human readable from the Payload information. This is used intially with quesitons, then again when splitting side-by-side questions into their components. Functionalizing this will allow us to more easily add cases for other questino types as we move forward with development and enhancements.

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


###Revise splitting a single side-by-side question

#This  function will be iterated over questions. A separate step of get_setup will split blocks based on the list of SBS questions.

#This applies to a single side-by-side question
split_side_by_side_q <- function(question) {

  #For a SBS question only
  #There should be a chekc earlier that the side-by-side actually has additional questions
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


  additional_questions <- question[["Payload"]][["AdditionalQuestions"]] %>%
    #Add names based on the QID
    #There's an issue with the "#" symbol, so replace with underscore in the question name; may need to change this later
    set_names(map_chr(.,~ str_replace(.x[["QuestionID"]], "#","_")))

  #Now we want to edit the split questions. In the original QualtricsTools, split quesitons ONLY have Payload and qtNotes
  #We will have to decide at a later date if anything else should be included


  split_q <- additional_questions %>%

    #Assign the additional question information as Payload for the new question element
    map(~ list("Payload" = .x)) %>%
    #Pull qtNotes from the original question and append them to each item
    #Add an additional note that this was split from a side-by-side question
    map(~ rlist::list.append(.x,qt_notes = if_else("qtNotes" %in% names(mainq),
                                                   list(append(mainq[["qtNotes"]],
                                                               "This question was split from a side-by-side question.")),
                                                   list("This question was split from a side-by-side question.")))) %>%
    #Add clean question text to the side-by-side question element
    map(~ rlist::list.append(.x, QuestionTextClean = str_c(mainq_text,"---",.x[["Payload"]][["QuestionText"]]))) %>%

    #Add QualtricsQtype for the side by side; prepend with "SBS" to indicate this came from a side-by-side question
    map( ~ rlist::list.append(.x, Qualtrics_qtype = paste0("SBS_",.x[["Payload"]][["QuestionType"]]))) %>%

    #Add human readable question type
    map(~ rlist::list.append(.x, QuestionTypeHuman = qtype_human(.x))) %>%

    #Add Primary Attribute that corresponds to the list element name
    map(~ rlist::list.append(.x, PrimaryAttribute = str_replace(.x[["Payload"]][["QuestionID"]], "#","_"))) %>%

    #Add a new element indicating that this was split from a side-by-side question
    map( ~ rlist::list.append(.x, SBS_origQID = main_qid))


  return(split_q)

}


###Splitting side by side questions as part of get_setup

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
  questions <- questions %>%
    discard(~ .x[["Qualtrics_qtype"]]=="SBS")
}


###Splitting block elements as part of get_setup

#This function iterates through a block and, where appropriate, splits the block element for a side-by-side question into separate elements for each component of the side-by-side.


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
      split_elements <- keep(sbs_questions, ~.x[["ParentID"]]==qid_orig) %>%
        #Now name them with the Question ID
        set_names(paste0(map_chr(., "QuestionID"),"-Question")) %>%
        #And now remove the parent ID element
        map(~rlist::list.remove(.x,"ParentID"))

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


###Skip logic into questions
#Add skip logic to questions
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


###Insert questions into blocks
insert_questions_into_block <- function(block,questions) {
  if ("BlockElements" %in% names(block)) {
    block[["BlockElements"]] <- modify_if(block[["BlockElements"]],
                                          ~.x[["Type"]]=="Question",
                                          ~pluck(questions,.x[["QuestionID"]]))
  }
  return(block)
}

###Get reorganized questions and blocks

#This is a revised version of the function that is used as the workhorse in get_setup and does a LOT of what is required later on. It will still need to be documented VERY carefully and incorporated more fully into the package.


#This is called by get_setup to pull what we need from survey and responses

get_reorganized_questions_and_blocks_rv <- function(survey,
                                                    responses,
                                                    original_first_rows) {
  #The survey JSON was read in during an early block, so we can just work with this.
  #Extract the survey elements from survey
  survey_elements <- survey_json[["SurveyElements"]] %>%
    # Name each item of survey elements based on the primary attribute.
    # This includes Survey Blocks, Survey Flow, Survey Options, and QIDs.
    # These names should make it much easier for us to get the information we need to extract.
    purrr::set_names(map_chr(., "PrimaryAttribute"))

  qtNotesList <- note_text_from_survey(survey_json)

  #Now extract the blocks; this is really the payload element of element "Survey Blocks"
  #Replace blocks_from_survey
  blocks_all <- purrr::pluck(survey_elements, "Survey Blocks", "Payload") %>%
    #Name the blocks with their descriptive value; this will be more user-friendly
    #Use "." to refer to the piped value within the map_chr function
    purrr::set_names(map_chr(.,"Description"))

  trash_questions <- blocks_all %>%
    #Now pluck just the trash block
    pluck("Trash / Unused Questions", "BlockElements") %>%
    #Pull a character vector of only the QIDs for questions that are in the trash
    map_chr(.,"QuestionID")


  blocks_notrash <- blocks_all %>%
    #Remove trash block based on the "Type" element of the block
    purrr::keep(~ .x[["Type"]]!= "Trash") %>%
    #Name the BlockElements (quesiton items) within the blocks
    map(~ modify_at(.x, "BlockElements", ~set_names(.x, map_chr(.x, ~ str_c(.x[["QuestionID"]],.x[["Type"]],sep="-")))))


  #Now we want to extract the list of questions. These can be identified as SurveyElements that have Element "SQ"
  #Replaces questions_from_survey
  questions <- purrr::keep(survey_elements, ~ .x[["Element"]]=="SQ") %>%
    #Remove questions that are in the trash, as specified by "trash_questions
    discard( ~ .x[["PrimaryAttribute"]] %in% trash_questions) %>%
    #Now clean question text using clean_html
    map(~ append(.x,list("QuestionTextClean" = clean_html(.x[["Payload"]][["QuestionText"]])))) %>%
    #Add the Qualtrics question type; this is for reference and won't be used in results
    map(~append(.x, list("Qualtrics_qtype" = .x[["Payload"]][["QuestionType"]]))) %>%
    #Add human readable question type to each question; this can be edited if we want
    #We will want ot expand this as we keep going
    #We will need to reclassify the question type when we split side-by-side!!
    map(~append(.x,list("QuestionTypeHuman" = qtype_human(.x)))) %>%
    #Now we want to insert notes into questions.
    #Previously side-by-side questions were not getting their notes.
    #This is now happening before the split to ensure that notes are included
    insert_notes_into_questions_rv(notes = qtNotesList) %>%

    #Insert skip logic into the questions
    insert_skiplogic_into_questions(blocks=blocks_notrash) %>%

    #Split side-by-side questions into their components
    #We can do this by first appending split side-by-side questions to the list, then removing the originals
    split_sbs_questions() %>%

    #Link responses to questions
    link_responses_to_questions(responses = responses,original_first_rows = original_first_rows) %>%

    #Generate a results table for each question
    generate_results(original_first_rows = original_first_rows)

  #Now get a list of the side-by-side questions
  #This is a named list where the name is the NEW split question ID and the element is the original QID
  sbs_question_qids<- keep(questions,~"SBS_origQID" %in% names(.x)) %>%
    map("SBS_origQID")

  #Revise the blocks to create space for elements split from side-by-side questions
  blocks <- map(blocks_notrash, ~ split_block_elements(.x,sbs_question_qids)) %>%
    #Now insert questions into blocks
    map(~ insert_questions_into_block(block = .x, questions = questions))

  return(list("questions" = questions, "blocks" = blocks))

}
