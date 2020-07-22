#' Turn a Directory into a list of Coded Comment Data Frames (unprocessed)
#'
#' This function takes as an argument a string representative of a
#' directory, loads the CSVs and Excel data from that directory,
#' extracts the coded sheets,and saves the coded comment table and the question ID
#' as a pair in the output coded_appendix_tables list. If there
#' are sheets which contain non-numeric data, warnings are raised.
#' @param directory A string path to the directory containing the coded comments
#' sheets, which are formatted as described in the Wiki.
#' https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding
#' @param code_type type of coded comment data export. The default is
#' to use NVivo crosstab export with the ResponseID in the first row and
#' second column labeled with the varnmae and containing response presence
#' 1/0 indicator. For old filemaker pro format, use "fmp" specification.
#' (\code{"nvivo"} or \code{"fmp"})
#' @return A list of dataframes for each sheet of coded comments.
directory_get_coded_comment_sheets <- function(directory, code_type) {
  # ask for directory if not provided
  if (missing(directory))
    directory <- choose.dir(caption = "Select coded comment directory")
  if (missing(code_type)) {
    code_type <- readline(prompt = "Specify the format of your coded comment exports, nvivo or fmp: ")
  }

  # we only want to look at Excel or CSV files in the given directory
  files_list <- list.files(path = directory, full.names = TRUE)
  # Filter for only xlsx, xls, and csv files
  files_list <-
    files_list[lapply(files_list, function(x)
      grepl("*.xlsx$|*.xls$|*.csv$", x)) == TRUE]
  # Exclude any temporary files
  files_list <-
    files_list[lapply(files_list, function(x)
      grepl("^~", basename(x))) == FALSE]
  if (length(files_list) == 0) {
    stop("The specified directory did not contain any xlsx, xls, or csv files.")
  }


  # If there are warnings in reading in the Excel sheets,
  # Save them as we go. Additionally, save the corresponding sheet's filenames.
  # If everything goes smoothly, we will construct a list of sheets which
  # contain coded comments, called the coded_appendix_tables
  warnings_list <- list()
  warning_files_list <- list()
  coded_appendix_tables <- list()

  for (i in 1:length(files_list)) {
    # For each file in the files_list, try to get its coded comment sheet.
    # If it warns, save the error and filename to warnings_list and warning_files_list.
    #This will execute a different function depending on coded comment export type
    tryCatch(
      coded_appendix_tables[[length(coded_appendix_tables) + 1]] <-
        get_coded_comment_sheet(codedfile = files_list[[i]], code_type = code_type),
      warning = function(w) {
        warning_files_list[[i]] <- files_list[[i]]
        warnings_list[[i]] <- w
      }
    )

  }

  # Subset the warnings_list and warning_files_list's to only include non-empty entries
  warnings_list <- warnings_list[lapply(warnings_list, length) != 0]
  warning_files_list <-
    warning_files_list[lapply(warning_files_list, length) != 0]

  # Print the first warning for each sheet
  if (length(warning_files_list) > 0) {
    print("There were errors getting the coded comment sheets")
    print("Theses are the following files that had errors and the first error message for each.")

    for (i in 1:length(warning_files_list)) {
      # print the filename the warning is generated from
      print(warning_files_list[[i]])

      # If it was an 'expecting numeric' message, include a message explaining
      # that it's an issue with the typing of data in the columns in the Excel sheets
      numeric_message <- "expecting numeric: got"
      if (grepl(numeric_message, (warnings_list[[i]]))) {
        print("Some of the cells that should be stored as numeric are stored as strings")
        print("Please go back into the file and change them to numeric")
        print("Here is the first error message:")
      }

      # print the warning
      print(warnings_list[[i]])
    }

    # if warnings were returned, return NULL
    return(NULL)
  }

  return(coded_appendix_tables)
}


#' Turn a Single Coded File into a Data Frame
#'
#' This retreives comment coding data as a dataframe from an excel
#' or .csv file. Use code_type to specify the format of coded comment data:
#' NVivo crosstab export (current) or  Filemaker Pro exports (legacy).
#' This is a wrapper function that calls get_coded_comment_sheet_fmp
#' or get_coded_comment_sheet_nvivo based on code_type.
#'
#' @param codedfile The string path to a Excel file.
#' @param codetype Type of coded comment data export. The default is
#' to use NVivo crosstab export with the ResponseID in the first row and
#' second column labeled with the varnmae and containing response presence
#' 1/0 indicator. For old filemaker pro format, use "fmp" specification.
#' (\code{"nvivo"} or \code{"fmp"})
#' @return A dataframe version of the contents of the coded comments
#' in the codedfile.

get_coded_comment_sheet <- function(codedfile, code_type) {
  if (code_type == "nvivo") {
    coded_use <- get_coded_comment_sheet_NVivo(codedfile = codedfile)
  } else if (code_type == "fmp") {
    coded_use <- get_coded_comment_sheet_fmp(codedfile = codedfile)
  }
  else {
    stop("The specification of code_type is not fmp or nvivo and is not supported by Qualtricstools.")
  }
  return(coded_use)
}


#' Turn a Single Coded File into a Data Frame
#'
#' This retrieves comment coding data as a dataframe
#' from a Excel file. Coded comments are retrieved from the sheet named "Coded"
#' (case does not matter). This works with the legacy coded comment format
#' based on exports from Filemaker Pro.
#' @param codedfile The string path to a Excel file.
#' @return A dataframe version of the contents of the coded comments
#' in the codedfile.
get_coded_comment_sheet_fmp <- function(codedfile) {
  # Ask for the Coded File if there isn't one provided
  if (missing(codedfile))
    codedfile <- file.choose()

  # Pick out the sheet called "Coded"
  # Warn if no such sheet exists.
  sheetindex <-
    which(tolower(readxl::excel_sheets(codedfile)) == "coded")
  if (length(sheetindex) == 0) {
    warning(paste0(codedfile, " did not have a Coded tab\n"))
    return(NA)
  }

  # Load the Coded Comments as a Data Frame
  coded_orig <- readxl::read_excel(codedfile, sheet = sheetindex)

  # Error if the first column isn't the "ResponseID" column.
  if (names(coded_orig)[[1]] != "ResponseID") {
    stop("The first column of the coded comments is not the ResponseID column.")
  }

  # Strip out the Blank and NA Rows
  blank_rows <- which(is.na(coded_orig[[1]]) | grepl("^\\s*$", coded_orig[[1]]))
  if (length(blank_rows) == 0) {
    coded_use <- coded_orig
  }
  else if (length(blank_rows) > 0) {
    coded_use <- coded_orig[-blank_rows, ]
  }

  # Make sure the Coded Comments have a Varname column
  index_qname <- which(tolower(names(coded_use)) == "varname")
  if (length(index_qname) == 0) {
    cat(paste0(codedfile, " did not have a varname column\n"))
    return(NA)
  }

  # Return the Coded Comments Data Frame (unprocessed)
  return(coded_use)
}

#' Turn a Single NVivo Crosstab Coded File into a Data Frame
#'
#' This retreives comment coding data as a data frame. Data must be
#' formatted with NVivo crosstab export format with ResponseID unlabeled
#' in the first column and second column labeled with the variable name
#' and values indicating response presence. Subsequent columns indicate
#' coded comment categories.
#' @inheritParams get_coded_comment_sheet_fmp
#' @return A dataframe version of the contents of the coded comments
#' in the codedfile.

get_coded_comment_sheet_NVivo <- function(codedfile) {
  # Ask for the Coded File if there isn't one provided
  if (missing(codedfile)) {
    codedfile <- file.choose()
  }

  # Check if there is more than one sheet. If there are multiple
  #   sheets, pick out the sheet called "Coded"
  # Warn if no such sheet exists.
  # Load the Coded Comments data frame
  sheet_count <- length(readxl::excel_sheets(codedfile))
  if (sheet_count == 1) {
    coded_orig <- readxl::read_excel(codedfile, col_types = "text", .name_repair = "minimal")
  } else if (sheet_count >1) {
    sheetindex <- which(tolower(readxl::excel_sheets(codedfile)) == "coded")
    if (length(sheetindex) == 0) {
      warning(paste0(codedfile, "had multiple sheets and did not have a Coded tab\n"))
      return(NA)
    } else {coded_orig <- readxl::read_excel(codedfile, sheet = sheetindex,
                                             col_types = "text", .name_repair = "minimal")}
  }


  #For NVivo exports, the second column of the exported sheet contains the question name
  qname <- names(coded_orig)[[2]]
  #NVivo exports include an unlabeled first column, so we will need to fix this
  #Rename the first column as "ResponseID"
  coded_use <- dplyr::rename(coded_orig, "ResponseID"=1)
  #filter data to keep only values with ResponseID starting with R_ (filter out blanks)
  coded_use <- dplyr::filter(coded_use, stringr::str_detect(ResponseID, "^R_"))
  #Convert all columns other than responseID to integer
  coded_use <- dplyr::mutate_at(coded_use, dplyr::vars(-ResponseID), function(x) as.integer(x))
  #Now filter to keep only rows for respondents who answered the question
  #These are identified with 1 value in the qname column
  #Use the filter to keep anyone with >0, in case we later want multiple comments to tally
  coded_use <- dplyr::filter(coded_use,!!as.name(qname)>0)

  #NVivo crosstab includes the last "Total" column; check for this and remove if it exists
  if (names(coded_use)[[ncol(coded_use)]]=="Total") {
    coded_use <- dplyr::select(coded_use, -Total)
  }

  # Return the Coded Comments Data Frame (unprocessed)
  return(coded_use)
}


#' Process a Dataframe of Coded Comments
#'
#' This turns the original dataframe of coded comments
#' into a pair (varname, coded_table), where the varname
#' is the column name in the response CSV data from Qualtrics
#' that the coded comments correspond to and coded_table
#' summarizes the responses to the coded comments with
#' frequencies for each coded category.
#' @param coded_comment_sheet A single dataframe, imported from a
#' file in the format used in legacy Filemaker Pro coded comment exports.
#' @return A pair (varname, coded_table) where varname corresponds
#' to the corresponding original response column name and coded_table
#' summarizes the frequencies of the provided coded comments.
format_coded_comments_fmp <- function(coded_comment_sheet) {
  # determine which column to start with
  index_qname <-
    which(tolower(names(coded_comment_sheet)) == "varname")

  # get the varname from the sheet
  varname = as.character(coded_comment_sheet[1, index_qname])

  # get coded comments, and the number of comments for each
  codeList <-
    names(coded_comment_sheet)[(index_qname + 2):ncol(coded_comment_sheet)]
  numComments <-
    lapply (codeList, function(x)
      length(which(coded_comment_sheet[x] == 1)))

  # construct the table
  coded_table <-
    as.data.frame(cbind(codeList, numComments, deparse.level = 0))
  names(coded_table) <- c("Response", "N")

  # remove zeroes
  coded_table <- coded_table[coded_table['N'] != 0,]

  # First sort in an ascending alphabetic sort, and then sort descending numerically.
  # The first sort is so that two rows with the same "N" values but with different
  # responses are sorted alphabetically.
  coded_table <- coded_table[order(unlist(coded_table[, "Response"]), decreasing=FALSE), ]
  coded_table <- coded_table[order(unlist(coded_table[, "N"]), decreasing=TRUE), ]

  # add "Total" and the total N to the list of coded comments and Ns
  n_responses <-
    length(unique(as.data.frame(coded_comment_sheet)[, 1]))
  coded_table <- rbind(coded_table, c("Total", n_responses))


  # we return a pair, the varname and the coded table.
  return(list('varname'=varname, 'coded_table'=coded_table))
}

#' Process a Dataframe of Coded Comments
#'
#' This turns the original dataframe of coded comments
#' into a pair (varname, coded_table), where the varname
#' is the column name in the response CSV data from Qualtrics
#' that the coded comments correspond to and coded_table
#' summarizes the responses to the coded comments with
#' frequencies for each coded category.
#' @param coded_comment_sheet A single dataframe, imported from a
#' file formatted by NVivo crosstab data exports.
#' @return A pair (varname, coded_table) where varname corresponds
#' to the corresponding original response column name and coded_table
#' summarizes the frequencies of the provided coded comments.
format_coded_comments_NVivo <- function(coded_comment_sheet) {
  # Identify the variable name; this should be the name of the second column for regular reports,
  #and the third column for split reports; check for SPLIT in the second column
  #within the shiny app this is prepended "split " instead of at the end
  if (stringr::str_detect(names(coded_comment_sheet)[[2]],"-split$|^split ")) {
    varname <- names(coded_comment_sheet)[[3]]
  } else if (!stringr::str_detect(names(coded_comment_sheet)[[2]],"-split$")) {
    varname <-  names(coded_comment_sheet)[[2]]
  }
  #Get the total number of comments
  total_comments <- nrow(coded_comment_sheet)
  #Construct the table
  coded_table <- coded_comment_sheet
  #Gather values to make them long and lean so we can easily tabulate
  coded_table <- tidyr::gather(coded_table, key = "Category", value="codeValue",
                               -ResponseID, -!!varname, -tidyselect::matches("-split|^split "))

  #browser()
  #Filter the long and lean data to keep only positive values showing a mapping to the category
  #This used to be values equal to 1, but we want to be flexible with multi-part questions coded as a single question
  #e.g. What are 3 strengths of the Fletcher School?
  coded_table <- dplyr::filter(coded_table, codeValue>0)
  #Group by Category so we will get the sum of each categories codeValues
  coded_table <- dplyr::group_by(coded_table, Category)
  #Use dplyr function "count" to tabulate the data
  coded_table <- dplyr::summarize(coded_table, n = sum(codeValue))
  coded_table <- dplyr::ungroup(coded_table)
  #Rename columns to match our desired format
  coded_table <- dplyr::rename(coded_table, "Response"=Category,"N" = n)
  #Filter zeros
  coded_table <- dplyr::filter(coded_table, N>0)
  #sort descending numeric with ascending alphabetical
  coded_table <- dplyr::arrange(coded_table, desc(N),Response)
  #add "Total with total number of comments to the bottom of the table
  coded_table <- dplyr::bind_rows(coded_table, tibble::tibble("Response"="Total", "N" = total_comments))

  # we return a pair, the varname and the coded table.
  return(list('varname'=varname, 'coded_table'=coded_table))

}


#' Format Comment Coding Data into Frequency Tables
#'
#' This function takes as an argument a list of dataframes
#' which categorize the text responses to a free-response
#' component of a survey and formats them into a standardized
#' frequency table by applying format_coded_comments to each
#' coded_comment_sheet.
#'
#' @param coded_comment_sheets A list of dataframes for
#' each sheet of coded comments.
#' @param code_type type of coded comment data export. The default is
#' to use NVivo crosstab export with the ResponseID in the first row and
#' second column labeled with the varnmae and containing response presence
#' 1/0 indicator. For old filemaker pro format, use "fmp" specification.
#' (\code{"nvivo"} or \code{"fmp"})
#' @return A list of pairs (varname, coded_table) where varname corresponds
#' to the response column name of the comments coded and coded_table
#' summarizes the frequencies of the provided coded comments.
format_coded_comment_sheets <- function(coded_comment_sheets, code_type) {
  coded_comments <- list()
  cc_length <- length(coded_comment_sheets)
  if (missing(code_type) || ! code_type %in% c("nvivo", "fmp")) {
    stop("The specification of code_type is not fmp or nvivo and is not supported by Qualtricstools.")
    }
  for (i in 1:cc_length) {
    if (code_type == "nvivo") {
      coded_comments[[i]] <-
        format_coded_comments_NVivo(coded_comment_sheets[[i]])
    } else if (code_type == "fmp") {
      coded_comments[[i]] <-
        format_coded_comments_fmp(coded_comment_sheets[[i]])
    }
  }
  return(coded_comments)
}

#' Merge a Splitting Column into an Unprocessed Coded Comment Sheet
#'
#' Run create_merged_response_column() before this to
#' create the splitting column in the responses.
#' Then pass the column name of the column created by
#' create_merged_response_column() as split_column
#' @param coded_comment_seet A single dataframe, imported from a
#' file either NVivo or filemake pro legacy format.
#' @return A pair (varname, coded_table) where varname corresponds
#' to the corresponding original response column name and coded_table
#' summarizes the frequencies of the provided coded comments
#' @param responses A dataframe of Qualtrics responses to a survey.
#' Use ask_for_csv() to create such a dataframe from a CSV file.
#' @param split_column The string name of the column to merge in for splitting
#' @return A dataframe similar to the input coded_comment_sheet, but with
#' the additional column specified by split_column merged in.
merge_split_column_into_comment_sheet <-
  function(coded_comment_sheet,
           responses,
           split_column) {
    # We require in this function that the first column be the ResponseID.
    if (! grepl("ResponseID", colnames(responses)[[1]], ignore.case = TRUE)) {
      stop("The first column of the responses is not the ResponseID.")
    }

    # Which column is the split_column
    responses_split_index <- which(colnames(responses) == split_column)
    if (responses_split_index == 0) {
      # Error if the split_column isn't present
      stop("No column in responses with name ", split_column)
    }
    # Get the response IDs and the split_column into a 2-column data frame
    relevant_columns <- responses[, c(1, responses_split_index)]
    colnames(relevant_columns)[[1]] <- colnames(responses)[[1]]

    coded_comment_sheet <-
      merge(x = coded_comment_sheet, y = relevant_columns, by = 1)
    comments_split_index <-
      which(colnames(coded_comment_sheet) == split_column)
    re_ordering <- c(1, comments_split_index, 2:(comments_split_index - 1))
    coded_comment_sheet <- coded_comment_sheet[, re_ordering]
    return(coded_comment_sheet)
  }

#' Format and Split a list of Unprocessed Coded Comment Sheets
#'
#' When splitting the respondents of a survey to create split reports,
#' the coded comments are split by this function and then returned as a list of lists.
#' The first list is a list for each split group, and each list within those is a
#' list of pairs of question IDs and their coded comments tables.
#'
#' @inheritParams format_coded_comment_sheets
#' @inheritParams merge_split_column_into_comment_sheet
#' @param split_column The string name of the column across which the coded comments
#' should be split.
#' @return A list of lists, for each split group, and for the pairs of question IDs with
#' coded comment tables in each split group.
format_and_split_comment_sheets <-
  function(coded_comment_sheets,
           responses,
           split_column,
           code_type) {
    # split_coded_comment_sheets will be a list of coded comment sheets for each respondent group
   # browser()
    levels <- levels(factor(responses[, split_column]))
    split_coded_comment_sheets <- sapply(levels, function(x)
      NULL)

    # merge split_column in and split each sheet
    for (i in 1:length(coded_comment_sheets)) {
      coded_comment_sheets[[i]] <-
        merge_split_column_into_comment_sheet(coded_comment_sheets[[i]], responses, split_column)
      coded_comment_sheets[[i]] <-
        split(coded_comment_sheets[[i]], coded_comment_sheets[[i]][, split_column], drop =
                TRUE)

      # sort each sheet into the appropriate level and insert into split_coded_comment_sheets
      for (j in 1:length(levels)) {
        for (k in 1:length(coded_comment_sheets[[i]])) {
          if (length(coded_comment_sheets[[i]][[k]][[split_column]] > 0) &&
              all(levels[[j]] %in% coded_comment_sheets[[i]][[k]][[split_column]])) {
            split_coded_comment_sheets[[j]][[length(split_coded_comment_sheets[[j]]) + 1]] <-
              as.data.frame(coded_comment_sheets[[i]][[k]])
          }
        }
        # sheet_contains_level <-
        #   sapply(coded_comment_sheets[[i]], function(x)
        #     isTRUE(levels[[j]] %in% x[, split_column]))
        # if (length(sheet_contains_level) != 0) {
        #   matching_split_sheet <- which(sheet_contains_level)
        #   if (length(matching_split_sheet) != 0) {
        #     split_coded_comment_sheets[[j]][[length(split_coded_comment_sheets[[j]]) + 1]] <-
        #       as.data.frame(coded_comment_sheets[[i]][[matching_split_sheet]])
        #   }
        # }
      }
    }

    # Format each coded comment sheet
    for (i in 1:length(split_coded_comment_sheets)) {
      if (!is.null(split_coded_comment_sheets[[i]]))
        split_coded_comment_sheets[[i]] <-
          format_coded_comment_sheets(coded_comment_sheets = split_coded_comment_sheets[[i]], code_type = code_type)
    }

    return(split_coded_comment_sheets)
  }

#' Insert Coded Comments into Blocks
#'
#' This takes a list of pairs of question IDs and coded comments tables
#' and finds their corresponding question based on the question ID in a list
#' of blocks and then inserts the coded comments table into the question.
#' The returned list is a list of blocks where the questions have had their
#' coded comments inserted.
#'
#' @param blocks A list of the survey blocks, with the questions included in them.
#' @param original_first_rows A dataframe contianing the header information
#' for each column of response data. This dataframe should include a row for the DataExportTag based
#' response column names, another for the Question Text stem and choice text (although
#' truncated), and a row with QID based column names.
#' @param coded_comments A list of pairs (varname, coded_table) where varname corresponds
#' to the response column name of the comments coded and coded_table
#' summarizes the frequencies of the provided coded comments.
insert_coded_comments <-
  function(blocks,
           original_first_rows,
           coded_comments) {
    # Create a response column dictionary, for use in looking up the
    # questions associated to each response column name.
    r_col_dictionary <-
      create_response_column_dictionary(question_blocks = blocks,
                                        original_first_row = original_first_rows[1, ])
    # Copy the questions from the blocks
    questions <- questions_from_blocks(blocks)

    # For each coded comments frequency table, use the response column name
    # stored in coded_comments[[i]][[1]] to match the coded comments table
    # to a specific question.
    for (i in 1:length(coded_comments)) {
      # Check for valid coded comments
      if (!is.null(coded_comments[[i]])) {
        # Get the coded_comments' varname, or associated response column name.
        varname <- coded_comments[[i]][[1]]
        # Find the row of the r_col_dictionary with the matching response
        # column name.
        matched_based_on_r_col <- which(r_col_dictionary[, 2] == varname)

        # If the match is unique, use this to find the Question's Data Export
        # Tag in the first column of the r_col_dictionary. If there are
        # no matches, print a warning that the text appendices could not
        # be matched to a question exactly.
        if (length(matched_based_on_r_col) >= 1) {
          qname <- r_col_dictionary[matched_based_on_r_col[1], 1]
        } else if (length(matched_based_on_r_col) == 0) {

          # Use fuzzy matching with a low tolerance to find similarly named
          # columns and list them in the warning.
          fuzzy_matches <-
            agrep('q3_volunteer', r_col_dictionary[, 2], max.distance = 0.1)
          fuzzy_paste <- paste(r_col_dictionary[fuzzy_matches, 2], collapse = ", ")
          # Construct the warning message
          warning_msg <- c("The appendices with varname ",
                           varname,
                           " could not be matched to a question\n")
          # If fuzzy matching with a low threshold returns results,
          # print the fuzzy matches.
          if (length(fuzzy_matches) > 0)
            warning_msg <- c(warning_msg,
                             "Should the varname have been one of these?\n",
                             fuzzy_paste)
          # Send the warning
          warning(paste0(warning_msg))
          # Continue on to the next coded comments
          next
        }

        # Use the matched question name to find the index of the question.
        question_index <- find_question_index(questions, qname)

        # cc_index is computed as the integer one greater than the number
        # of coded comments already in the question; It is used for inserting
        # the next set of coded comments into a question in a way that ensures
        # that coded comments for other parts of the question, if they exist,
        # are not overwritten.
        cc_index <-
          length(questions[[question_index]][['CodedComments']]) + 1
        questions[[question_index]][['CodedComments']][[cc_index]] <-
          coded_comments[[i]]
      }
    }

    #Now insert questions into blocks
    blocks <- purrr::map(blocks, ~ insert_questions_into_block(block = .x, questions = questions))
    return(blocks)
  }

#' Split Survey and Insert Split Coded Comments
#'
#' This function splits the survey's response data and the
#' coded comments, then inserts the split coded comments into the split
#' surveys.
#'
#' @param split_blocks A list of a list of blocks. The same question, but with different respondent groups,
#'  might look something like split_blocks[[1]][[1]][['BlockElements']][[1]] and
#'  split_blocks[[2]][[1]][['BlockElements']][[1]]. These refer to the first and second respondent
#'  groups, the first block, and the first block element. Split blocks of this form are
#'  generated by the split_respondents function.
#'
#' @param split_coded_comment_sheets A list of lists where the outer lists each split group
#' and the inner lists the pairs of question IDs with coded comment tables in each split group.
#'
#' @inheritParams format_and_split_comment_sheets
#'
#' @param original_first_rows A dataframe contianing the header information
#' for each column of response data. This dataframe should include a row for the DataExportTag based
#' response column names, another for the Question Text stem and choice text (although
#' truncated), and a row with QID based column names.
#'
#' @return The returned data is a list of lists of blocks. Each list of blocks has the coded comments
#' for a specific respondent group inserted into them, and each of the lists of blocks in the
#' output corresponds to a specific respondent group. The list of blocks is duplicated for each
#' respondent group, and then that respondent group's coded comments are inserted into each
#' list of blocks.
insert_split_survey_comments <-
  function(split_blocks,
           split_coded_comment_sheets,
           split_column,
           original_first_rows) {

    # grab the original first rows if not included
    if (missing(original_first_rows))
      original_first_rows <-
        get(x = "original_first_rows", envir = globalenv())

    # match split blocks and split coded comments
    for (i in 1:length(split_coded_comment_sheets)) {
      if (!is.null(split_coded_comment_sheets[[i]])) {
        # The blocks and coded comments are split across the same
        # column (generated by create_merged_response_column), and so
        # we can directly lookup the split blocks which correspond to
        # a specific split_group using the split_group element of the
        # split blocks and the names of each of the split_coded_comment_sheets.
        matching_block <-
          which(sapply(split_blocks, function(x)
            x[['split_group']] == names(split_coded_comment_sheets)[[i]]))
        split_blocks[[matching_block]] <-
          insert_coded_comments(split_blocks[[matching_block]],
                                original_first_rows,
                                split_coded_comment_sheets[[i]])
      }
    }
    return(split_blocks)
  }

