requireNamespace("tidyverse")

#' Turn a Directory into a list of Coded Comment Data Frames (unprocessed)
#'
#' This function takes as an argument a string representative of a
#' directory, loads the CSVs and Excel data from that directory,
#' looks for 'Coded' sheets, extracts the coded sheets,
#' and saves the coded comment table and the question ID
#' as a pair in the output coded_appendix_tables list. If there
#' are sheets which contain non-numeric data, warnings are raised.
#' @param directory A string path to the directory containing the coded comments
#' sheets, which are formatted as described in the Wiki.
#' https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding
#' @return A list of dataframes for each sheet of coded comments.
directory_get_coded_comment_sheets_NVivo <- function(directory) {
  # ask for directory if not provided
  if (missing(directory))
    directory <- choose.dir(caption = "Select coded comment directory")

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
    tryCatch(
      coded_appendix_tables[[length(coded_appendix_tables) + 1]] <-
        get_coded_comment_sheet_NVivo(files_list[[i]]),
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
#' This retrieves comment coding data as a dataframe
#' from a Excel file. Previously with FileMaker exports, the sheet was
#' identified by the sheet name "coded" (case did not matter).
#' With the NVivo exports, there should only be one sheet. We have decided
#' to add a check that the sheet must be called "coded" when there is more than
#' one sheet to choose from.
#' @param codedfile The string path to a Excel file.
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
  coded_use <- coded_orig %>%
    #Rename the first column as "ResponseID"
    dplyr::rename("ResponseID"=1) %>%
    #filter data to keep only values with ResponseID starting with R_ (filter out blanks)
    dplyr::filter(stringr::str_detect(ResponseID, "^R_")) %>%
    #Convert all columns other than responseID to integer
    mutate_at(vars(-ResponseID), funs(as.integer(.))) %>%
    #Now filter to keep only rows for respondents who answered the question
      #These are identified with 1 value in the qname column
      #Use the filter to keep anyone with >0, in case we later want multiple comments to tally
    dplyr::filter(!!as.name(qname)>0)

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
#' file in the format as specified by the wiki.
#' https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding
#' @return A pair (varname, coded_table) where varname corresponds
#' to the corresponding original response column name and coded_table
#' summarizes the frequencies of the provided coded comments.
format_coded_comments_NVivo <- function(coded_comment_sheet) {
  # Identify the variable name; this should be the name of the second column for regular reports,
    #and the third column for split reports; check for SPLIT in the second column
  if (stringr::str_detect(names(coded_comment_sheet)[[2]],"-split$")) {
    varname <- names(coded_comment_sheet)[[3]]
  } else if (!stringr::str_detect(names(coded_comment_sheet)[[2]],"-split$")) {
    varname <-  names(coded_comment_sheet)[[2]]
  }
  #Get the total number of comments
  total_comments <- nrow(coded_comment_sheet)

  # get coded comments, and the number of comments for each
  codeList <-
    names(coded_comment_sheet)[3:ncol(coded_comment_sheet)]
  numComments <-
    lapply (codeList, function(x)
      length(which(coded_comment_sheet[x] == 1)))
  #Construct the table
  coded_table <- coded_comment_sheet %>%
    #Gather values to make them long and lean so we can easily tabulate
    tidyr::gather(key = "Category", value="codeFlag", -ResponseID, -!!varname, -ends_with("-split")) %>%
    #Filter the long and lean data to keep only values with "1" showing a mapping to the category
    dplyr::filter(codeFlag==1) %>%
    #Use dplyr function "count" to tabulate the data
    dplyr::count(Category) %>%
    #Rename columns to match our desired format
    dplyr::rename("Response"=Category,"N" = n) %>%
    #Filter zeros
    dplyr::filter(N>0) %>%
    #sort descending numeric with ascending alphabetical
    dplyr::arrange(desc(N),Response) %>%
    #add "Total with total number of comments to the bottom of the table
    dplyr::bind_rows(tibble("Response"="Total", "N" = total_comments))

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
#' @return A list of pairs (varname, coded_table) where varname corresponds
#' to the response column name of the comments coded and coded_table
#' summarizes the frequencies of the provided coded comments.
format_coded_comment_sheets_NVivo <- function(coded_comment_sheets) {
  coded_comments <- list()
  cc_length <- length(coded_comment_sheets)
  for (i in 1:cc_length) {
    coded_comments[[i]] <-
      format_coded_comments_NVivo(coded_comment_sheets[[i]])
  }
  return(coded_comments)
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
format_and_split_comment_sheets_NVivo <-
  function(coded_comment_sheets,
           responses,
           split_column) {
    # split_coded_comment_sheets will be a list of coded comment sheets for each respondent group
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
        sheet_contains_level <-
          sapply(coded_comment_sheets[[i]], function(x)
            isTRUE(levels[[j]] %in% x[, split_column]))
        if (length(sheet_contains_level) != 0) {
          matching_split_sheet <- which(sheet_contains_level)
          if (length(matching_split_sheet) != 0) {
            split_coded_comment_sheets[[j]][[length(split_coded_comment_sheets[[j]]) + 1]] <-
              as.data.frame(coded_comment_sheets[[i]][[matching_split_sheet]])
          }
        }
      }
    }

    # Format each coded comment sheet
    for (i in 1:length(split_coded_comment_sheets)) {
      if (!is.null(split_coded_comment_sheets[[i]]))
        split_coded_comment_sheets[[i]] <-
          format_coded_comment_sheets_NVivo(split_coded_comment_sheets[[i]])
    }

    return(split_coded_comment_sheets)
  }


#' Create Text Appendices including Coded Comments
#'
#' This was taken from helper_functions.R during development of coded comments NVivo update
#' Using `get_setup`, `directory_get_coded_comment_sheets`, `format_coded_comment_sheets`,
#' `insert_coded_comments`, and `html_2_pandoc`, this function renders
#' text appendices with coded comments included from CSV or XLSX files
#' from the specified `sheets_dir` parameter.
#'
#' @inheritParams make_results_tables
#' @param sheets_dir is the string path location of the directory which contains Excel documents
#' with a "Coded" sheet formatted as specified on the wiki:
#' https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding
#' @param n_threshold is the number of verbatim comments which must appear before an appendix of
#' coded comments will be included.
make_coded_comments_NVivo <-
  function(qsf_path,
           csv_path,
           headerrows,
           sheets_dir,
           output_dir,
           filename = 'Text Appendices with Coded Comments.docx',
           n_threshold = 15
  ) {
    # Either use the passed parameters or interactively get setup with the survey data.
    get_setup_in_environment(
      qsf_path = qsf_path,
      csv_path = csv_path,
      headerrows = headerrows,
      environment = environment()
    )

    coded_sheets <- directory_get_coded_comment_sheets_NVivo(sheets_dir)

    if (is.null(coded_sheets)) {
      stop("Please fix errors before attempting again")
    }

    comment_tables <-
      format_coded_comment_sheets_NVivo(coded_comment_sheets = coded_sheets)
    blocks <-
      insert_coded_comments(
        blocks = blocks,
        original_first_rows = original_first_rows,
        coded_comments = comment_tables
      )

    # Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
    flow = flow_from_survey(survey)

    html_2_pandoc(
      html = c(
        blocks_header_to_html(blocks),
        text_appendices_table(
          blocks = blocks,
          original_first_row = original_first_rows,
          flow = flow,
          n_threshold = n_threshold
        )
      ),
      file_name = filename,
      output_dir = output_dir
    )
  }



#' Split a Survey's Split Coded Comment Appendices
#'
#' This question automates the entire process of splitting a
#' survey's text appendices by specific response columns. The QSF
#' and CSV file are passed as string arguments,
#' the sheets_dir specifies where the coded comments excel or csv
#' data is stored, and the output_dir specifies where the split
#' coded comment appendices should be saved. The n_threshold
#' specifies how many coded comments there must be before the coded
#' comment appendices are included, and headerrows is an argument
#' necessary to process the survey results correctly.
#' @inheritParams make_coded_comments
#' @inheritParams make_split_results_tables
make_split_coded_comments_NVivo <-
  function(qsf_path,
           csv_path,
           sheets_dir,
           output_dir,
           split_by,
           n_threshold = 15,
           headerrows) {
    # This turns the split_by list into a name for the column
    # which will contain the concatenation of the entries of responses
    # which are being split over. That is if split_by = c('column1', 'column2', 'column3'),
    # then this constructs split_string = 'column1-column2-column3'
    split_string <- c(split_by, "split")
    split_string <- toString(paste(split_string, "-"))
    split_string <- gsub(' ', '', split_string)
    split_string <- gsub(',', '', split_string)
    split_string <- substr(split_string, 1, nchar(split_string) - 1)

    # Either use the passed parameters or interactively get setup with the survey data.
    get_setup_in_environment(
      qsf_path = qsf_path,
      csv_path = csv_path,
      headerrows = headerrows,
      environment = environment()
    )

    # Merges the selected columns into one name
    # In this case School, DegType, and Porgram merged into school-degtype-program
    responses <-
      create_merged_response_column(split_by, split_string, blocks, responses)

    coded_sheets <- directory_get_coded_comment_sheets_NVivo(sheets_dir)

    if (is.null(coded_sheets)) {
      stop("Please fix errors before attempting again")
    }

    split_comment_tables <-
      format_and_split_comment_sheets_NVivo(coded_comment_sheets = coded_sheets,
                                            responses = responses,
                                            split_column = split_string)

    split_blocks <-
      split_respondents(
        response_column = split_string,
        responses = responses,
        survey = survey,
        blocks = blocks,
        questions = questions,
        headerrows = headerrows,
        already_loaded = FALSE,
        original_first_rows
      )

    split_blocks <-
      insert_split_survey_comments(split_blocks = split_blocks,
                                   split_coded_comment_sheets = split_comment_tables,
                                   split_column = split_string,
                                   original_first_rows = original_first_rows)

    #Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
    flow = flow_from_survey(survey)

    #Appends .docx to the file names collected by splitting the data to output them as Word Documents
    filenames <- sapply(split_blocks, function(x)
      x$split_group)
    filenames <- sapply(filenames, function(x)
      paste0(x, '.docx'))

    #Outputs the data to word documents using html_2_pandoc
    return_list <- c()
    for (i in 1:length(filenames)) {
      outpath <- html_2_pandoc(
        html = c(
          blocks_header_to_html(split_blocks[[i]]),
          text_appendices_table(
            blocks = split_blocks[[i]],
            original_first_row = original_first_rows,
            flow = flow,
            n_threshold = n_threshold
          )
        ),
        file_name = filenames[[i]],
        output_dir = output_dir
      )
      return_list <- c(return_list, outpath)
    }
    return(return_list)
  }



