# Test TE_numerical_validation results
context("Testing text_entry_numerical_results")

# Load the original_first_rows from the Dummy Enhancement Sample Survey.
# This is needed for the next several tests that use questions from the
# Dummy Enhancement Sample Survey.

original_first_rows <- readRDS(
  here::here('data/Sample Surveys/Dummy Enhancement Sample Survey/RDS',
    'original_first_rows.rds')
)

# Dummy Enchancement Sample Survey: Q4
test_that(
  "Test that text_entry_numerical_results table correct for te_numeric_good in the Dummy Enhancement Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    te_numeric_good <- readRDS(
      here::here('data/Sample Surveys/Dummy Enhancement Sample Survey/RDS',
        'te_numeric_good_notable.rds')
    )

    # Process the question and insert results tables into it.
    te_numeric_good <- process_question_results(te_numeric_good, original_first_rows)

    # Load the previously computed results table.
    te_numeric_good_table <-  readRDS(
      here::here('data/Sample Surveys/Dummy Enhancement Sample Survey/RDS',
        'te_numeric_good_table.rds'
      )
    )

    # Check that the previously computed results and new results match.
    expect_true(all(te_numeric_good[['Table']] == te_numeric_good_table) &&
                  all(names(te_numeric_good[['Table']]) == names(te_numeric_good_table)))
  }
)

# Dummy Enchancement Sample Survey: Q11
test_that(
  "Test that results are not processed for numeric text entry question with text responses",
  {
    # Load the question, without the results tables, for processing.
    te_numeric_fail <- readRDS(
      here::here('data/Sample Surveys/Dummy Enhancement Sample Survey/RDS',
        'te_numeric_fail.rds'
      )
    )

    # Process the question and insert results tables into it.
    te_numeric_test <- process_question_results(te_numeric_fail, original_first_rows)

    #Check that there are data that cannot be converted to numeric
    te_numeric_test_responses <- as.character(te_numeric_fail[["Responses"]][['te_numeric_fail']])
    te_numeric_test_responses <- suppressWarnings(as.numeric(te_numeric_test_responses))

    # No tables should be generated and if this true, the test passes.
    expect_true(!("Table" %in% names(te_numeric_test)) & any(is.na(te_numeric_test_responses)))
  }
)

