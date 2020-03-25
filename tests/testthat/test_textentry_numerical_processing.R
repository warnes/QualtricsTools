# Test TE_numerical_validation results
context("Testing text_entry_numerical_results")

# Load the original_first_rows from the Dummy Enhancement Sample Survey.
# This is needed for the next several tests that use questions from the
# Long Exhaustive Sample Survey.
original_first_rows <- readRDS(
  file.path(
    find.package('QualtricsTools'),
    'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS',
    'original_first_rows.rds'
  )
)

# Dummy Enchancement Sample Survey: Q4
test_that(
  "Test that text_entry_numerical_results is correct for Q4 in the Dummy Enhancement Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q4 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS',
        'Q4.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q4 <- process_question_results(Q4, original_first_rows)

    # Load the previously computed results table.
    Q29_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q29_results_table.rds'
      )
    )
    Q4_results_table <- as.data.frame(Q4_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q4[['Table']] == Q4_results_table) &&
                  all(names(Q4[['Table']]) == names(Q4_results_table)))
  }
)

