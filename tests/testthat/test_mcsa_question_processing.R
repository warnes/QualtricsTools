
# Test mc_single_answer_results
context("Testing mc_single_answer_results")

# Better Sample Survey: q2_favorite
test_that(
  "Test that mc_single_answer_results is correct for q2_favorite in the Better Sample Survey",
  {
    # Load the original first rows from the Better Sample Survey
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'original_first_rows.rds'
      )
    )

    # Load the question, without the results tables, for processing.
    q2_favorite <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'q2_favorite.rds'
      )
    )

    # Process the question and insert results tables into it.
    q2_favorite <-
      process_question_results(q2_favorite, original_first_rows)

    # Load the previously computed results table.
    q2_favorite_results_table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Better Sample Survey/RDS',
        'q2_favorite_results_table.rds'
      )
    )
    q2_favorite_results_table <- as.data.frame(q2_favorite_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(q2_favorite[['Table']][["N"]] == q2_favorite_results_table[["N"]]) &&
                  all(q2_favorite[['Table']][['Percent']] == q2_favorite_results_table[['Percent']]))

  }
)

# Load the original_first_rows from the Long Exhaustive Sample Survey.
# This is needed for the next several tests that use questions from the
# Long Exhaustive Sample Survey.
original_first_rows <- readRDS(
  file.path(
    find.package('QualtricsTools'),
    'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
    'original_first_rows.rds'
  )
)

# Long Exhaustive Sample Survey: Q2
test_that(
  "Test that mc_single_answer_results is correct for Q2 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q2 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q2.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q2 <- process_question_results(Q2, original_first_rows)

    # Load the previously computed results table.
    Q2_results_table <-  readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q2_results_table.rds'
      )
    )
    Q2_results_table <- as.data.frame(Q2_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q2[['Table']][['N']] == Q2_results_table[['N']]) &&
                all(Q2[['Table']][['Percent']] == Q2_results_table[['Percent']]))
  }
)


# Long Exhaustive Sample Survey: Q10
test_that(
  "Test that mc_single_answer_results is correct for Q10 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q10 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q10.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q10 <- process_question_results(Q10, original_first_rows)

    # Load the previously computed results table.
    Q10_results_table <-
      readRDS(
        file.path(
          find.package('QualtricsTools'),
          'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
          'Q10_results_table.rds'
        )
      )
    Q10_results_table <- as.data.frame(Q10_results_table)

    # Check that the previously computed results and new results match.
    expect_true(all(Q10[['Table']][['N']] == Q10_results_table[['N']]) &&
                  all(Q10[['Table']][['Percent']] == Q10_results_table[['Percent']]))

  }
)


# Long Exhaustive Sample Survey: Q11
test_that(
  "Test that mc_single_answer_results is correct for Q11 in the Long Exhaustive Sample Survey",
  {
    # Load the question, without the results tables, for processing.
    Q11 <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q11.rds'
      )
    )

    # Process the question and insert results tables into it.
    Q11 <- process_question_results(Q11, original_first_rows)

    # Load the previously computed results table.
    Q11_results_table <-
      readRDS(
        file.path(
          find.package('QualtricsTools'),
          'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
          'Q11_results_table.rds'
        )
      )

    # Check that the previously computed results and new results match.
    expect_true(all(Q11[['Table']][['N']] == Q11_results_table[['N']]) &&
                  all(Q11[['Table']][['Percent']] == Q11_results_table[['Percent']]))
  }
)


# Long Exhaustive Sample Survey: Q3
test_that("Test that mc_single_answer_results is correct for Q3 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q3 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q3.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q3 <- process_question_results(Q3, original_first_rows)

  # Load the previously computed results table.
  Q3_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q3_results_table.rds'
      )
    )
  Q3_results_table <- as.data.frame(Q3_results_table)

  # Check that the previously computed results and new results match.
  expect_true(all(Q3[['Table']][['N']] == Q3_results_table[['N']]) &&
                all(Q3[['Table']][['Percent']] == Q3_results_table[['Percent']]))
})


# Long Exhaustive Sample Survey: q4_colored_fruit
test_that("Test that mc_single_answer_results is correct for q4_colored_fruit in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  q4_colored_fruit <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'q4_colored_fruit.rds'
    )
  )

  # Process the question and insert results tables into it.
  q4_colored_fruit <- process_question_results(q4_colored_fruit, original_first_rows)

  # Load the previously computed results table.
  q4_colored_fruit_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'q4_colored_fruit_results_table.rds'
      )
    )
  q4_colored_fruit_results_table <- as.data.frame(q4_colored_fruit_results_table)

  # print.data.frame(q4_colored_fruit_results_table)
  # print.data.frame(q4_colored_fruit[['Table']])
  # print(str(q4_colored_fruit_results_table))
  # print(str(as.data.frame(q4_colored_fruit[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(q4_colored_fruit[['Table']][['N']] == q4_colored_fruit_results_table[['N']]) &&
                all(q4_colored_fruit[['Table']][['Percent']] == q4_colored_fruit_results_table[['Percent']]) &&
                all(
                  names(q4_colored_fruit[['Table']]) == names(q4_colored_fruit_results_table)
                ))
})


# Long Exhaustive Sample Survey: Q5
test_that("Test that mc_single_answer_results is correct for Q5 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q5 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q5.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q5 <- process_question_results(Q5, original_first_rows)

  # Load the previously computed results table.
  Q5_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q5_results_table.rds'
      )
    )
  Q5_results_table <- as.data.frame(Q5_results_table)

  # print.data.frame(Q5_results_table)
  # print.data.frame(Q5[['Table']])
  # print(str(Q5_results_table))
  # print(str(as.data.frame(Q5[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(Q5[['Table']][['N']] == Q5_results_table[["N"]]) &&
                all(Q5[['Table']][['Percent']] == Q5_results_table[["Percent"]]) &&
                all(names(Q5[['Table']]) == names(Q5_results_table)))
})


# Long Exhaustive Sample Survey: Q6
test_that("Test that mc_single_answer_results is correct for Q6 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q6 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q6.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q6 <- process_question_results(Q6, original_first_rows)

  # Load the previously computed results table.
  Q6_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q6_results_table.rds'
      )
    )
  Q6_results_table <- as.data.frame(Q6_results_table)

  # print.data.frame(Q6_results_table)
  # print.data.frame(Q6[['Table']])
  # print(str(Q6_results_table))
  # print(str(as.data.frame(Q6[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(Q6[['Table']][['N']] == Q6_results_table[['N']]) &&
                all(Q6[['Table']][['Percent']] == Q6_results_table[['Percent']]) &&
                all(names(Q6[['Table']]) == names(Q6_results_table)))
})


# Long Exhaustive Sample Survey: Q7
test_that("Test that mc_single_answer_results is correct for Q7 in the Long Exhaustive Sample Survey", {
  # Load the question, without the results tables, for processing.
  Q7 <- readRDS(
    file.path(
      find.package('QualtricsTools'),
      'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
      'Q7.rds'
    )
  )

  # Process the question and insert results tables into it.
  Q7 <- process_question_results(Q7, original_first_rows)

  # Load the previously computed results table.
  Q7_results_table <-
    readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Long Exhaustive Sample Survey/RDS',
        'Q7_results_table.rds'
      )
    )
  Q7_results_table <- as.data.frame(Q7_results_table)

  # print.data.frame(Q7_results_table)
  # print.data.frame(Q7[['Table']])
  # print(str(Q7_results_table))
  # print(str(as.data.frame(Q7[['Table']], stringsAsFactors=FALSE)))
  # print(options()[['stringsAsFactors']])

  # Check that the previously computed results and new results match.
  expect_true(all(Q7[['Table']][['N']] == Q7_results_table[['N']]) &&
                all(Q7[['Table']][['Percent']] == Q7_results_table[['Percent']]) &&
                all(names(Q7[['Table']]) == names(Q7_results_table)))
})



# Dummy Enhancement Sample Survey: NA_Matrix_Question
test_that(
  "Test that process_question_results is correct for NA_Matrix_Question in the Dummy Enhancement Sample Survey",
  {
    # Load the original first rows from the Dummy Enhancement Sample Survey
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS - MC_with_NA',
        'original_first_rows.rds'
      )
    )

    # Load the question, without the results tables, for processing.
    NA_Matrix_Question_NO_Table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS - MC_with_NA',
        'NA_Matrix_Question_NO_Table.rds'
      )
    )

    # Process the question and insert results tables into it.
    NA_Matrix_Question_NO_Table <-
      process_question_results(NA_Matrix_Question_NO_Table, original_first_rows)

    # Load the previously computed results table.
    NA_Matrix_Table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS - MC_with_NA',
        'NA_Matrix_Table.rds'
      )
    )
    NA_Matrix_Table <- as.data.frame(NA_Matrix_Table)

    # Check that the previously computed results and new results match.
    expect_true(all(NA_Matrix_Question_NO_Table[['Table']][["N"]] == NA_Matrix_Table[["N"]]) &&
                  all(NA_Matrix_Question_NO_Table[['Table']][['Percent']] == NA_Matrix_Table[['Percent']]))

  }
)



# Dummy Enhancement Sample Survey: Reg_MCSA_Question
test_that(
  "Test that process_question_results is correct for Reg_MCSA_Question in the Dummy Enhancement Sample Survey",
  {
    # Load the original first rows from the Dummy Enhancement Sample Survey
    original_first_rows <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS - MC_with_NA',
        'original_first_rows.rds'
      )
    )

    # Load the question, without the results tables, for processing.
    Reg_MCSA_Question_NO_Table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS - MC_with_NA',
        'Reg_MCSA_Question_NO_Table.rds'
      )
    )

    # Process the question and insert results tables into it.
    Reg_MCSA_Question_NO_Table <-
      process_question_results(Reg_MCSA_Question_NO_Table, original_first_rows)

    # Load the previously computed results table.
    Reg_MCSA_Table <- readRDS(
      file.path(
        find.package('QualtricsTools'),
        'data/Sample Surveys/Dummy Enhancement Sample Survey/RDS - MC_with_NA',
        'Reg_MCSA_Table.rds'
      )
    )
    Reg_MCSA_Table <- as.data.frame(Reg_MCSA_Table)

    # Check that the previously computed results and new results match.
    expect_true(all(Reg_MCSA_Question_NO_Table[['Table']][["N"]] == Reg_MCSA_Table[["N"]]) &&
                  all(Reg_MCSA_Question_NO_Table[['Table']][['Percent']] == Reg_MCSA_Table[['Percent']]))

  }
)


