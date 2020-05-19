# Load the package as it is and load tidyverse
devtools::load_all(".")

# Get stuff in from the files from the survey
get_setup(
  qsf_path = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "Long_Exhaustive_Sample_Survey.qsf"),
  csv_path = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "Long_Exhaustive_Sample_Survey.csv"),
  headerrows = 3
)


### QID9
# Find the index of the question
index <- find_question_index_by_qid(questions, 'QID9')

# Get the question we want to test against
question <- questions[[index]]

# Save its table as an RDS file
Q9_results_table <- question$Table
saveRDS(Q9_results_table, file = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "RDS", "Q9_results_table.rds"))



### QID21
# Find the index of the question
index <- find_question_index_by_qid(questions, 'QID21')

# Get the question we want to test against
question <- questions[[index]]

# Save its table as an RDS file
Q21_results_table <- question$Table
saveRDS(Q21_results_table, file = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "RDS", "Q21_results_table.rds"))




### QID13
# Find the index of the question
index <- find_question_index_by_qid(questions, 'QID13')

# Get the question we want to test against
question <- questions[[index]]

# Save its table as an RDS file
q13_family_food_results_table <- question$Table
saveRDS(q13_family_food_results_table, file = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "RDS", "q13_family_food_results_table.rds"))



### QID14
# Find the index of the question
index <- find_question_index_by_qid(questions, 'QID14')

# Get the question we want to test against
question <- questions[[index]]

# Save its table as an RDS file
Q14_results_table <- question$Table
saveRDS(Q14_results_table, file = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "RDS", "Q14_results_table.rds"))




### QID15
# Find the index of the question
index <- find_question_index_by_qid(questions, 'QID15')

# Get the question we want to test against
question <- questions[[index]]

# Save its table as an RDS file
Q15_results_table <- question$Table
saveRDS(Q15_results_table, file = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "RDS", "Q15_results_table.rds"))




### QID16
# Find the index of the question
index <- find_question_index_by_qid(questions, 'QID16')

# Get the question we want to test against
question <- questions[[index]]

# Save its table as an RDS file
Q16_results_table <- question$Table
saveRDS(Q16_results_table, file = here::here("data", "Sample Surveys", "Long Exhaustive Sample Survey", "RDS", "Q16_results_table.rds"))


