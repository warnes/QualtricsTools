# Load the package as it is and load tidyverse
devtools::load_all(".")

# Get stuff in from the files from the survey
get_setup(
  qsf_path = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "Dummy_Enhancement_Sample_Survey.qsf"),
  csv_path = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "Dummy_Enhancement_Sample_Survey.csv"),
  headerrows = 3
)

# Get the question we want to test against
question <- questions[[10]]

# Save its table as an RDS file
No_recode_Table <- question$Table
saveRDS(No_recode_Table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MCMA_Sort", "No_recode_Table.rds"))

# Save the original first rows
saveRDS(original_first_rows, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MCMA_Sort", "original_first_rows.rds"))


# Delete the table from the question so the question can be used in the future by a test
question$Table <- NULL

# Save the question without the Table
saveRDS(question, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "No_recode_NO_Table.rds"))




# Get the question we want to test against
question <- questions[[8]]

# Save its table as an RDS file
Recode_900_Table <- question$Table
saveRDS(Recode_900_Table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MCMA_Sort", "Recode_900_Table.rds"))

# Delete the table from the question so the question can be used in the future by a test
question$Table <- NULL

# Save the question without the Table
saveRDS(question, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "Recode_900_NO_Table.rds"))


# Sort by Choice_Alpha then save that table
question <- mc_multiple_answer_results(question, original_first_rows, sort_by = "Choices_Alpha")
Recode_Choice_Table <- question$Table
saveRDS(Recode_Choice_Table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MCMA_Sort", "Recode_Choice_Table.rds"))


# Sort by Choice Order then save that table
question <- mc_multiple_answer_results(question, original_first_rows, sort_by = "Choice_Order")
Recode_Order_Table <- question$Table
saveRDS(Recode_Order_Table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MCMA_Sort", "Recode_Order_Table.rds"))
