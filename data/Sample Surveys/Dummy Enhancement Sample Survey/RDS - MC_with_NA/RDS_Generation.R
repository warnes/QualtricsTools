# Load the package as it is and load tidyverse
devtools::load_all(".")

# Get stuff in from the files from the survey
get_setup(
  qsf_path = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "Dummy_Enhancement_Sample_Survey.qsf"),
  csv_path = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "Dummy_Enhancement_Sample_Survey.csv"),
  headerrows = 3
)

# Get the question we want to test against
question <- questions[[2]]

# Save its table as an RDS file
NA_Matrix_Table <- question$Table
saveRDS(NA_Matrix_Table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "NA_Matrix_Table.rds"))

# Save the original first rows
saveRDS(original_first_rows, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "original_first_rows.rds"))


# Delete the table from the question so the question can be used in the future by a test
question$Table <- NULL

# Save the question without the Table
saveRDS(question, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "NA_Matrix_Question_NO_Table.rds"))





# Lets do this again but with a different question that has only positive recode values

# Get the question we want to test against
question <- questions[[1]]

# Save its table as an RDS file
Reg_MCSA_Table <- question$Table
saveRDS(Reg_MCSA_Table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "Reg_MCSA_Table.rds"))

# Delete the table from the question so the question can be used in the future by a test
question$Table <- NULL

# Save the question without the Table
saveRDS(question, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS - MC_with_NA", "Reg_MCSA_Question_NO_Table.rds"))



