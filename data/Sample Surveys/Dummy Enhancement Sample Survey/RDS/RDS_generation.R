##RDS Generation for the Dummy Enhancement Summary Surve
#Right now  this only contains information for the Numeric Text Entry Processing test, but it may be expanded to include other tests

#Created by Emma Morgan (emma-morgan)
#Updated 8/11/2020

#Load the QualtricsTools package
devtools::load_all(".")

#Get setup with the dummy enhancement sample survey
get_setup(
  qsf_path = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "Dummy_Enhancement_Sample_Survey.qsf"),
  csv_path = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "Dummy_Enhancement_Sample_Survey.csv"),
  headerrows = 3
)

#Save the original_first_rows.rds
saveRDS(original_first_rows, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS", "original_first_rows.rds"))

#te_numeric_good----
#Save the table for te_numeric_good; this can be compared with how we process later
te_numeric_good_table <- questions[['te_numeric_good']][['Table']]
saveRDS(te_numeric_good_table, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS", "te_numeric_good_table.rds"))

#Save the question WITHOUT a table; this can be read in for processing with the test
te_numeric_good_notable <- questions[['te_numeric_good']]
te_numeric_good_notable[['Table']] <- NULL
saveRDS(te_numeric_good_notable, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS", "te_numeric_good_notable.rds"))

#te_numeric_fail ----
#Save the question for te_numeric_fail so we can test whether or not it processes
te_numeric_fail <- questions[['te_numeric_fail']]
saveRDS(te_numeric_fail, file = here::here("data", "Sample Surveys", "Dummy Enhancement Sample Survey", "RDS", "te_numeric_fail.rds"))
