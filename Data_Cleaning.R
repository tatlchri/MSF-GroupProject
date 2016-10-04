# Data cleaning
# -------------

# Open file
survey_data <- read.csv("data.csv", header = TRUE, sep = ",", na.strings = "NA")
# dim(survey_data) would indicate 1642 rows

# remove rows where caseID is not a valid number
# first convert caseID from Factor into a character vector, then into integer. Invalid entries will turn into NA
case_id <- as.integer(as.character(survey_data$ï..CaseID))
survey_data <- survey_data[!is.na(case_id), ]

# dim(survey_data) would indicate 1638 rows
#   ---> 4 rows removed. Checked with csv manually. Removed rows matches with rows with string instead of integer

# remove rows where parent answered 0 or empty response to QS1
# QS1: How many teenagers aged 13 through 17 live in your household at least 50% of the time?  [IF QS1=0, INSERT STANDARD CLOSE AND TERMINATE]
survey_data <- survey_data[!is.na(survey_data$QS1), ]
survey_data <- survey_data[survey_data$QS1 > 0, ]
# 1109 rows left ---> 529 additional rows removed
# did a counter-check with Excel to ensure that it's correct
#  rows with non valid QS1 responses have all the other columns empty

# remove rows where parent answered 0 or empty response to QS1
# QS2: For how many of the teenagers aged 13 to 17 in your household are you the parent or legal guardian?   [IF QS2=0, INSERT STANDARD CLOSE AND TERMINATE]
survey_data <- survey_data[!is.na(survey_data$QS2), ]
survey_data <- survey_data[survey_data$QS2 > 0, ]
# 1093 rows left ---> 16 additional rows removed
# did a counter-check with Excel to ensure that it's correct
#  rows with non valid QS2 responses have all the other columns empty

# remove rows where teen's age is not between 13 and 17.
survey_data <- survey_data[!is.na(survey_data$Child_age), ]
survey_data <- survey_data[(survey_data$Child_age >= 13 & survey_data$Child_age <= 17), ]
# 1081 rows left ---> 12 additional rows removed
# did a counter-check with Excel to ensure that it's correct

save(survey_data, file = 'cleaned_data.Rdata')

# TO USE:
# type load('cleaned_data.Rdata') at the start of your code..
# But with a merged Rmd file, this is not needed